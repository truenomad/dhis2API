# a tracker server of a given version holding `total` fake events
tracker_server <- function(version, total = 5, key = "events") {
  function(path, query) {
    if (path == "system/info.json") {
      return(json_resp(list(version = version)))
    }
    page <- as.integer(query$page)
    size <- as.integer(query$pageSize)
    first <- (page - 1) * size + 1
    ids <- if (first > total) integer() else seq(first, min(total, page * size))
    records <- purrr::map(ids, function(i) {
      list(
        event = paste0("EV_", i),
        dataValues = list(list(dataElement = "DE_1", value = i))
      )
    })
    body <- list(pager = list(page = page, pageSize = size))
    body[[key]] <- records
    json_resp(body)
  }
}

test_that("events page until a short page", {
  log <- local_fake_server(tracker_server("2.43.2"))
  quiet_cli()
  out <- dhis2_tracker(
    "events", program = "PR_1", org_unit = "OU_C", page_size = 2,
    session = fake_session()
  )
  expect_equal(out$event, paste0("EV_", 1:5))
  # one version read, then three pages
  expect_length(log$calls, 4)
})

test_that("org unit parameters follow the resource and the version", {
  expect_equal(
    .dhis2_tracker_ou_params("events", numeric_version("2.43")),
    list(unit = "orgUnit", mode = "orgUnitMode")
  )
  expect_equal(
    .dhis2_tracker_ou_params("enrollments", numeric_version("2.42")),
    list(unit = "orgUnits", mode = "orgUnitMode")
  )
  expect_equal(
    .dhis2_tracker_ou_params("trackedEntities", numeric_version("2.40")),
    list(unit = "orgUnit", mode = "ouMode")
  )

  log <- local_fake_server(tracker_server("2.43.2", total = 1))
  quiet_cli()
  dhis2_tracker(
    "events", program = "PR_1", org_unit = c("OU_A", "OU_B"),
    session = fake_session()
  )
  q <- log$calls[[2]]$query
  expect_equal(q$orgUnit, "OU_A;OU_B")
  expect_equal(q$orgUnitMode, "DESCENDANTS")
  expect_null(q$ouMode)
})

test_that("older servers return records under instances", {
  local_fake_server(tracker_server("2.39.1", total = 3, key = "instances"))
  quiet_cli()
  out <- dhis2_tracker("events", program = "PR_1", session = fake_session())
  expect_equal(nrow(out), 3)
})

test_that("servers before the tracker API are refused", {
  local_fake_server(tracker_server("2.35.0"))
  expect_error(
    dhis2_tracker("events", session = fake_session()), "needs DHIS2 2.36"
  )
})

test_that("relationships are not read by org unit", {
  expect_error(
    dhis2_tracker("relationships", org_unit = "OU_C",
                  session = fake_session()),
    "not by org unit"
  )
})

test_that("event values come out long", {
  local_fake_server(tracker_server("2.43.2", total = 2))
  quiet_cli()
  events <- dhis2_tracker("events", session = fake_session())
  out <- dhis2_event_values(events)
  expect_equal(names(out), c("event", "data_element", "value"))
  expect_equal(out$value, c("1", "2"))
  expect_error(dhis2_event_values(tibble::tibble(x = 1)), "dataValues")
})

test_that("events without data values give zero typed rows", {
  empty_cols <- c("event", "data_element", "value")
  all_empty <- tibble::tibble(
    event = c("EV_1", "EV_2"), dataValues = list(list(), list())
  )
  out <- dhis2_event_values(all_empty)
  expect_equal(names(out), empty_cols)
  expect_equal(nrow(out), 0)
  expect_type(out$value, "character")

  mixed <- tibble::tibble(
    event = c("EV_1", "EV_2"),
    dataValues = list(
      list(), data.frame(dataElement = "DE_1", value = 4)
    )
  )
  out <- dhis2_event_values(mixed)
  expect_equal(out$event, "EV_2")
  expect_equal(out$value, "4")

  local_fake_server(tracker_server("2.43.2", total = 0))
  quiet_cli()
  none <- dhis2_tracker("events", session = fake_session())
  expect_equal(nrow(none), 0)
  expect_equal(names(dhis2_event_values(none)), empty_cols)

  bad <- tibble::tibble(
    event = "EV_1", dataValues = list(data.frame(other = 1))
  )
  expect_error(dhis2_event_values(bad), "dataElement")
})
