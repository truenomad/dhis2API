fake_units <- c("OU_D1", "OU_D2", "OU_D3")

values_server <- function(fail = character()) {
  function(path, query) {
    if (query$orgUnit %in% fail) {
      return(json_resp(list(message = "fake failure"), status = 500L))
    }
    csv_resp(fake_values_csv(query$orgUnit, n = 2))
  }
}

test_that("unit downloads are saved once and reused on resumption", {
  quiet_cli()
  log <- local_fake_server(values_server())
  dir <- withr::local_tempdir()
  args <- list(
    start_date = "2001-01-01", end_date = "2001-12-31",
    data_element_group = "DEG_1", workers = 1, progress = FALSE,
    session = fake_session()
  )
  got <- rlang::exec(dhis2_fetch_units, fake_units, dir, !!!args)
  expect_equal(unname(got), c(2L, 2L, 2L))
  expect_setequal(.dhis2_parked_units(dir), fake_units)

  again <- rlang::exec(dhis2_fetch_units, fake_units, dir, !!!args)
  expect_equal(unname(again), c(0L, 0L, 0L))
  expect_length(log$calls, 3)
})

test_that("a unit that fails twice comes back as NA", {
  quiet_cli()
  log <- local_fake_server(values_server(fail = "OU_D2"))
  got <- dhis2_fetch_units(
    fake_units, withr::local_tempdir(),
    start_date = "2001-01-01", end_date = "2001-12-31",
    data_element_group = "DEG_1", workers = 1, progress = FALSE,
    session = fake_session()
  )
  expect_true(is.na(got[["OU_D2"]]))
  expect_equal(got[["OU_D1"]], 2L)
  expect_length(log$calls, 4)
})

test_that("a group downloads each year in full, then downloads changes", {
  quiet_cli()
  log <- local_fake_server(values_server())
  path <- fs::path(withr::local_tempdir(), "fake.duckdb")
  con <- dhis2_store_open(path)
  withr::defer(dhis2_store_close(con))
  interim <- withr::local_tempdir()
  sync <- function() {
    dhis2_sync_group(
      con, "fake_group", fake_units, interim,
      window_start = "2000-06-01", window_end = "2001-12-31",
      data_element_group = "DEG_1", workers = 1, session = fake_session()
    )
  }

  expect_equal(dhis2_plan_run(con, fake_units, "2000-06-01",
                              "fake_group")$mode, "backfill")
  sync()
  # three units in two years
  expect_length(log$calls, 6)
  expect_null(log$calls[[1]]$query$lastUpdatedDuration)
  expect_equal(dhis2_plan_run(con, fake_units, "2000-06-01",
                              "fake_group")$mode, "delta")

  sync()
  delta_calls <- log$calls[7:9]
  expect_equal(
    purrr::map_chr(delta_calls, \(x) x$query$lastUpdatedDuration),
    rep("8d", 3)
  )
  expect_equal(delta_calls[[1]]$query$startDate, "2000-06-01")
  expect_equal(dhis2_delta_window(con, "fake_group"), 8L)
})

test_that("a unit that fails is not logged and is fetched next time", {
  quiet_cli()
  local_fake_server(values_server(fail = "OU_D3"))
  path <- fs::path(withr::local_tempdir(), "fake.duckdb")
  con <- dhis2_store_open(path)
  withr::defer(dhis2_store_close(con))
  dhis2_sync_group(
    con, "fake_group", fake_units, withr::local_tempdir(),
    window_start = "2001-01-01", window_end = "2001-12-31",
    data_element_group = "DEG_1", workers = 1, session = fake_session()
  )
  plan <- dhis2_plan_run(con, fake_units, "2001-01-01", "fake_group")
  expect_equal(plan$mode, "backfill")
  expect_equal(plan$todo, "OU_D3")
})

test_that("updates include changes since the least recently updated unit", {
  path <- fs::path(withr::local_tempdir(), "fake.duckdb")
  con <- dhis2_store_open(path)
  withr::defer(dhis2_store_close(con))
  old <- format(Sys.time() - 20 * 86400, "%Y-%m-%dT%H:%M:%S")
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  dhis2_log_run(con, old, "delta", "g", "OU_D1", "2001-01-01", "2001-12-31")
  dhis2_log_run(con, now, "delta", "g", "OU_D2", "2001-01-01", "2001-12-31")
  # a 20-day gap rounds up to 21, plus one day of overlap
  expect_equal(dhis2_delta_window(con, "g"), 22L)
  expect_equal(dhis2_delta_window(con, "other"), 8L)
})

test_that("analytics downloads are saved, resumed and loaded", {
  quiet_cli()
  log <- local_fake_server(fake_analytics_server())
  path <- fs::path(withr::local_tempdir(), "fake.duckdb")
  con <- dhis2_store_open(path)
  withr::defer(dhis2_store_close(con))
  interim <- withr::local_tempdir()
  run <- function() {
    dhis2_sync_analytics(
      con, paste0("DE_", 1:3), c("2026SunW1", "2026SunW2"), "LEVEL-3",
      interim, table = "analytics_subannual", period_type = "WeeklySunday",
      dx_per_request = 2, periods_per_request = 1, session = fake_session()
    )
  }
  first <- run()
  expect_equal(first$rows, 6)
  expect_length(analytics_calls(log), 4)
  expect_length(fs::dir_ls(interim, glob = "*.parquet"), 4)

  second <- run()
  expect_equal(second$rows, 0)
  expect_length(analytics_calls(log), 4)
  held <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM analytics_subannual")
  expect_equal(held$n, 6)
})

# a store and interim folder for one test, closed and removed afterwards
fake_sync_store <- function(env = parent.frame()) {
  path <- fs::path(withr::local_tempdir(.local_envir = env), "fake.duckdb")
  con <- dhis2_store_open(path)
  withr::defer(dhis2_store_close(con), envir = env)
  interim <- withr::local_tempdir(.local_envir = env)
  list(path = path, con = con, interim = interim)
}

count_rows <- function(con, table, where = "TRUE") {
  DBI::dbGetQuery(
    con, paste0("SELECT count(*) AS n FROM ", table, " WHERE ", where)
  )$n
}

test_that("a refreshed analytics request removes rows the server dropped", {
  quiet_cli()
  rows_for <- list(DE_1 = 1, DE_2 = 1)
  local_fake_server(fake_analytics_server(function(dims) {
    keep <- dims$dx[purrr::map_dbl(dims$dx, \(d) rows_for[[d]] %||% 0) > 0]
    json_resp(fake_analytics_body(keep, dims$pe))
  }))
  store <- fake_sync_store()
  run <- function(dx) {
    dhis2_sync_analytics(
      store$con, dx, "202601", "LEVEL-3", store$interim,
      table = "analytics_values", dx_per_request = 1, resume = FALSE,
      session = fake_session()
    )
  }
  run(c("DE_1", "DE_2"))
  expect_equal(count_rows(store$con, "analytics_values"), 2)

  # DE_1 now comes back empty; DE_2 was not requested and must stay
  rows_for$DE_1 <- 0
  run("DE_1")
  expect_equal(count_rows(store$con, "analytics_values"), 1)
  expect_equal(
    count_rows(store$con, "analytics_values", "data_element = 'DE_2'"), 1
  )
})

test_that("a failed analytics request leaves its old rows in place", {
  quiet_cli()
  fail <- FALSE
  local_fake_server(fake_analytics_server(function(dims) {
    if (fail) {
      return(json_resp(list(message = "fake failure"), status = 500L))
    }
    json_resp(fake_analytics_body(dims$dx, dims$pe))
  }))
  store <- fake_sync_store()
  run <- function() {
    dhis2_sync_analytics(
      store$con, "DE_1", c("202601", "202602"), "LEVEL-3", store$interim,
      table = "analytics_values", resume = FALSE, session = fake_session()
    )
  }
  run()
  expect_equal(count_rows(store$con, "analytics_values"), 2)
  fail <- TRUE
  out <- run()
  expect_equal(out$failed, 1)
  expect_equal(count_rows(store$con, "analytics_values"), 2)
})

test_that("a refresh only touches the units the selection covers", {
  quiet_cli()
  local_fake_server(fake_analytics_server(function(dims) {
    json_resp(fake_analytics_body(dims$dx, dims$pe, ou = "OU_D1"))
  }))
  store <- fake_sync_store()
  # a row for a unit at another level, and one for another period
  DBI::dbAppendTable(store$con, "analytics_values", data.frame(
    data_element = "DE_1", period = c("202601", "202612"),
    org_unit = c("OU_P1", "OU_D1"), cat_option_combo = "COC_1", value = "1"
  ))
  dhis2_sync_analytics(
    store$con, "DE_1", "202601", "LEVEL-3", store$interim,
    table = "analytics_values", resume = FALSE, session = fake_session()
  )
  held <- DBI::dbGetQuery(
    store$con, "SELECT org_unit, period, value FROM analytics_values
                ORDER BY org_unit, period"
  )
  expect_equal(held$org_unit, c("OU_D1", "OU_D1", "OU_P1"))
  expect_equal(held$value, c("5", "1", "1"))
})

test_that("analytics unit selections resolve against the hierarchy", {
  log <- local_fake_server(fake_org_route)
  s <- fake_session()
  units <- dhis2_org_units(ancestors = FALSE, session = s)
  resolve <- function(ou) .dhis2_analytics_units(ou, units, session = s)
  expect_equal(.dhis2_analytics_units("OU_D1;OU_P1"), c("OU_D1", "OU_P1"))
  expect_equal(resolve("LEVEL-3"), "OU_D1")
  expect_equal(resolve("LEVEL-2;LEVEL-3"), c("OU_P1", "OU_D1"))
  expect_equal(resolve("LEVEL-3;OU_P1"), "OU_D1")
  expect_equal(resolve("LEVEL-3;OU_D1"), "OU_D1")
  expect_equal(resolve("LEVEL-2;OU_D1"), character())
  expect_equal(resolve("LEVEL-LVL2"), "OU_P1")
  # a level under one boundary unit
  expect_equal(resolve("LEVEL-3-OU_P1"), "OU_D1")
  expect_equal(resolve("LEVEL-3-OU_D1"), "OU_D1")
  expect_equal(resolve("LEVEL-2-OU_D1"), character())

  # the account's units come from api/me and bound levels like UIDs
  expect_equal(resolve("USER_ORGUNIT"), "OU_P1")
  expect_equal(resolve("USER_ORGUNIT_CHILDREN"), "OU_D1")
  expect_equal(resolve("USER_ORGUNIT_GRANDCHILDREN"), character())
  expect_equal(resolve("USER_ORGUNIT;USER_ORGUNIT_CHILDREN"),
               c("OU_P1", "OU_D1"))
  expect_equal(resolve("USER_ORGUNIT;LEVEL-3"), "OU_D1")
  expect_equal(resolve("USER_ORGUNIT;LEVEL-1"), character())
  expect_equal(resolve("USER_ORGUNIT;OU_C"), c("OU_C", "OU_P1"))

  expect_null(resolve("FUTURE-ITEM"))
  expect_null(resolve("LEVEL-3;USER_ORGUNIT_PARENT"))
  expect_true(all(purrr::map_chr(log$calls, "path") %in% c(
    "organisationUnitLevels.json", "organisationUnits.json", "me.json"
  )))
})

test_that("a user organisation unit refresh removes deleted values", {
  quiet_cli()
  empty <- FALSE
  log <- local_fake_server(fake_analytics_server(function(dims) {
    body <- fake_analytics_body(dims$dx, dims$pe, ou = "OU_P1")
    if (empty) body$rows <- list()
    json_resp(body)
  }))
  store <- fake_sync_store()
  run <- function(ou) {
    dhis2_sync_analytics(
      store$con, "DE_1", "202601", ou, store$interim,
      table = "analytics_values", resume = FALSE, session = fake_session()
    )
  }
  run("USER_ORGUNIT")
  expect_equal(count_rows(store$con, "analytics_values"), 1)
  # only api/me is needed for the account's own units
  expect_false("organisationUnits.json" %in% purrr::map_chr(log$calls, "path"))

  # a row for the unit below the account's unit, and one for another period
  DBI::dbAppendTable(store$con, "analytics_values", data.frame(
    data_element = "DE_1", period = c("202601", "202612"),
    org_unit = c("OU_D1", "OU_P1"), cat_option_combo = "COC_1", value = "9"
  ))
  empty <- TRUE
  run("USER_ORGUNIT")
  held <- DBI::dbGetQuery(
    store$con, "SELECT org_unit, period FROM analytics_values ORDER BY 1, 2"
  )
  expect_equal(held$org_unit, c("OU_D1", "OU_P1"))
  expect_equal(held$period, c("202601", "202612"))

  run("USER_ORGUNIT_CHILDREN")
  held <- DBI::dbGetQuery(store$con, "SELECT org_unit FROM analytics_values")
  expect_equal(held$org_unit, "OU_P1")
})

# a server whose account may view data for `view` only; metadata still
# lists every district under the country
restricted_server <- function(view, empty = FALSE) {
  district <- function(id) {
    list(
      id = id, name = id, level = 3, path = paste0("/OU_C/", id),
      parent = list(id = "OU_C", name = "Fakeland")
    )
  }
  function(path, query) {
    if (path == "organisationUnitLevels.json") {
      return(json_resp(fake_levels_body))
    }
    if (path == "organisationUnits.json") {
      limited <- identical(query$userDataViewFallback, "true")
      ids <- if (limited) view else c("OU_C", "OU_D1", "OU_D2")
      units <- purrr::map(ids, \(id) {
        if (id == "OU_C") fake_units_body$organisationUnits[[1]] else {
          district(id)
        }
      })
      return(json_resp(list(
        pager = list(pageCount = 1), organisationUnits = units
      )))
    }
    body <- fake_analytics_body("DE_1", "202601", ou = view[[1]])
    if (empty) body$rows <- list()
    json_resp(body)
  }
}

two_districts <- function(con) {
  DBI::dbAppendTable(con, "analytics_values", data.frame(
    data_element = "DE_1", period = "202601",
    org_unit = c("OU_D1", "OU_D2"), cat_option_combo = "COC_1",
    value = c("5", "99")
  ))
}

test_that("a level refresh stays within the account's data view units", {
  quiet_cli()
  local_fake_server(restricted_server(view = "OU_D1", empty = TRUE))
  store <- fake_sync_store()
  two_districts(store$con)
  dhis2_sync_analytics(
    store$con, "DE_1", "202601", "LEVEL-3", store$interim,
    table = "analytics_values", resume = FALSE, session = fake_session()
  )
  held <- DBI::dbGetQuery(
    store$con, "SELECT org_unit, value FROM analytics_values"
  )
  expect_equal(held$org_unit, "OU_D2")
  expect_equal(held$value, "99")
})

test_that("a full-access account refreshes every unit at the level", {
  quiet_cli()
  local_fake_server(restricted_server(view = "OU_C", empty = TRUE))
  store <- fake_sync_store()
  two_districts(store$con)
  dhis2_sync_analytics(
    store$con, "DE_1", "202601", "LEVEL-3", store$interim,
    table = "analytics_values", resume = FALSE, session = fake_session()
  )
  expect_equal(count_rows(store$con, "analytics_values"), 0)
})

test_that("unreadable data view units stop a refresh before any change", {
  quiet_cli()
  log <- local_fake_server(restricted_server(view = character()))
  store <- fake_sync_store()
  two_districts(store$con)
  expect_error(
    dhis2_sync_analytics(
      store$con, "DE_1", "202601", "LEVEL-3", store$interim,
      table = "analytics_values", resume = FALSE, session = fake_session()
    ),
    "data view"
  )
  expect_equal(count_rows(store$con, "analytics_values"), 2)
  expect_length(analytics_calls(log), 0)
  expect_length(fs::dir_ls(store$interim), 0)
})

test_that("unrecognised selections keep old rows and warn", {
  quiet_cli()
  local_fake_server(fake_analytics_server(function(dims) {
    json_resp(fake_analytics_body(character(), dims$pe))
  }))
  store <- fake_sync_store()
  DBI::dbAppendTable(store$con, "analytics_values", data.frame(
    data_element = "DE_1", period = "202601", org_unit = "OU_D1",
    cat_option_combo = "COC_1", value = "1"
  ))
  expect_warning(
    dhis2_sync_analytics(
      store$con, "DE_1", "202601", "FUTURE-ITEM", store$interim,
      table = "analytics_values", resume = FALSE, session = fake_session()
    ),
    "cannot be worked out"
  )
  expect_equal(count_rows(store$con, "analytics_values"), 1)
})

test_that("the first twelve analytics arguments keep their positions", {
  expect_equal(
    names(formals(dhis2_sync_analytics))[1:12],
    c(
      "con", "dx", "periods", "ou", "interim_dir", "table", "period_type",
      "dx_per_request", "periods_per_request", "resume", "keep_files",
      "session"
    )
  )
})

test_that("saved analytics belong to one server and account", {
  quiet_cli()
  log <- local_fake_server(fake_analytics_server())
  interim <- withr::local_tempdir()
  run <- function(con, session) {
    dhis2_sync_analytics(
      con, "DE_1", "202601", "LEVEL-3", interim,
      table = "analytics_values", keep_files = TRUE, session = session
    )
  }
  first <- fake_sync_store()
  run(first$con, fake_session())
  expect_length(analytics_calls(log), 1)

  # same files, other server: nothing is reused and nothing old is loaded
  other <- fake_session_as("fake_user", "https://other.fake.example")
  second <- fake_sync_store()
  run(second$con, other)
  expect_length(analytics_calls(log), 2)
  expect_equal(count_rows(second$con, "analytics_values"), 1)

  # same server, other account
  third <- fake_sync_store()
  run(third$con, fake_session_as())
  expect_length(analytics_calls(log), 3)

  # same request again is still skipped
  run(first$con, fake_session())
  expect_length(analytics_calls(log), 3)
  manifests <- fs::dir_ls(interim, glob = "*.qs2")
  expect_length(manifests, 3)
  expect_false(any(grepl("fake_pass", purrr::map_chr(manifests, \(m) {
    paste(unlist(qs2::qs_read(m)), collapse = " ")
  }))))
})

test_that("files from an earlier split are not loaded into a new store", {
  quiet_cli()
  value <- "5"
  local_fake_server(fake_analytics_server(function(dims) {
    body <- fake_analytics_body(dims$dx, dims$pe)
    body$rows <- purrr::map(body$rows, \(r) c(r[1:4], value))
    json_resp(body)
  }))
  interim <- withr::local_tempdir()
  first <- fake_sync_store()
  dhis2_sync_analytics(
    first$con, c("DE_1", "DE_2"), "202601", "LEVEL-3", interim,
    table = "analytics_values", dx_per_request = 2, keep_files = TRUE,
    session = fake_session()
  )
  value <- "7"
  second <- fake_sync_store()
  dhis2_sync_analytics(
    second$con, c("DE_1", "DE_2"), "202601", "LEVEL-3", interim,
    table = "analytics_values", dx_per_request = 1, keep_files = TRUE,
    session = fake_session()
  )
  held <- DBI::dbGetQuery(second$con, "SELECT value FROM analytics_values")
  expect_equal(held$value, c("7", "7"))
})

test_that("a longer window downloads the added history in full", {
  quiet_cli()
  log <- local_fake_server(values_server())
  store <- fake_sync_store()
  sync <- function(end) {
    dhis2_sync_group(
      store$con, "fake_group", fake_units, store$interim,
      window_start = "2001-01-01", window_end = end,
      data_element_group = "DEG_1", workers = 1, session = fake_session()
    )
  }
  sync("2001-12-31")
  expect_length(log$calls, 3)

  sync("2002-12-31")
  new_calls <- log$calls[4:length(log$calls)]
  history <- purrr::keep(new_calls, \(x) is.null(x$query$lastUpdatedDuration))
  updates <- purrr::keep(new_calls, \(x) !is.null(x$query$lastUpdatedDuration))
  expect_length(history, 3)
  expect_equal(
    unique(purrr::map_chr(history, \(x) x$query$startDate)), "2002-01-01"
  )
  expect_equal(
    unique(purrr::map_chr(history, \(x) x$query$endDate)), "2002-12-31"
  )
  expect_length(updates, 3)

  plan <- dhis2_plan_run(
    store$con, fake_units, "2001-01-01", "fake_group",
    window_end = "2002-12-31"
  )
  expect_equal(plan$mode, "delta")
  shorter <- dhis2_plan_run(
    store$con, fake_units, "2001-06-01", "fake_group",
    window_end = "2002-06-30"
  )
  expect_equal(shorter$mode, "delta")
})

test_that("a new unit or selection needs its own full download", {
  quiet_cli()
  local_fake_server(values_server())
  store <- fake_sync_store()
  dhis2_sync_group(
    store$con, "fake_group", fake_units[1:2], store$interim,
    window_start = "2001-01-01", window_end = "2001-12-31",
    data_element_group = "DEG_1", workers = 1, session = fake_session()
  )
  plan <- dhis2_plan_run(
    store$con, fake_units, "2001-01-01", "fake_group",
    window_end = "2001-12-31",
    selection = .dhis2_selection(data_element_group = "DEG_1")
  )
  expect_equal(plan$mode, "backfill")
  expect_equal(plan$todo, "OU_D3")
  expect_equal(plan$gaps$org_unit, "OU_D3")
  expect_equal(plan$gaps$start, as.Date("2001-01-01"))

  changed <- dhis2_plan_run(
    store$con, fake_units[1:2], "2001-01-01", "fake_group",
    window_end = "2001-12-31",
    selection = .dhis2_selection(data_set = "DS_1")
  )
  expect_equal(changed$mode, "backfill")
  expect_setequal(changed$todo, fake_units[1:2])
  # the earlier call signature still works
  legacy <- dhis2_plan_run(store$con, fake_units[1:2], "2001-01-01",
                           "fake_group")
  expect_equal(legacy$mode, "delta")
  expect_setequal(legacy$banked, fake_units[1:2])
})

test_that("downloads logged without a selection do not certify one", {
  quiet_cli()
  log <- local_fake_server(values_server())
  store <- fake_sync_store()
  DBI::dbExecute(store$con, "ALTER TABLE download_log DROP COLUMN selection")
  DBI::dbAppendTable(store$con, "download_log", data.frame(
    run_at = "2001-01-01T00:00:00", mode = "backfill",
    element_group = "g", org_unit = "OU_D1", window_start = "2001-01-01",
    window_end = "2001-12-31", rows_returned = 1L
  ))
  dhis2_store_close(store$con)
  con <- dhis2_store_open(store$path)
  withr::defer(dhis2_store_close(con))
  expect_true("selection" %in% DBI::dbListFields(con, "download_log"))

  # the old signature still treats the unit as complete
  legacy <- dhis2_plan_run(con, "OU_D1", "2001-01-01", "g")
  expect_equal(legacy$mode, "delta")
  plan <- dhis2_plan_run(
    con, "OU_D1", "2001-01-01", "g", window_end = "2001-12-31",
    selection = "dataSet=DS_1"
  )
  expect_equal(plan$mode, "backfill")
  expect_equal(plan$todo, "OU_D1")

  # the first run with a selection downloads the history once more
  dhis2_sync_group(
    con, "g", "OU_D1", store$interim,
    window_start = "2001-01-01", window_end = "2001-12-31",
    data_set = "DS_1", workers = 1, session = fake_session()
  )
  expect_length(log$calls, 1)
  expect_null(log$calls[[1]]$query$lastUpdatedDuration)
  expect_equal(count_rows(con, "data_values"), 2)
  expect_equal(count_rows(con, "download_log"), 2)
  again <- dhis2_plan_run(
    con, "OU_D1", "2001-01-01", "g", window_end = "2001-12-31",
    selection = "dataSet=DS_1"
  )
  expect_equal(again$mode, "delta")
})

test_that("gaps between downloaded ranges are found", {
  covered <- tibble::tibble(
    start = as.Date(c("2001-01-01", "2002-01-01")),
    end = as.Date(c("2001-12-31", "2002-06-30"))
  )
  gaps <- .dhis2_uncovered(as.Date("2000-07-01"), as.Date("2002-12-31"),
                           covered)
  expect_equal(gaps$start, as.Date(c("2000-07-01", "2002-07-01")))
  expect_equal(gaps$end, as.Date(c("2000-12-31", "2002-12-31")))
  none <- .dhis2_uncovered(as.Date("2001-03-01"), as.Date("2002-03-01"),
                           covered)
  expect_equal(nrow(none), 0)
})

test_that("the session's delta_days setting sets the update interval", {
  quiet_cli()
  log <- local_fake_server(values_server())
  store <- fake_sync_store()
  s <- fake_session(delta_days = 30)
  sync <- function() {
    dhis2_sync_group(
      store$con, "fake_group", fake_units, store$interim,
      window_start = "2001-01-01", window_end = "2001-12-31",
      data_element_group = "DEG_1", workers = 1, session = s
    )
  }
  sync()
  sync()
  expect_equal(log$calls[[4]]$query$lastUpdatedDuration, "30d")
  expect_equal(dhis2_delta_window(store$con, "fake_group", session = s), 30L)
  expect_equal(dhis2_delta_window(store$con, "fake_group"), 8L)
  expect_equal(
    dhis2_delta_window(store$con, "fake_group", floor_days = 3, session = s),
    3L
  )
})

test_that("saved unit files from another server or request are not reused", {
  quiet_cli()
  log <- local_fake_server(values_server())
  dir <- withr::local_tempdir()
  fetch <- function(session, ...) {
    dhis2_fetch_units(
      fake_units, dir, start_date = "2001-01-01", end_date = "2001-12-31",
      data_element_group = "DEG_1", workers = 1, progress = FALSE,
      session = session, ...
    )
  }
  fetch(fake_session())
  expect_length(log$calls, 3)
  fetch(fake_session_as())
  expect_length(log$calls, 6)
  fetch(fake_session_as())
  expect_length(log$calls, 6)
  fetch(fake_session_as(), last_updated_duration = "8d")
  expect_length(log$calls, 9)
  manifest <- qs2::qs_read(fs::path(dir, "manifest.qs2"))
  expect_false(grepl("fake_pass", paste(unlist(manifest), collapse = " ")))
})
