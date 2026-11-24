dims_of <- function(call) {
  dims <- call$query$dimension
  rlang::set_names(sub("^[a-z]+:", "", dims), sub(":.*$", "", dims))
}

test_that("dimensions are written dimension:item;item", {
  expect_equal(dhis2_dimension("dx", c("A", "B")), "dx:A;B")
  expect_equal(dhis2_dimension("co"), "co")
})

test_that("analytics rows parse to character columns", {
  log <- local_fake_server(function(path, query) {
    json_resp(fake_analytics_body(c("DE_1", "DE_2"), "200101"))
  })
  out <- dhis2_analytics(
    c("DE_1", "DE_2"), "200101", "LEVEL-3", session = fake_session()
  )
  expect_equal(names(out), c("dx", "pe", "ou", "co", "value"))
  expect_equal(nrow(out), 2)
  expect_type(out$value, "character")
  expect_equal(log$calls[[1]]$query$skipMeta, "true")
  expect_true("co" %in% log$calls[[1]]$query$dimension)
})

test_that("an empty grid keeps its columns", {
  local_fake_server(function(path, query) {
    json_resp(list(
      headers = list(list(name = "dx"), list(name = "value")), rows = list()
    ))
  })
  out <- dhis2_analytics("DE_1", "200101", "LEVEL-3", session = fake_session())
  expect_equal(names(out), c("dx", "value"))
  expect_equal(nrow(out), 0)
})

test_that("requests split by element and period", {
  log <- local_fake_server(function(path, query) {
    dims <- strsplit(dims_of(list(query = query)), ";")
    json_resp(fake_analytics_body(dims$dx, dims$pe))
  })
  out <- dhis2_analytics(
    paste0("DE_", 1:5), paste0("20010", 1:4), "LEVEL-3",
    dx_per_request = 2, pe_per_request = 2, session = fake_session()
  )
  expect_length(log$calls, 6)
  expect_equal(nrow(out), 20)
})

test_that("a request that times out is halved by period", {
  log <- local_fake_server(function(path, query) {
    dims <- strsplit(dims_of(list(query = query)), ";")
    if (length(dims$pe) > 1) {
      return(json_resp(list(message = "timeout"), status = 504L))
    }
    json_resp(fake_analytics_body(dims$dx, dims$pe))
  })
  out <- dhis2_analytics(
    "DE_1", paste0("20010", 1:4), "LEVEL-3", session = fake_session()
  )
  expect_equal(sort(out$pe), paste0("20010", 1:4))
  # 1 of 4, 2 of 2, 4 of 1
  expect_length(log$calls, 7)
})

test_that("a refusal on authorisation stops rather than halving", {
  log <- local_fake_server(function(path, query) {
    json_resp(list(message = "User is not allowed to read"), status = 403L)
  })
  expect_error(
    dhis2_analytics("DE_1", c("200101", "200102"), "LEVEL-3",
                    session = fake_session()),
    "cannot help"
  )
  expect_length(log$calls, 1)
})

test_that("a piece that keeps failing is reported and left out", {
  local_fake_server(function(path, query) {
    dims <- strsplit(dims_of(list(query = query)), ";")
    if ("200102" %in% dims$pe) {
      return(json_resp(list(message = "fake failure"), status = 500L))
    }
    json_resp(fake_analytics_body(dims$dx, dims$pe))
  })
  expect_warning(
    out <- dhis2_analytics("DE_1", c("200101", "200102"), "LEVEL-3",
                           session = fake_session()),
    "missing from the result"
  )
  expect_equal(out$pe, "200101")
})

test_that("analytics take the store's shape", {
  d <- tibble::tibble(
    dx = "DE_1", pe = "2026SunW1", ou = "OU_D1", co = "COC_1", value = "5"
  )
  expect_equal(
    names(dhis2_analytics_to_store(d)),
    names(.dhis2_store_schema$analytics_values)
  )
  sub <- dhis2_analytics_to_subannual(d, "WeeklySunday")
  expect_equal(names(sub), names(.dhis2_store_schema$analytics_subannual))
  expect_equal(sub$period_type, "WeeklySunday")
  expect_equal(nrow(dhis2_analytics_to_store(NULL)), 0)
})
