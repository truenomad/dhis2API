test_that("versions are read with or without a suffix", {
  expect_equal(.dhis2_parse_version("2.41.11"), numeric_version("2.41.11"))
  expect_equal(.dhis2_parse_version("2.44-SNAPSHOT"), numeric_version("2.44"))
  expect_equal(.dhis2_parse_version("41.1"), numeric_version("2.41.1"))
  expect_error(.dhis2_parse_version("unknown"), "Cannot read")
})

test_that("server info is read once per server", {
  log <- local_fake_server(function(path, query) {
    json_resp(list(version = "2.40.3"))
  })
  s <- fake_session()
  expect_true(dhis2_server_version(s) < "2.41")
  dhis2_server_version(s)
  expect_length(log$calls, 1)
  expect_error(
    .dhis2_require_version(s, "2.41", "Fake feature"),
    "needs DHIS2 2.41"
  )
})

test_that("the weekly check passes when the server agrees", {
  quiet_cli()
  log <- local_fake_server(function(path, query) {
    pe <- sub("^pe:", "", grep("^pe:", query$dimension, value = TRUE))
    year <- as.integer(substr(pe, 1, 4))
    week <- as.integer(sub("^.*SunW", "", pe))
    if (week > dhis2_weeks_in_year(year)) {
      return(json_resp(list(message = "no such week"), status = 409L))
    }
    json_resp(fake_analytics_body("DE_1", pe))
  })
  out <- dhis2_check_weekly_periods(
    "DE_1", "OU_C", years = 2019:2021, session = fake_session()
  )
  expect_true(all(out$agrees))
  expect_equal(out$weeks, c(52L, 53L, 52L))
  expect_length(log$calls, 6)
})

test_that("the weekly check aborts when the server disagrees", {
  quiet_cli()
  local_fake_server(function(path, query) {
    json_resp(fake_analytics_body("DE_1", "x"))
  })
  expect_error(
    dhis2_check_weekly_periods(
      "DE_1", "OU_C", years = 2020, session = fake_session()
    ),
    "calendar differs"
  )
})
