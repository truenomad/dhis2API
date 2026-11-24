test_that("a session reads the environment and strips a trailing api", {
  withr::local_envvar(
    DHIS2_BASE_URL = paste0(fake_base, "/api/"),
    DHIS2_USERNAME = "env_user", DHIS2_PASSWORD = "env_pass",
    DHIS2_TOKEN = ""
  )
  s <- dhis2_session(keyring = FALSE, set_default = FALSE)
  expect_s3_class(s, "dhis2_session")
  expect_equal(s$base_url, fake_base)
  expect_equal(s$auth, "basic")
  expect_equal(s$username, "env_user")
})

test_that("the token is used when no password is available", {
  withr::local_envvar(
    DHIS2_BASE_URL = fake_base, DHIS2_USERNAME = "", DHIS2_PASSWORD = "",
    DHIS2_TOKEN = "fake_token"
  )
  s <- dhis2_session(keyring = FALSE, set_default = FALSE)
  expect_equal(s$auth, "token")
  expect_null(s$password)
})

test_that("missing settings abort with a hint", {
  withr::local_envvar(
    DHIS2_BASE_URL = "", DHIS2_USERNAME = "", DHIS2_PASSWORD = "",
    DHIS2_TOKEN = ""
  )
  expect_error(dhis2_session(keyring = FALSE), "No server address")
  expect_error(
    dhis2_session(fake_base, keyring = FALSE), "No credentials"
  )
  expect_error(
    dhis2_session(fake_base, username = "u", auth = "basic",
                  keyring = FALSE),
    "both a username and a password"
  )
  expect_error(
    dhis2_session(fake_base, token = "t", keyring = FALSE, timeot = 5),
    "Unknown setting"
  )
})

test_that("printing never shows the secret", {
  s <- fake_session()
  shown <- utils::capture.output(print(s))
  expect_false(any(grepl("fake_pass", shown)))
  expect_true(any(grepl("fake_user", shown)))
})

test_that("the default session is set, used and cleared", {
  withr::defer(dhis2_clear_session())
  s <- dhis2_session(
    fake_base, token = "t", keyring = FALSE, requests_per_minute = Inf
  )
  expect_identical(dhis2_get_session(), s)
  expect_identical(.dhis2_session(), s)
  dhis2_clear_session()
  expect_null(dhis2_get_session())
  expect_error(dhis2_set_session(list()), "must come from")
})

test_that("a setting comes from the session, the option, then the default", {
  s <- fake_session(timeout = 7)
  expect_equal(.dhis2_setting(s, "timeout"), 7)
  withr::local_options(dhis2api.page_size = 50)
  expect_equal(.dhis2_setting(s, "page_size"), 50)
  expect_equal(.dhis2_setting(s, "workers"), 6)
  expect_error(.dhis2_setting(s, "nonsense"), "Unknown setting")
})
