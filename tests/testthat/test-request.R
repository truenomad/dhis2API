test_that("paths gain the api prefix and the version", {
  expect_equal(.dhis2_api_path("api/me.json"), "api/me.json")
  expect_equal(.dhis2_api_path("/me.json"), "api/me.json")
  expect_equal(.dhis2_api_path("api/me.json", 40), "api/40/me.json")
})

test_that("a vector query value repeats the parameter", {
  req <- dhis2_request(
    "analytics.json", dimension = c("dx:A", "pe:B"), session = fake_session()
  )
  parts <- req_parts(req)
  expect_equal(parts$path, "analytics.json")
  expect_equal(parts$query$dimension, c("dx:A", "pe:B"))
  expect_true("Authorization" %in% names(req$headers))
  expect_false(req$options$followlocation)
})

test_that("dhis2_get parses JSON and gives NULL for an empty body", {
  local_fake_server(function(path, query) {
    if (path == "me.json") {
      return(json_resp(list(username = "fake_user")))
    }
    httr2::response(status_code = 200)
  })
  s <- fake_session()
  expect_equal(dhis2_get("me.json", session = s)$username, "fake_user")
  expect_null(dhis2_get("dataValueSets.json", session = s))
})

test_that("failures name the cause", {
  local_fake_server(function(path, query) {
    switch(path,
      "a" = json_resp(list(message = "Bad org unit: FAKE"), status = 409L),
      "b" = html_resp(403L),
      "c" = httr2::response(status_code = 302),
      "d" = json_resp(list(message = "no"), status = 401L)
    )
  })
  s <- fake_session()
  expect_error(dhis2_get("a", session = s), "Bad org unit",
               class = "dhis2_http_error")
  expect_error(dhis2_get("b", session = s), "reverse proxy")
  expect_error(dhis2_get("c", session = s), "login page")
  expect_error(dhis2_get("d", session = s), "rejected the credentials")
})

test_that("dhis2_get_all walks every page", {
  log <- local_fake_server(function(path, query) {
    page <- as.integer(query$page)
    json_resp(list(
      pager = list(page = page, pageCount = 3),
      dataElements = list(list(id = paste0("DE", page), name = "fake"))
    ))
  })
  quiet_cli()
  out <- dhis2_get_all(
    "dataElements.json", "dataElements", session = fake_session()
  )
  expect_equal(out$id, c("DE1", "DE2", "DE3"))
  expect_length(log$calls, 3)
  expect_equal(log$calls[[1]]$query$pageSize, "1000")
})

test_that("dhis2_ping reports the account and the version", {
  local_fake_server(function(path, query) {
    if (path == "system/info.json") {
      return(json_resp(list(version = "2.41.11")))
    }
    json_resp(list(id = "U1", username = "fake_user"))
  })
  shown <- cli::cli_fmt(me <- dhis2_ping(fake_session()))
  expect_equal(me$username, "fake_user")
  expect_match(paste(shown, collapse = " "), "2.41.11")
})

test_that("a web page sent with status 200 is refused", {
  local_fake_server(function(path, query) html_resp(200L))
  expect_error(
    dhis2_get("me.json", session = fake_session()),
    class = "dhis2_html_error"
  )
})

test_that("a refused token login points at the server version", {
  local_fake_server(function(path, query) {
    json_resp(list(message = "no"), status = 401L)
  })
  s <- dhis2_session(
    fake_base, token = "t", auth = "token", keyring = FALSE,
    set_default = FALSE,
    requests_per_minute = Inf, max_tries = 1
  )
  expect_error(dhis2_get("me.json", session = s), "2.38 or later")
})
