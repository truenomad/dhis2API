# Synthetic fixtures only; nothing here comes from a server.

quiet_cli <- function(env = parent.frame()) {
  withr::local_options(
    list(cli.default_handler = function(msg) invisible()), .local_envir = env
  )
}

fake_base <- "https://dhis2.fake.example"

# a session that sends no throttle and tries once
fake_session <- function(...) {
  dhis2_session(
    fake_base, username = "fake_user", password = "fake_pass",
    keyring = FALSE, set_default = FALSE,
    requests_per_minute = Inf, max_tries = 1, ...
  )
}

# the same server as fake_session(), as another account
fake_session_as <- function(username = "other_user", base_url = fake_base,
                            ...) {
  dhis2_session(
    base_url, username = username, password = "other_pass",
    keyring = FALSE, set_default = FALSE,
    requests_per_minute = Inf, max_tries = 1, ...
  )
}

json_resp <- function(body, status = 200L) {
  httr2::response_json(status_code = status, body = body)
}

csv_resp <- function(text, status = 200L) {
  httr2::response(
    status_code = status,
    headers = list(`Content-Type` = "text/csv"),
    body = charToRaw(text)
  )
}

html_resp <- function(status) {
  httr2::response(
    status_code = status,
    headers = list(`Content-Type` = "text/html"),
    body = charToRaw("<html><body>blocked</body></html>")
  )
}

# the path after api/ and the query of a request, with repeated parameters
# gathered into vectors
req_parts <- function(req) {
  url <- httr2::url_parse(req$url)
  query <- url$query %||% list()
  if (length(query) == 0) {
    return(list(path = sub("^/api/", "", url$path), query = list()))
  }
  keys <- unique(names(query))
  query <- purrr::map(
    rlang::set_names(keys),
    \(k) unlist(query[names(query) == k], use.names = FALSE)
  )
  list(path = sub("^/api/", "", url$path), query = query)
}

# mock a server from a function of path and query; counts the calls
local_fake_server <- function(route, env = parent.frame()) {
  .dhis2_state$info <- NULL
  withr::defer(.dhis2_state$info <- NULL, envir = env)
  log <- new.env()
  log$calls <- list()
  httr2::local_mocked_responses(function(req) {
    parts <- req_parts(req)
    log$calls[[length(log$calls) + 1]] <- parts
    route(parts$path, parts$query)
  }, env = env)
  log
}

# org units in three levels: a country, two provinces, a district each
fake_levels_body <- list(
  pager = list(page = 1, pageCount = 1),
  organisationUnitLevels = list(
    list(id = "LVL1", level = 1, name = "Country"),
    list(id = "LVL2", level = 2, name = "Province"),
    list(id = "LVL3", level = 3, name = "District")
  )
)

fake_units_body <- list(
  pager = list(page = 1, pageCount = 1),
  organisationUnits = list(
    list(
      id = "OU_C", name = "Fakeland", level = 1, path = "/OU_C",
      ancestors = list()
    ),
    list(
      id = "OU_P1", name = "Province A", level = 2, path = "/OU_C/OU_P1",
      parent = list(id = "OU_C", name = "Fakeland"),
      ancestors = list(list(id = "OU_C", name = "Fakeland", level = 1))
    ),
    list(
      id = "OU_D1", name = "District A1", level = 3, code = "D1",
      path = "/OU_C/OU_P1/OU_D1",
      openingDate = "2001-01-01T00:00:00.000",
      parent = list(id = "OU_P1", name = "Province A"),
      ancestors = list(
        list(id = "OU_C", name = "Fakeland", level = 1),
        list(id = "OU_P1", name = "Province A", level = 2)
      )
    )
  )
)

# a dataValueSets CSV for one unit, in the lower-case header the server
# writes; `n` rows with fake values
fake_values_csv <- function(ou, n = 2, period = "200101") {
  header <- paste(
    "dataelement,period,orgunit,categoryoptioncombo,attributeoptioncombo,",
    "value,storedby,lastupdated,comment,followup,deleted", sep = ""
  )
  if (n == 0) {
    return(paste0(header, "\n"))
  }
  rows <- paste0(
    "DE_", seq_len(n), ",", period, ",", ou, ",COC_1,AOC_1,",
    seq_len(n) * 10, ",fake_user,2001-02-01T00:00:00.000,,false,false"
  )
  paste(c(header, rows), collapse = "\n")
}

# the organisation unit metadata routes and the account's units (Province
# A), or NULL for any other path
fake_org_route <- function(path, query) {
  switch(path,
    "organisationUnitLevels.json" = json_resp(fake_levels_body),
    "organisationUnits.json" = json_resp(fake_units_body),
    "me.json" = json_resp(list(
      id = "U_1", organisationUnits = list(list(id = "OU_P1"))
    )),
    NULL
  )
}

# an analytics server answering metadata and analytics; `answer` maps the
# dimensions of a request to a response, by default one row per element
# and period
fake_analytics_server <- function(answer = NULL) {
  answer <- answer %||% function(dims) {
    json_resp(fake_analytics_body(dims$dx, dims$pe))
  }
  function(path, query) {
    fake_org_route(path, query) %||% {
      dims <- strsplit(sub("^[a-z]+:", "", query$dimension), ";")
      names(dims) <- sub(":.*$", "", query$dimension)
      answer(dims)
    }
  }
}

# the analytics requests among a server's logged calls
analytics_calls <- function(log) {
  purrr::keep(log$calls, \(x) x$path == "analytics.json")
}

# an analytics response with one row per element and period
fake_analytics_body <- function(dx, pe, ou = "OU_D1") {
  grid <- tidyr::expand_grid(dx = dx, pe = pe)
  list(
    headers = list(
      list(name = "dx"), list(name = "pe"), list(name = "ou"),
      list(name = "co"), list(name = "value")
    ),
    rows = purrr::pmap(grid, \(dx, pe) list(dx, pe, ou, "COC_1", "5"))
  )
}
