#' Build a DHIS2 request
#'
#' Builds an authenticated httr2 request using the session's settings.
#' A vector query value repeats the parameter once per element, allowing
#' multiple analytics dimensions. This function builds the request;
#' [dhis2_get()] sends it and reports HTTP errors.
#'
#' Redirects are not followed. A redirect to a login page is reported as
#' an error by [dhis2_get()].
#'
#' @param path Path under the server, such as `"api/me.json"` or
#'   `"me.json"`. A leading `api/` is optional.
#' @param ... Query parameters.
#' @param session A session from [dhis2_session()]; the default session
#'   when `NULL`.
#' @return An httr2 request.
#' @export
dhis2_request <- function(path, ..., session = NULL) {
  session <- .dhis2_session(session)
  setting <- function(name) .dhis2_setting(session, name)
  statuses <- setting("retry_statuses")

  req <- httr2::request(session$base_url) |>
    httr2::req_url_path_append(.dhis2_api_path(path, session$api_version)) |>
    httr2::req_url_query(..., .multi = "explode") |>
    httr2::req_user_agent(setting("user_agent")) |>
    httr2::req_options(
      followlocation = FALSE, http_version = setting("http_version")
    ) |>
    httr2::req_timeout(setting("timeout")) |>
    httr2::req_retry(
      max_tries = setting("max_tries"),
      retry_on_failure = setting("retry_on_failure"),
      is_transient = \(resp) httr2::resp_status(resp) %in% statuses
    ) |>
    httr2::req_error(is_error = \(resp) FALSE)

  per_minute <- setting("requests_per_minute")
  if (is.finite(per_minute)) {
    req <- httr2::req_throttle(req, capacity = per_minute, fill_time_s = 60)
  }

  if (session$auth == "basic") {
    return(httr2::req_auth_basic(req, session$username, session$password))
  }
  httr2::req_headers(
    req,
    Authorization = paste("ApiToken", session$token),
    .redact = "Authorization"
  )
}

# "api/x", "/api/x" and "x" all become api/x, with the version if one is set
.dhis2_api_path <- function(path, api_version = NULL) {
  path <- sub("^api/", "", sub("^/+", "", path))
  if (is.null(api_version)) {
    return(paste0("api/", path))
  }
  paste0("api/", api_version, "/", path)
}

# status alone misleads: a 403 in HTML is the proxy, a 302 the login page
.dhis2_stop_for_status <- function(resp, session) {
  status <- httr2::resp_status(resp)
  if (status == 200) {
    return(invisible(resp))
  }
  ctype <- httr2::resp_content_type(resp) %||% ""
  from_proxy <- grepl("html", ctype, fixed = TRUE)

  detail <- if (status == 302) {
    "The request was redirected to the login page, so it arrived
     unauthenticated."
  } else if (status == 401) {
    "DHIS2 rejected the credentials."
  } else if (status == 403 && from_proxy) {
    "The reverse proxy rejected the request before it reached DHIS2."
  } else if (status == 403) {
    "The account is logged in but does not have permission for this request."
  } else if (status == 409) {
    "DHIS2 rejected the query."
  } else {
    "The request failed."
  }

  hint <- if (status == 403 && from_proxy && session$auth == "token") {
    "Some servers block {.field Authorization: ApiToken} at the proxy. Open
     the session with {.code auth = \"basic\"} instead."
  } else if (status == 401 && session$auth == "token") {
    "Token login needs DHIS2 2.38 or later. On an older server, open the
     session with a username and password."
  } else if (status %in% c(302, 401)) {
    "Check the username and password by logging in through the browser
     first. Repeated failures can lock the account."
  } else if (status == 409) {
    "Check for an invalid weekly period identifier or analytics tables
     that have not been built on the server."
  } else {
    NULL
  }

  # DHIS2 names the offending org unit or dimension in the body
  api_msg <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = TRUE)$message,
    error = function(e) NULL
  )

  cli::cli_abort(
    c(
      "DHIS2 request failed ({status}).",
      "x" = detail,
      if (!is.null(api_msg)) c(">" = "{api_msg}"),
      "i" = hint
    ),
    class = "dhis2_http_error",
    status = status
  )
}

#' Read a DHIS2 API response
#'
#' Sends a GET request and returns the JSON response as R objects. Stops
#' with an error on any HTTP status other than 200, using the server's
#' message and a suggested action where available. An empty response
#' returns `NULL`.
#'
#' @inheritParams dhis2_request
#' @param simplify If `TRUE`, simplify JSON to data frames where possible.
#' @return The parsed JSON, or `NULL` for an empty body.
#' @export
dhis2_get <- function(path, ..., simplify = TRUE, session = NULL) {
  session <- .dhis2_session(session)
  resp <- httr2::req_perform(dhis2_request(path, ..., session = session))
  .dhis2_stop_for_status(resp, session)
  if (!httr2::resp_has_body(resp) || length(httr2::resp_body_raw(resp)) == 0) {
    return(NULL)
  }
  .dhis2_stop_for_html(resp, path)
  httr2::resp_body_json(resp, simplifyVector = simplify)
}

# Reject HTML responses here, before a JSON or table parser tries to read them.
.dhis2_stop_for_html <- function(resp, path) {
  ctype <- httr2::resp_content_type(resp) %||% ""
  if (!grepl("html", ctype, fixed = TRUE)) {
    return(invisible(resp))
  }
  cli::cli_abort(
    c(
      "{.path {path}} returned a web page instead of data.",
      "i" = "This is usually a login page or a notice from a proxy in front
             of the server. Check the address and the credentials in a
             browser."
    ),
    class = "dhis2_html_error"
  )
}

#' Read every page of a DHIS2 collection
#'
#' Downloads one page at a time and combines the results. This avoids
#' requesting a large collection in a single response.
#'
#' @inheritParams dhis2_get
#' @param collection Name of the collection in the response, such as
#'   `"organisationUnits"`.
#' @param page_size Rows per page; defaults to the `page_size` setting.
#' @param progress Report each page.
#' @return A tibble when `simplify` is `TRUE`, else a list of records.
#' @export
dhis2_get_all <- function(path, collection, ..., page_size = NULL,
                          simplify = TRUE, progress = TRUE, session = NULL) {
  session <- .dhis2_session(session)
  page_size <- page_size %||% .dhis2_setting(session, "page_size")
  pages <- list()
  page <- 1
  repeat {
    res <- dhis2_get(
      path, ..., page = page, pageSize = page_size,
      simplify = simplify, session = session
    )
    rows <- res[[collection]]
    if (is.null(rows) || NROW(rows) == 0) {
      break
    }
    pages[[page]] <- rows
    total <- res$pager$pageCount %||% 1
    if (isTRUE(progress) && total > 1) {
      cli::cli_alert_info("{collection}: page {page} of {total}")
    }
    if (page >= total) {
      break
    }
    page <- page + 1
  }
  if (isTRUE(simplify)) {
    return(tibble::as_tibble(dplyr::bind_rows(pages)))
  }
  purrr::list_flatten(pages)
}

#' Check the connection and credentials
#'
#' Checks the login and reports the account name and server version.
#'
#' @inheritParams dhis2_request
#' @return The `api/me` record, invisibly.
#' @export
dhis2_ping <- function(session = NULL) {
  session <- .dhis2_session(session)
  me <- dhis2_get(
    "api/me.json", fields = "id,username,displayName", session = session
  )
  version <- as.character(dhis2_server_version(session = session))
  cli::cli_alert_success(
    "Connected to {.url {session$base_url}} (DHIS2 {version}) as
     {.strong {me$username}}"
  )
  invisible(me)
}
