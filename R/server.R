#' Server version and details
#'
#' Reads `api/system/info` once per server and keeps it for the R session.
#' `dhis2_server_version()` gives the version as a `numeric_version`, with
#' any suffix such as `-SNAPSHOT` dropped, so it can be compared:
#' `dhis2_server_version() >= "2.41"`.
#'
#' @inheritParams dhis2_request
#' @param refresh Read the server again instead of using cached details.
#' @return `dhis2_server_info()` gives a list; `dhis2_server_version()` a
#'   `numeric_version`.
#' @export
dhis2_server_info <- function(session = NULL, refresh = FALSE) {
  session <- .dhis2_session(session)
  key <- session$base_url
  .dhis2_state$info <- .dhis2_state$info %||% list()
  if (!refresh && !is.null(.dhis2_state$info[[key]])) {
    return(.dhis2_state$info[[key]])
  }
  info <- dhis2_get(
    "api/system/info.json",
    fields = "version,revision,buildTime,serverDate,contextPath,calendar",
    simplify = TRUE, session = session
  )
  .dhis2_state$info[[key]] <- info
  info
}

#' @rdname dhis2_server_info
#' @export
dhis2_server_version <- function(session = NULL, refresh = FALSE) {
  .dhis2_parse_version(
    dhis2_server_info(session = session, refresh = refresh)$version
  )
}

# "2.41.11" and "2.44-SNAPSHOT" both compare; "41.1" (the new numbering in
# release notes) is read as 2.41.1
.dhis2_parse_version <- function(text) {
  core <- sub("[^0-9.].*$", "", text %||% "")
  if (!nzchar(core)) {
    cli::cli_abort("Cannot read a version from {.val {text}}.")
  }
  if (!startsWith(core, "2.")) {
    core <- paste0("2.", core)
  }
  numeric_version(core)
}

# abort before a request the server is too old to answer
.dhis2_require_version <- function(session, minimum, feature) {
  version <- dhis2_server_version(session = session)
  if (version < minimum) {
    cli::cli_abort(c(
      "{feature} needs DHIS2 {minimum} or later.",
      "x" = "{.url {session$base_url}} runs {as.character(version)}."
    ))
  }
  invisible(version)
}

#' Check the weekly calendar against the server
#'
#' Checks that the server accepts the last week of each year calculated
#' by the package and rejects the following week with HTTP status 409.
#' A mismatch means the period identifiers from [dhis2_weekly_periods()]
#' may omit valid weeks or include invalid ones on this server.
#'
#' Run once for each server and weekly period type you use.
#'
#' @param dx One data element UID the account can read.
#' @param ou One organisation unit UID the account can read.
#' @inheritParams dhis2_week_year_start
#' @param years Years to check.
#' @inheritParams dhis2_request
#' @return A tibble with one row per year: the week count, whether the last
#'   week was accepted and whether the week after was refused. Aborts when
#'   any year disagrees.
#' @export
dhis2_check_weekly_periods <- function(dx, ou,
                                       period_type = "WeeklySunday",
                                       years = NULL, session = NULL) {
  session <- .dhis2_session(session)
  prefix <- .dhis2_weekly_type(period_type)$prefix
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  years <- years %||% seq(this_year - 6L, this_year)

  status_of <- function(period) {
    req <- dhis2_request(
      "api/analytics.json",
      dimension = c(dhis2_dimension("dx", dx), dhis2_dimension("pe", period)),
      filter = dhis2_dimension("ou", ou),
      skipMeta = "true",
      session = session
    )
    httr2::resp_status(httr2::req_perform(req))
  }

  out <- tibble::tibble(
    year = as.integer(years),
    weeks = dhis2_weeks_in_year(years, period_type)
  ) |>
    dplyr::mutate(
      last_status = purrr::map2_int(
        .data$year, .data$weeks, \(y, n) status_of(paste0(y, prefix, n))
      ),
      past_status = purrr::map2_int(
        .data$year, .data$weeks, \(y, n) status_of(paste0(y, prefix, n + 1))
      ),
      agrees = .data$last_status == 200 & .data$past_status == 409
    )

  if (!all(out$agrees)) {
    bad <- out$year[!out$agrees]
    cli::cli_abort(c(
      "The server's {period_type} calendar differs in {.val {bad}}.",
      "i" = "Check the server's weekly calendar before downloading
             weekly data.",
      "i" = "A 401 or 403 means the account cannot read {.val {dx}}."
    ))
  }
  cli::cli_alert_success(
    "{period_type} calendar agrees with the server for {length(years)}
     year{?s}"
  )
  out
}
