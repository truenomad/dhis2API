#' Monthly period identifiers for a date range
#'
#' Returns identifiers such as `"202601"` for January 2026. Use these to
#' request specific months in [dhis2_analytics()].
#'
#' @param start_date,end_date First and last dates of the range.
#' @return A character vector of `YYYYMM` identifiers.
#' @examples
#' dhis2API::dhis2_monthly_periods(as.Date("2025-11-15"), as.Date("2026-02-01"))
#' @export
dhis2_monthly_periods <- function(start_date, end_date) {
  months <- seq(
    as.Date(format(as.Date(start_date), "%Y-%m-01")), as.Date(end_date),
    by = "month"
  )
  format(months, "%Y%m")
}

# weekly period types, the prefix of their ids, and the weekday (as %w) each
# week opens on
.dhis2_weekly_types <- tibble::tibble(
  period_type = c(
    "Weekly", "WeeklyWednesday", "WeeklyThursday", "WeeklySaturday",
    "WeeklySunday"
  ),
  prefix = c("W", "WedW", "ThuW", "SatW", "SunW"),
  weekday = c(1L, 3L, 4L, 6L, 0L)
)

.dhis2_weekly_type <- function(period_type) {
  row <- dplyr::filter(.dhis2_weekly_types, .data$period_type == !!period_type)
  if (nrow(row) != 1) {
    known <- .dhis2_weekly_types$period_type
    cli::cli_abort(c(
      "Unknown weekly period type {.val {period_type}}.",
      "i" = "Use one of {.val {known}}."
    ))
  }
  row
}

#' Weekly period calendar
#'
#' The first week of a year is the week containing 4 January, using the
#' start day for the selected period type. `dhis2_weeks_in_year()` returns
#' 52 or 53. An invalid week identifier can cause the server to reject an
#' entire analytics request with HTTP status 409.
#'
#' Use [dhis2_check_weekly_periods()] to check this calendar against your
#' server.
#'
#' @param year Integer years.
#' @param period Weekly period identifiers, such as `"2025SunW10"` or
#'   `"2025W10"`.
#' @param period_type One of `"Weekly"` (ISO, Monday), `"WeeklyWednesday"`,
#'   `"WeeklyThursday"`, `"WeeklySaturday"` or `"WeeklySunday"`.
#' @return `dhis2_week_year_start()` and `dhis2_weekly_period_start()` give
#'   dates; `dhis2_weeks_in_year()` gives integers.
#' @examples
#' dhis2API::dhis2_weeks_in_year(2020:2026, "WeeklySunday")
#' dhis2API::dhis2_weekly_period_start("2026SunW1")
#' @export
dhis2_week_year_start <- function(year, period_type = "WeeklySunday") {
  start_day <- .dhis2_weekly_type(period_type)$weekday
  jan4 <- as.Date(paste0(year, "-01-04"))
  jan4 - (as.integer(format(jan4, "%w")) - start_day) %% 7L
}

#' @rdname dhis2_week_year_start
#' @export
dhis2_weeks_in_year <- function(year, period_type = "WeeklySunday") {
  span <- dhis2_week_year_start(year + 1L, period_type) -
    dhis2_week_year_start(year, period_type)
  as.integer(span) %/% 7L
}

#' @rdname dhis2_week_year_start
#' @export
dhis2_weekly_period_start <- function(period) {
  prefix <- sub("^[0-9]{4}([A-Za-z]+)[0-9]+$", "\\1", period)
  types <- .dhis2_weekly_types$period_type[
    match(prefix, .dhis2_weekly_types$prefix)
  ]
  if (anyNA(types)) {
    cli::cli_abort("Not a weekly period id: {.val {period[is.na(types)]}}.")
  }
  year <- as.integer(substr(period, 1, 4))
  week <- as.integer(sub("^[0-9]{4}[A-Za-z]+", "", period))
  starts <- purrr::map2(
    year, types, \(y, t) dhis2_week_year_start(y, t)
  )
  purrr::list_c(starts) + (week - 1L) * 7L
}

#' Weekly period identifiers for a date range
#'
#' Returns weeks for every calendar year in the range, including only
#' weeks that have started by `end_date`. The result starts at week 1 of
#' the year containing `start_date`, even if that week begins in the previous
#' December. Weeks before `start_date` in that year are included.
#'
#' For large requests, use `pe_per_request` in [dhis2_analytics()] to limit
#' the number of periods per request.
#'
#' @inheritParams dhis2_monthly_periods
#' @inheritParams dhis2_week_year_start
#' @return A character vector of period identifiers.
#' @examples
#' dhis2API::dhis2_weekly_periods(as.Date("2026-01-01"), as.Date("2026-02-01"))
#' @export
dhis2_weekly_periods <- function(start_date, end_date,
                                 period_type = "WeeklySunday") {
  prefix <- .dhis2_weekly_type(period_type)$prefix
  years <- seq(
    as.integer(format(as.Date(start_date), "%Y")),
    as.integer(format(as.Date(end_date), "%Y"))
  )
  ids <- purrr::map(years, function(y) {
    paste0(y, prefix, seq_len(dhis2_weeks_in_year(y, period_type)))
  }) |>
    purrr::list_c()
  ids[dhis2_weekly_period_start(ids) <= as.Date(end_date)]
}

#' Split a date range by calendar year
#'
#' Returns one date range per calendar year, limited to the supplied
#' start and end dates. Use these ranges to split a large download.
#'
#' @inheritParams dhis2_monthly_periods
#' @return A list of date ranges, each with `label`, `start` and `end`.
#' @export
dhis2_period_chunks <- function(start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  years <- seq(
    as.integer(format(start_date, "%Y")), as.integer(format(end_date, "%Y"))
  )
  purrr::map(years, function(y) {
    list(
      label = as.character(y),
      start = max(start_date, as.Date(paste0(y, "-01-01"))),
      end = min(end_date, as.Date(paste0(y, "-12-31")))
    )
  })
}

# split a vector into pieces of at most `size`; NULL keeps it whole
.dhis2_chunk <- function(x, size = NULL) {
  if (length(x) == 0) {
    return(list())
  }
  if (is.null(size) || size >= length(x)) {
    return(list(x))
  }
  unname(split(x, ceiling(seq_along(x) / size)))
}
