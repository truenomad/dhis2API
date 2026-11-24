# Request settings. Each is read when a request is built, in the order: the
# session's own value, then the option `dhis2api.<name>`, then the default
# below, so an option set after a session opens still takes effect.
.dhis2_defaults <- list(
  # the proxy in front of the reference server closes an unanswered request
  # at about 60 s, so a longer timeout only delays the retry
  timeout = 120,
  max_tries = 4,
  retry_statuses = c(429, 500, 502, 503, 504),
  retry_on_failure = TRUE,
  # one request a second; sustained parallel export exhausts a small server
  requests_per_minute = 60,
  # curl's code for HTTP/1.1: dataValueSets fails with an HTTP/2 framing
  # error on the reference server, even on a one-month request
  http_version = 2,
  user_agent = "dhis2API (R httr2)",
  page_size = 1000,
  cache_dir = NULL,
  cache_max_age = 86400,
  workers = 6,
  delta_days = 8
)

#' Request settings and their defaults
#'
#' Every request reads its settings when it is built: first the value given
#' to [dhis2_session()], then the R option `dhis2api.<name>`, then the
#' package default. An option set after a session opens therefore still
#' applies.
#'
#' The settings are:
#'
#' * `timeout`: seconds to wait for a response.
#' * `max_tries`: maximum number of attempts per request.
#' * `retry_statuses`: HTTP status codes that trigger another attempt.
#' * `retry_on_failure`: retry a dropped connection or DNS failure.
#' * `requests_per_minute`: request rate limit per R process; `Inf` removes
#'   the limit.
#' * `http_version`: curl's `http_version` option. The default, `2`, is
#'   HTTP/1.1.
#' * `user_agent`: the user agent string.
#' * `page_size`: number of records per page of metadata or tracker results.
#' * `cache_dir`: folder for the metadata cache; `NULL` turns it off.
#' * `cache_max_age`: seconds a cached metadata file stays valid.
#' * `workers`: number of R processes for [dhis2_fetch_units()].
#' * `delta_days`: minimum number of days to check for changed values.
#'
#' @return A named list of the package defaults.
#' @examples
#' names(dhis2API::dhis2_defaults())
#' @export
dhis2_defaults <- function() {
  .dhis2_defaults
}

# one setting for a session, resolved when called
.dhis2_setting <- function(session, name) {
  if (!name %in% names(.dhis2_defaults)) {
    cli::cli_abort("Unknown setting {.val {name}}.")
  }
  own <- if (is.null(session)) NULL else session$settings[[name]]
  own %||%
    getOption(paste0("dhis2api.", name)) %||%
    .dhis2_defaults[[name]]
}

# an environment variable, NULL when unset or empty
.dhis2_env_var <- function(name) {
  value <- Sys.getenv(name)
  if (nzchar(value)) value else NULL
}
