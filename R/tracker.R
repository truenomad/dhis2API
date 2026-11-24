#' Read tracker data
#'
#' Reads events, enrollments, tracked entities or relationships from the
#' tracker API (`api/tracker/...`), one page at a time. Requires DHIS2 2.36
#' or later.
#'
#' Selects organisation unit parameter names according to the resource
#' and server version. Downloads pages until one contains fewer records
#' than `page_size`.
#'
#' Tracker records can contain personal information. Request only the
#' `fields` needed for your analysis and avoid printing personal records
#' in logs.
#'
#' @param resource `"events"`, `"enrollments"`, `"trackedEntities"` or
#'   `"relationships"`.
#' @param program Program UID.
#' @param org_unit Organisation unit UIDs.
#' @param ou_mode `"DESCENDANTS"` to include every unit below `org_unit`,
#'   `"SELECTED"` for the units alone, or `"ACCESSIBLE"` for units the
#'   account can access.
#' @param fields Fields to return.
#' @param updated_after Return only records changed since this date or time.
#' @param ... Further query parameters, such as `programStage`,
#'   `occurredAfter` or `status`.
#' @param page_size Records per page.
#' @param progress Report each page.
#' @inheritParams dhis2_request
#' @return A tibble, with nested fields as list or data frame columns.
#' @export
dhis2_tracker <- function(resource = c(
                            "events", "enrollments", "trackedEntities",
                            "relationships"
                          ),
                          program = NULL, org_unit = NULL,
                          ou_mode = c("DESCENDANTS", "SELECTED", "ACCESSIBLE"),
                          fields = "*", updated_after = NULL, ...,
                          page_size = NULL, progress = TRUE, session = NULL) {
  session <- .dhis2_session(session)
  resource <- rlang::arg_match(resource)
  ou_mode <- rlang::arg_match(ou_mode)
  if (resource == "relationships" && !is.null(org_unit)) {
    cli::cli_abort(
      "Relationships are read by {.field trackedEntity}, {.field enrollment}
       or {.field event}, not by org unit; pass one of those in {.arg ...}."
    )
  }
  .dhis2_require_version(session, "2.36", "The tracker API")
  page_size <- page_size %||% .dhis2_setting(session, "page_size")
  names <- .dhis2_tracker_ou_params(
    resource, dhis2_server_version(session = session)
  )
  query <- list(
    program = program,
    fields = fields,
    updatedAfter = if (is.null(updated_after)) NULL else {
      format(updated_after, "%Y-%m-%dT%H:%M:%S")
    },
    ...
  )
  if (!is.null(org_unit)) {
    query[[names$unit]] <- paste(org_unit, collapse = ";")
    query[[names$mode]] <- ou_mode
  }

  pages <- list()
  page <- 1
  repeat {
    res <- rlang::exec(
      dhis2_get, paste0("api/tracker/", resource, ".json"),
      !!!query, page = page, pageSize = page_size,
      simplify = TRUE, session = session
    )
    rows <- res[[resource]] %||% res$instances
    n <- NROW(rows)
    if (n == 0) {
      break
    }
    pages[[page]] <- tibble::as_tibble(rows)
    if (isTRUE(progress)) {
      cli::cli_alert_info("{resource}: page {page}, {n} record{?s}")
    }
    if (n < page_size) {
      break
    }
    page <- page + 1
  }
  dplyr::bind_rows(pages)
}

# The org unit parameters each resource takes. Measured on the 2.41 to 2.44
# demo servers: from 2.42 events ignore `ouMode` and return no rows without
# an error, and enrollments refuse `orgUnit` with `orgUnitMode`. Before 2.41
# the older names are used; no server of those versions was available.
.dhis2_tracker_ou_params <- function(resource, version) {
  if (version < "2.41") {
    return(list(unit = "orgUnit", mode = "ouMode"))
  }
  if (resource == "events") {
    return(list(unit = "orgUnit", mode = "orgUnitMode"))
  }
  list(unit = "orgUnits", mode = "orgUnitMode")
}

#' Data values of tracker events in long form
#'
#' One row per event and data element, from the `dataValues` column of
#' [dhis2_tracker()] output for events.
#'
#' @param events Output of `dhis2_tracker("events")` with `event` and
#'   `dataValues` among its fields. A result with no events, or whose
#'   events all have empty `dataValues`, gives zero rows.
#' @return A tibble with `event`, `data_element` and `value`, all
#'   character.
#' @export
dhis2_event_values <- function(events) {
  empty <- tibble::tibble(
    event = character(), data_element = character(), value = character()
  )
  if (nrow(events) == 0) {
    return(empty)
  }
  if (!all(c("event", "dataValues") %in% names(events))) {
    cli::cli_abort(
      "{.arg events} needs the {.field event} and {.field dataValues}
       fields."
    )
  }
  # an event without values holds an empty list, which cannot be combined
  # with the data frames of the other events
  long <- events |>
    dplyr::select("event", "dataValues") |>
    dplyr::mutate(dataValues = purrr::map(
      .data$dataValues, \(x) if (length(x) == 0) NULL else x
    )) |>
    tidyr::unnest("dataValues", keep_empty = FALSE)
  if (nrow(long) == 0) {
    return(empty)
  }
  if (!all(c("dataElement", "value") %in% names(long))) {
    cli::cli_abort(
      "{.field dataValues} needs {.field dataElement} and {.field value}."
    )
  }
  dplyr::transmute(
    long,
    event = as.character(.data$event),
    data_element = as.character(.data$dataElement),
    value = as.character(.data$value)
  )
}
