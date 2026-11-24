#' Read raw data values
#'
#' Downloads raw values for one or more organisation units over a date
#' range using `dataValueSets`. Returns columns matching the database's
#' `data_values` table. Use [dhis2_sync_group()] for larger downloads that
#' need to resume after an interruption.
#'
#' @param org_unit Organisation unit UIDs.
#' @param start_date,end_date First and last dates to include.
#' @param data_set,data_element_group UIDs selecting the values; give at
#'   least one.
#' @param children Include the units below each organisation unit. Otherwise,
#'   server returns the unit's own values only.
#' @param include_deleted Include deleted values, flagged in `deleted`.
#'   Include these when updating a local copy so that server deletions
#'   are recorded locally.
#' @param last_updated_duration Only values changed in this span, such as
#'   `"8d"` for the last eight days.
#' @param format `"csv"` for a smaller download without creation dates,
#'   or `"json"` to include creation dates in the `created` column.
#' @inheritParams dhis2_request
#' @return A tibble with the `data_values` columns.
#' @export
dhis2_data_values <- function(org_unit, start_date, end_date,
                              data_set = NULL, data_element_group = NULL,
                              children = TRUE, include_deleted = TRUE,
                              last_updated_duration = NULL,
                              format = c("csv", "json"), session = NULL) {
  session <- .dhis2_session(session)
  format <- rlang::arg_match(format)
  if (is.null(data_set) && is.null(data_element_group)) {
    cli::cli_abort(
      "Give {.arg data_set} or {.arg data_element_group}, or both."
    )
  }

  query <- list(
    dataSet = data_set,
    dataElementGroup = data_element_group,
    orgUnit = org_unit,
    children = if (isTRUE(children)) "true" else "false",
    startDate = format(as.Date(start_date)),
    endDate = format(as.Date(end_date)),
    includeDeleted = if (isTRUE(include_deleted)) "true" else "false",
    lastUpdatedDuration = last_updated_duration
  )

  if (format == "json") {
    res <- rlang::exec(
      dhis2_get, "api/dataValueSets.json", !!!query,
      simplify = FALSE, session = session
    )
    return(.dhis2_values_from_json(res$dataValues))
  }

  req <- rlang::exec(
    dhis2_request, "api/dataValueSets.csv", !!!query, session = session
  )
  resp <- httr2::req_perform(req)
  .dhis2_stop_for_status(resp, session)
  if (!httr2::resp_has_body(resp)) {
    return(.dhis2_empty("data_values"))
  }
  .dhis2_stop_for_html(resp, "api/dataValueSets.csv")
  .dhis2_values_from_csv(httr2::resp_body_string(resp))
}

.dhis2_values_from_json <- function(values) {
  if (length(values) == 0) {
    return(.dhis2_empty("data_values"))
  }
  chr <- function(field) {
    purrr::map_chr(values, \(x) as.character(x[[field]] %||% NA_character_))
  }
  lgl <- function(field) {
    purrr::map_lgl(values, \(x) as.logical(x[[field]] %||% NA))
  }
  tibble::tibble(
    data_element = chr("dataElement"),
    period = chr("period"),
    org_unit = chr("orgUnit"),
    cat_option_combo = chr("categoryOptionCombo"),
    attr_option_combo = chr("attributeOptionCombo"),
    value = chr("value"),
    stored_by = chr("storedBy"),
    created = chr("created"),
    last_updated = chr("lastUpdated"),
    comment = chr("comment"),
    followup = lgl("followup"),
    deleted = dplyr::coalesce(lgl("deleted"), FALSE)
  )
}

# column names vary by server version, so each field has its known spellings
.dhis2_values_from_csv <- function(text) {
  if (!nzchar(trimws(text))) {
    return(.dhis2_empty("data_values"))
  }
  raw <- readr::read_csv(
    I(text),
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE,
    show_col_types = FALSE
  )
  names(raw) <- tolower(names(raw))
  # a header this parser does not know would otherwise give empty columns
  core <- c("dataelement", "period", "orgunit", "value")
  absent <- setdiff(core, names(raw))
  if (length(absent) > 0) {
    cli::cli_abort(c(
      "The CSV from this server lacks {.field {absent}}.",
      "i" = "Its columns are {.field {names(raw)}}.",
      "i" = "Use {.code format = \"json\"} on this server."
    ))
  }
  if (nrow(raw) == 0) {
    return(.dhis2_empty("data_values"))
  }
  pick <- function(...) {
    hit <- intersect(c(...), names(raw))
    if (length(hit) == 0) rep(NA_character_, nrow(raw)) else raw[[hit[[1]]]]
  }
  tibble::tibble(
    data_element = pick("dataelement"),
    period = pick("period"),
    org_unit = pick("orgunit"),
    cat_option_combo = pick("categoryoptioncombo", "catoptcombo"),
    attr_option_combo = pick("attributeoptioncombo", "attroptcombo"),
    value = pick("value"),
    stored_by = pick("storedby"),
    created = pick("created"),
    last_updated = pick("lastupdated"),
    comment = pick("comment"),
    followup = tolower(pick("followup")) %in% "true",
    deleted = tolower(pick("deleted")) %in% "true"
  )
}
