#' Build an analytics dimension
#'
#' @param dimension Dimension identifier: `"dx"` for data items, `"pe"` for
#'   periods, `"ou"` for organisation units, `"co"` for category option
#'   combinations, or the UID of a category or group set.
#' @param items Items to select; the whole dimension when `NULL`.
#' @return A string such as `"dx:a1;b2"`.
#' @examples
#' dhis2API::dhis2_dimension("ou", c("LEVEL-3", "abc123"))
#' dhis2API::dhis2_dimension("co")
#' @export
dhis2_dimension <- function(dimension, items = NULL) {
  if (is.null(items)) {
    return(dimension)
  }
  paste0(dimension, ":", paste(items, collapse = ";"))
}

#' Read analytics
#'
#' Reads values from the server's analytics tables. These tables contain
#' data already aggregated by period and organisation unit.
#'
#' Use `dx_per_request`, `pe_per_request` and `ou_per_request` to split a
#' large download into smaller requests. If a request fails, its periods
#' are split into two smaller requests and retried, up to three levels of
#' splitting. Authentication and permission errors stop the download.
#' Requests that still fail produce a warning, and their data is omitted
#' from the result.
#'
#' Names are omitted by default to reduce the response size and server
#' processing time. Add them afterwards with [dhis2_add_names()].
#'
#' @param dx Data element UIDs, indicator UIDs or data set reporting items.
#' @param pe Period identifiers, such as from [dhis2_monthly_periods()].
#' @param ou Organisation unit selections, such as `"LEVEL-5"`, a UID or
#'   `"USER_ORGUNIT"`.
#' @param co Return a separate value for each category option combination,
#'   such as an age and sex group. When `FALSE`, the server combines values
#'   across these categories.
#' @param dimensions Further dimensions from [dhis2_dimension()].
#' @param filters Filters, written like dimensions.
#' @param dx_per_request,pe_per_request,ou_per_request Items per request
#'   for each dimension; `NULL` sends them all in one.
#' @param skip_meta Leave the name metadata out of the response.
#' @param ... Further query parameters.
#' @inheritParams dhis2_request
#' @return A tibble with one column per dimension and `value`, all
#'   stored as character columns; convert `value` with `as.numeric()`.
#' @export
dhis2_analytics <- function(dx, pe, ou, co = TRUE, dimensions = NULL,
                            filters = NULL, dx_per_request = NULL,
                            pe_per_request = NULL, ou_per_request = NULL,
                            skip_meta = TRUE, ..., session = NULL) {
  session <- .dhis2_session(session)
  grid <- tidyr::expand_grid(
    dx = .dhis2_chunk(dx, dx_per_request),
    pe = .dhis2_chunk(pe, pe_per_request),
    ou = .dhis2_chunk(ou, ou_per_request)
  )

  slices <- purrr::pmap(grid, function(dx, pe, ou) {
    .dhis2_analytics_slice(
      dx, pe, ou, co = co, dimensions = dimensions, filters = filters,
      skip_meta = skip_meta, ..., session = session
    )
  })

  failed <- purrr::keep(slices, \(s) isTRUE(s$failed))
  if (length(failed) > 0) {
    cli::cli_warn(c(
      "{length(failed)} of {length(slices)} analytics request{?s} failed
       and {?is/are} missing from the result.",
      "x" = "{failed[[1]]$message}"
    ))
  }

  slices |>
    purrr::map("data") |>
    purrr::list_rbind() |>
    tibble::as_tibble()
}

# one request, parsed to a tibble of character columns
.dhis2_analytics_fetch <- function(dx, pe, ou, co = TRUE, dimensions = NULL,
                                   filters = NULL, skip_meta = TRUE, ...,
                                   session = NULL) {
  res <- dhis2_get(
    "api/analytics.json",
    dimension = c(
      dhis2_dimension("dx", dx),
      dhis2_dimension("pe", pe),
      dhis2_dimension("ou", ou),
      if (isTRUE(co)) "co",
      dimensions
    ),
    filter = filters,
    skipMeta = if (isTRUE(skip_meta)) "true" else NULL,
    ...,
    simplify = FALSE,
    session = session
  )
  .dhis2_analytics_parse(res)
}

# headers name the columns; rows are lists of strings
.dhis2_analytics_parse <- function(res) {
  cols <- purrr::map_chr(res$headers, "name")
  if (length(res$rows) == 0) {
    return(tibble::as_tibble(
      rlang::set_names(rep(list(character()), length(cols)), cols)
    ))
  }
  res$rows |>
    purrr::map(\(row) as.character(unlist(row))) |>
    purrr::list_transpose(simplify = TRUE) |>
    rlang::set_names(cols) |>
    tibble::as_tibble()
}

# Stop on authentication and permission errors. Retrying smaller requests
# cannot resolve them.
.dhis2_analytics_fatal <- function(err) {
  status <- err$status %||% NA_integer_
  msg <- conditionMessage(err)
  isTRUE(status %in% c(401, 403)) ||
    grepl("not allowed|not authori[sz]ed|forbidden", msg, ignore.case = TRUE)
}

# Fetch a set of elements and periods. Split the periods and retry on failure.
.dhis2_analytics_slice <- function(dx, pe, ou, ..., depth = 0L,
                                   max_depth = 3L) {
  out <- tryCatch(
    .dhis2_analytics_fetch(dx, pe, ou, ...),
    error = function(e) e
  )
  if (!inherits(out, "error")) {
    return(list(data = out, failed = FALSE))
  }
  if (.dhis2_analytics_fatal(out)) {
    cli::cli_abort(
      c(
        "Analytics refused the request for {.val {ou}}.",
        "i" = "A smaller request cannot help, so the run stops here."
      ),
      parent = out
    )
  }
  if (length(pe) == 1L || depth >= max_depth) {
    message <- gsub("\\s+", " ", substr(conditionMessage(out), 1, 180))
    return(list(data = NULL, failed = TRUE, message = message))
  }
  halves <- .dhis2_chunk(pe, ceiling(length(pe) / 2))
  parts <- purrr::map(halves, function(p) {
    .dhis2_analytics_slice(
      dx, p, ou, ..., depth = depth + 1L, max_depth = max_depth
    )
  })
  failed <- purrr::keep(parts, \(p) isTRUE(p$failed))
  list(
    data = purrr::list_rbind(purrr::map(parts, "data")),
    failed = length(failed) > 0,
    message = if (length(failed) > 0) failed[[1]]$message else NULL
  )
}

# The organisation units the logged-in account is assigned to.
.dhis2_user_units <- function(session) {
  me <- dhis2_get(
    "api/me.json", fields = "organisationUnits[id]",
    simplify = TRUE, session = session
  )
  as.character(.dhis2_col(me$organisationUnits, "id"))
}

# The organisation units whose data the account may view: the server's
# `userDataViewFallback` selection, which is the account's data view units
# or, when none are set, its data capture units. Empty when the server
# names none. Always read from the server: this decides which rows may be
# removed, so a cached copy of earlier, wider permissions must not be used.
.dhis2_data_view_units <- function(session) {
  raw <- dhis2_metadata(
    "organisationUnits", fields = "id", userDataViewFallback = "true",
    cache = FALSE, session = session
  )
  as.character(.dhis2_col(raw, "id"))
}

# The organisation units an analytics `ou` selection can return rows for.
#
# Items follow the analytics API: UIDs; USER_ORGUNIT, USER_ORGUNIT_CHILDREN
# and USER_ORGUNIT_GRANDCHILDREN, read from `api/me`; LEVEL-<n>,
# LEVEL-<level uid> and OU_GROUP-<uid>, resolved from the organisation unit
# metadata within the UIDs and user units given alongside them (or the
# whole hierarchy when there are none); and LEVEL-<n>-<uid>, a level under
# one boundary unit. Level and group items are further limited to the
# units under the account's data view units, since metadata can show units
# whose data the account cannot read. Gives NULL for an item it does not
# recognise, and stops when the data view units cannot be read.
.dhis2_analytics_units <- function(ou, org_units = NULL, session = NULL) {
  items <- unique(unlist(strsplit(ou, ";", fixed = TRUE)))
  level_re <- "^LEVEL-([A-Za-z0-9_]+)(-([A-Za-z0-9_]+))?$"
  is_level <- grepl(level_re, items)
  is_group <- grepl("^OU_GROUP-[A-Za-z0-9]+$", items)
  user_items <- c(
    "USER_ORGUNIT", "USER_ORGUNIT_CHILDREN", "USER_ORGUNIT_GRANDCHILDREN"
  )
  is_user <- items %in% user_items
  is_uid <- !is_level & !is_group & !startsWith(items, "USER_ORGUNIT") &
    grepl("^[A-Za-z][A-Za-z0-9_]*$", items)
  if (!all(is_uid | is_level | is_group | is_user)) {
    return(NULL)
  }

  uids <- items[is_uid]
  roots <- if (any(is_user)) .dhis2_user_units(session) else character()
  direct <- c(uids, if ("USER_ORGUNIT" %in% items) roots)
  descend <- intersect(items, user_items[-1])
  if (!any(is_level | is_group) && length(descend) == 0) {
    return(unique(direct))
  }

  org_units <- org_units %||%
    dhis2_org_units(ancestors = FALSE, session = session)
  view <- character()
  if (any(is_level | is_group)) {
    view <- .dhis2_data_view_units(session)
    if (length(view) == 0) {
      cli::cli_abort(c(
        "The account's data view organisation units could not be read, so
         the units covered by {.val {ou}} cannot be established.",
        "i" = "Nothing was downloaded or changed."
      ))
    }
  }
  within <- function(units, bounds) {
    if (length(bounds) == 0) {
      return(units)
    }
    pattern <- paste0("/(", paste(bounds, collapse = "|"), ")(/|$)")
    dplyr::filter(units, grepl(pattern, .data$path))
  }
  below <- function(parents) {
    org_units |>
      dplyr::filter(.data$parent_uid %in% parents) |>
      dplyr::pull("ou_uid")
  }

  # the user's units, like literal UIDs, bound levels and groups when
  # those are present; otherwise they are returned themselves
  bounds <- c(uids, roots)
  found <- character()
  if (!any(is_level | is_group)) {
    found <- direct
  }
  children <- if (length(descend) > 0) below(roots) else character()
  if ("USER_ORGUNIT_CHILDREN" %in% descend) {
    found <- c(found, children)
  }
  if ("USER_ORGUNIT_GRANDCHILDREN" %in% descend) {
    found <- c(found, below(children))
  }

  level_of <- function(key) {
    if (grepl("^[0-9]+$", key)) {
      return(as.integer(key))
    }
    known <- dhis2_org_unit_levels(session = session)
    known$level[known$level_uid == key]
  }
  for (item in items[is_level]) {
    key <- sub(level_re, "\\1", item)
    own <- sub(level_re, "\\3", item)
    at_level <- org_units |>
      dplyr::filter(.data$ou_level %in% level_of(key)) |>
      within(if (nzchar(own)) own else bounds) |>
      within(view)
    found <- c(found, at_level$ou_uid)
  }

  if (any(is_group)) {
    group_uids <- sub("^OU_GROUP-", "", items[is_group])
    groups <- dhis2_metadata(
      "organisationUnitGroups", fields = "id,organisationUnits[id]",
      filter = dhis2_filter("id", "in", group_uids),
      session = session
    )
    member_ids <- purrr::list_c(purrr::map(
      .dhis2_col(groups, "organisationUnits", list(NULL)),
      \(x) as.character(.dhis2_col(x, "id"))
    ))
    members <- org_units |>
      dplyr::filter(.data$ou_uid %in% member_ids) |>
      within(bounds) |>
      within(view) |>
      dplyr::pull("ou_uid")
    found <- c(found, members)
  }
  unique(found)
}

#' Prepare analytics for the database
#'
#' `dhis2_analytics_to_store()` returns columns for the monthly
#' `analytics_values` table. `dhis2_analytics_to_subannual()` adds
#' `period_type` for the weekly or daily `analytics_subannual` table.
#' Keeping these tables separate avoids counting the same data twice.
#'
#' Analytics results do not include attribute option combinations or
#' fields such as who entered a value and when. They are stored separately
#' from raw values in `data_values`.
#'
#' @param data Output of [dhis2_analytics()] with `co = TRUE`.
#' @param period_type Period type of the rows, such as `"WeeklySunday"`.
#' @return A tibble in the table's column order.
#' @export
dhis2_analytics_to_store <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(.dhis2_empty("analytics_values"))
  }
  tibble::tibble(
    data_element = data$dx,
    period = data$pe,
    org_unit = data$ou,
    cat_option_combo = data$co,
    value = data$value
  )
}

#' @rdname dhis2_analytics_to_store
#' @export
dhis2_analytics_to_subannual <- function(data, period_type) {
  if (is.null(data) || nrow(data) == 0) {
    return(.dhis2_empty("analytics_subannual"))
  }
  tibble::tibble(
    data_element = data$dx,
    period = data$pe,
    period_type = period_type,
    org_unit = data$ou,
    cat_option_combo = data$co,
    value = data$value
  )
}
