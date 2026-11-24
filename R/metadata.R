#' Build a metadata filter
#'
#' Creates one `filter` value in the API format,
#' `field:operator:value`. A vector value with `in` or `!in` becomes a
#' bracketed list. Pass several filters to [dhis2_metadata()] as a vector.
#'
#' @param field Property to filter on; nested properties use dots, such as
#'   `"parent.id"`.
#' @param operator API operator, such as `"eq"`, `"ilike"`, `"in"`, `"gt"`
#'   or `"null"`.
#' @param value Value to compare with; leave missing for operators that take
#'   none, such as `"null"`.
#' @return A string.
#' @examples
#' dhis2API::dhis2_filter("level", "eq", 3)
#' dhis2API::dhis2_filter("id", "in", c("a1", "b2"))
#' dhis2API::dhis2_filter("code", "null")
#' @export
dhis2_filter <- function(field, operator = "eq", value) {
  if (missing(value)) {
    return(paste(field, operator, sep = ":"))
  }
  value <- if (operator %in% c("in", "!in")) {
    paste0("[", paste(value, collapse = ","), "]")
  } else {
    as.character(value)
  }
  paste(field, operator, value, sep = ":")
}

#' Read a metadata collection
#'
#' Pages through `api/<resource>` and returns the records as a tibble.
#' When `cache_dir` is set, results are saved locally and reused for up to
#' `max_age` seconds. Each server, account, resource and query has a
#' separate saved result, so two accounts on one server never read each
#' other's cached metadata. This cache stores metadata only.
#'
#' @param resource Collection name, such as `"organisationUnits"`.
#' @param fields Fields to return, in the API's field syntax.
#' @param filter Filters from [dhis2_filter()].
#' @param root_junction `"AND"` to match every filter, `"OR"` to match any.
#' @param ... Further query parameters.
#' @param cache Use the cache when the `cache_dir` setting is set.
#' @param max_age Seconds a cached file stays valid; defaults to the
#'   `cache_max_age` setting.
#' @param session A session from [dhis2_session()].
#' @return A tibble.
#' @export
dhis2_metadata <- function(resource, fields = "id,name", filter = NULL,
                           root_junction = c("AND", "OR"), ...,
                           cache = TRUE, max_age = NULL, session = NULL) {
  session <- .dhis2_session(session)
  root_junction <- rlang::arg_match(root_junction)
  query <- list(
    fields = fields,
    filter = filter,
    rootJunction = if (root_junction == "OR") "OR" else NULL,
    ...
  )

  fetch <- function() {
    rlang::exec(
      dhis2_get_all,
      path = paste0("api/", resource, ".json"),
      collection = resource,
      !!!query,
      session = session
    )
  }

  dir <- .dhis2_setting(session, "cache_dir")
  if (!isTRUE(cache) || is.null(dir)) {
    return(fetch())
  }

  key <- rlang::hash(list(.dhis2_session_identity(session), resource, query))
  file <- fs::path(dir, paste0(resource, "_", key, ".qs2"))
  max_age <- max_age %||% .dhis2_setting(session, "cache_max_age")

  if (fs::file_exists(file)) {
    age <- as.numeric(difftime(
      Sys.time(), fs::file_info(file)$modification_time, units = "secs"
    ))
    if (age < max_age) {
      return(qs2::qs_read(file))
    }
  }

  out <- fetch()
  fs::dir_create(dir)
  qs2::qs_save(out, file)
  out
}

# The server, API version and account of a session, with no secret in it.
# A token is represented by a shortened hash so a changed token is a
# different identity. Used to separate cached metadata and resumable
# downloads made with different servers or accounts.
.dhis2_session_identity <- function(session) {
  account <- if (session$auth == "basic") {
    paste0("user:", session$username)
  } else {
    paste0("token:", substr(rlang::hash(c("dhis2API", session$token)), 1, 16))
  }
  list(
    base_url = session$base_url,
    api_version = session$api_version,
    account = account
  )
}

#' Remove the metadata cache
#'
#' Removes every cached file in the `cache_dir` setting of `session`, of
#' the default session when `session` is `NULL`, or of the R option or
#' package default when no default session is set. No connection to the
#' server is needed.
#'
#' @inheritParams dhis2_metadata
#' @return The number of files removed, invisibly.
#' @export
dhis2_cache_clear <- function(session = NULL) {
  session <- session %||% dhis2_get_session()
  dir <- .dhis2_setting(session, "cache_dir")
  if (is.null(dir) || !fs::dir_exists(dir)) {
    return(invisible(0L))
  }
  files <- fs::dir_ls(dir, glob = "*.qs2")
  fs::file_delete(files)
  invisible(length(files))
}

# a column of a data frame, or NA when the server left the field out of
# every record; jsonlite drops a field no record carries
.dhis2_col <- function(df, name, type = NA_character_) {
  if (is.null(df) || !name %in% names(df)) {
    return(rep(type, NROW(df)))
  }
  df[[name]]
}

#' Organisation unit levels
#'
#' @inheritParams dhis2_metadata
#' @return A tibble with `level`, `level_name` and `level_uid`.
#' @export
dhis2_org_unit_levels <- function(cache = TRUE, session = NULL) {
  raw <- dhis2_metadata(
    "organisationUnitLevels", fields = "id,level,name",
    cache = cache, session = session
  )
  tibble::tibble(
    level = as.integer(raw$level),
    level_name = raw$name,
    level_uid = raw$id
  ) |>
    dplyr::arrange(level)
}

#' Organisation units and their hierarchy
#'
#' Returns one row per organisation unit. With `ancestors = TRUE`, each
#' row includes the UID and name of the unit at each level above it, and
#' of the unit itself, as `level<n>_uid` and `level<n>_name`. For example,
#' a facility row can include its district and province.
#'
#' @param level Levels to keep, as numbers; all when `NULL`.
#' @param root UID of an organisation unit. Keep this unit and all units
#'   below it in the hierarchy.
#' @param ancestors Add UID and name columns for each level in the hierarchy.
#' @inheritParams dhis2_metadata
#' @return A tibble with `ou_uid`, `ou_name`, `ou_short_name`, `ou_code`,
#'   `ou_level`, `ou_level_name`, `parent_uid`, `parent_name`, `path`,
#'   `opening_date`, `closed_date` and, with `ancestors`, the level columns.
#' @export
dhis2_org_units <- function(level = NULL, root = NULL, ancestors = TRUE,
                            cache = TRUE, session = NULL) {
  filter <- c(
    if (!is.null(level)) dhis2_filter("level", "in", level),
    if (!is.null(root)) dhis2_filter("path", "like", root)
  )
  fields <- paste(
    "id,name,shortName,code,level,path,openingDate,closedDate",
    "parent[id,name]",
    if (ancestors) "ancestors[id,name,level]",
    sep = ","
  )
  raw <- dhis2_metadata(
    "organisationUnits", fields = fields, filter = filter,
    cache = cache, session = session
  )
  levels <- dhis2_org_unit_levels(cache = cache, session = session)

  units <- tibble::tibble(
    ou_uid = raw$id,
    ou_name = raw$name,
    ou_short_name = .dhis2_col(raw, "shortName"),
    ou_code = .dhis2_col(raw, "code"),
    ou_level = as.integer(raw$level),
    parent_uid = .dhis2_col(raw$parent, "id"),
    parent_name = .dhis2_col(raw$parent, "name"),
    path = raw$path,
    opening_date = as.Date(substr(.dhis2_col(raw, "openingDate"), 1, 10)),
    closed_date = as.Date(substr(.dhis2_col(raw, "closedDate"), 1, 10))
  ) |>
    dplyr::left_join(
      dplyr::select(levels, ou_level = level, ou_level_name = level_name),
      by = "ou_level"
    ) |>
    dplyr::relocate("ou_level_name", .after = "ou_level")

  if (!ancestors || nrow(units) == 0) {
    return(units)
  }

  above <- tibble::tibble(ou_uid = raw$id, ancestors = raw$ancestors) |>
    tidyr::unnest(ancestors, keep_empty = FALSE)
  self <- dplyr::transmute(raw, ou_uid = id, id, name, level)
  wide <- dplyr::bind_rows(above, self) |>
    dplyr::transmute(
      ou_uid,
      level = as.integer(level),
      uid = id,
      name
    ) |>
    dplyr::arrange(level) |>
    tidyr::pivot_wider(
      id_cols = "ou_uid",
      names_from = "level",
      values_from = c("uid", "name"),
      names_glue = "level{level}_{.value}",
      names_vary = "slowest"
    )

  dplyr::left_join(units, wide, by = "ou_uid")
}

#' Data elements
#'
#' @inheritParams dhis2_metadata
#' @return A tibble with `de_uid`, `de_name`, `de_short_name`, `de_code`,
#'   `value_type`, `aggregation_type`, `domain_type`, `zero_significant`,
#'   `cat_combo_uid` and `cat_combo_name`.
#' @export
dhis2_data_elements <- function(cache = TRUE, session = NULL) {
  raw <- dhis2_metadata(
    "dataElements",
    fields = paste(
      "id,name,shortName,code,valueType,aggregationType,domainType",
      "zeroIsSignificant,categoryCombo[id,name]", sep = ","
    ),
    cache = cache, session = session
  )
  tibble::tibble(
    de_uid = raw$id,
    de_name = raw$name,
    de_short_name = .dhis2_col(raw, "shortName"),
    de_code = .dhis2_col(raw, "code"),
    value_type = .dhis2_col(raw, "valueType"),
    aggregation_type = .dhis2_col(raw, "aggregationType"),
    domain_type = .dhis2_col(raw, "domainType"),
    zero_significant = .dhis2_col(raw, "zeroIsSignificant", NA),
    cat_combo_uid = .dhis2_col(raw$categoryCombo, "id"),
    cat_combo_name = .dhis2_col(raw$categoryCombo, "name")
  )
}

#' Data sets
#'
#' @inheritParams dhis2_metadata
#' @return A tibble with `ds_uid`, `ds_name`, `ds_code`, `period_type`,
#'   `cat_combo_uid` and `data_elements`, a list of data element UIDs.
#' @export
dhis2_data_sets <- function(cache = TRUE, session = NULL) {
  raw <- dhis2_metadata(
    "dataSets",
    fields = paste(
      "id,name,code,periodType,categoryCombo[id]",
      "dataSetElements[dataElement[id]]", sep = ","
    ),
    cache = cache, session = session
  )
  tibble::tibble(
    ds_uid = raw$id,
    ds_name = raw$name,
    ds_code = .dhis2_col(raw, "code"),
    period_type = .dhis2_col(raw, "periodType"),
    cat_combo_uid = .dhis2_col(raw$categoryCombo, "id"),
    data_elements = purrr::map(
      .dhis2_col(raw, "dataSetElements", list(NULL)),
      \(x) .dhis2_col(x$dataElement, "id")
    )
  )
}

#' Category option combinations
#'
#' These identify combinations of categories used to report a value,
#' such as an age group and sex.
#'
#' @inheritParams dhis2_metadata
#' @return A tibble with `coc_uid`, `coc_name`, `cat_combo_uid` and
#'   `cat_combo_name`.
#' @export
dhis2_category_option_combos <- function(cache = TRUE, session = NULL) {
  raw <- dhis2_metadata(
    "categoryOptionCombos", fields = "id,name,categoryCombo[id,name]",
    cache = cache, session = session
  )
  tibble::tibble(
    coc_uid = raw$id,
    coc_name = raw$name,
    cat_combo_uid = .dhis2_col(raw$categoryCombo, "id"),
    cat_combo_name = .dhis2_col(raw$categoryCombo, "name")
  )
}

#' Indicators, with their formulas
#'
#' @inheritParams dhis2_metadata
#' @return A tibble with `ind_uid`, `ind_name`, `ind_code`, `numerator`,
#'   `denominator`, `annualized`, `indicator_type` and `factor`.
#' @export
dhis2_indicators <- function(cache = TRUE, session = NULL) {
  raw <- dhis2_metadata(
    "indicators",
    fields = paste(
      "id,name,code,annualized,numerator,denominator",
      "indicatorType[name,factor]", sep = ","
    ),
    cache = cache, session = session
  )
  tibble::tibble(
    ind_uid = raw$id,
    ind_name = raw$name,
    ind_code = .dhis2_col(raw, "code"),
    numerator = .dhis2_col(raw, "numerator"),
    denominator = .dhis2_col(raw, "denominator"),
    annualized = .dhis2_col(raw, "annualized", NA),
    indicator_type = .dhis2_col(raw$indicatorType, "name"),
    factor = .dhis2_col(raw$indicatorType, "factor", NA_real_)
  )
}

#' Data element UIDs in a data set or group
#'
#' Returns the current list of data element UIDs from the server.
#'
#' @param uid UID of the data set or data element group.
#' @inheritParams dhis2_request
#' @return A character vector of data element UIDs.
#' @export
dhis2_data_set_elements <- function(uid, session = NULL) {
  res <- dhis2_get(
    paste0("api/dataSets/", uid, ".json"),
    fields = "dataSetElements[dataElement[id]]",
    simplify = TRUE, session = session
  )
  .dhis2_require_elements(res$dataSetElements$dataElement$id, uid)
}

#' @rdname dhis2_data_set_elements
#' @export
dhis2_group_elements <- function(uid, session = NULL) {
  res <- dhis2_get(
    paste0("api/dataElementGroups/", uid, ".json"),
    fields = "dataElements[id]",
    simplify = TRUE, session = session
  )
  .dhis2_require_elements(res$dataElements$id, uid)
}

.dhis2_require_elements <- function(ids, uid) {
  if (length(ids) == 0) {
    cli::cli_abort("{.val {uid}} returned no data elements.")
  }
  ids
}

#' Add names beside UID columns
#'
#' Adds `<column>_name` beside each UID column found: `data_element` or
#' `dx`, `org_unit` or `ou`, and `cat_option_combo` or `co`. If a lookup
#' table is not supplied, it is read from the server or metadata cache.
#'
#' @param data A tibble of values.
#' @param elements Output of [dhis2_data_elements()]. An analytics `dx`
#'   column can also hold indicators; their names are returned as `NA`.
#' @param org_units Output of [dhis2_org_units()].
#' @param combos Output of [dhis2_category_option_combos()].
#' @inheritParams dhis2_request
#' @return `data` with the name columns added.
#' @export
dhis2_add_names <- function(data, elements = NULL, org_units = NULL,
                            combos = NULL, session = NULL) {
  has <- function(cols) intersect(cols, names(data))

  join_names <- function(data, cols, lookup, uid, name) {
    purrr::reduce(cols, function(acc, col) {
      key <- dplyr::select(lookup, dplyr::all_of(c(uid, name)))
      names(key) <- c(col, paste0(col, "_name"))
      acc |>
        dplyr::left_join(key, by = col) |>
        dplyr::relocate(
          dplyr::all_of(paste0(col, "_name")), .after = dplyr::all_of(col)
        )
    }, .init = data)
  }

  de_cols <- has(c("data_element", "dx"))
  if (length(de_cols) > 0) {
    elements <- elements %||% dhis2_data_elements(session = session)
    data <- join_names(data, de_cols, elements, "de_uid", "de_name")
  }
  ou_cols <- has(c("org_unit", "ou"))
  if (length(ou_cols) > 0) {
    org_units <- org_units %||%
      dhis2_org_units(ancestors = FALSE, session = session)
    data <- join_names(data, ou_cols, org_units, "ou_uid", "ou_name")
  }
  co_cols <- has(c("cat_option_combo", "co"))
  if (length(co_cols) > 0) {
    combos <- combos %||% dhis2_category_option_combos(session = session)
    data <- join_names(data, co_cols, combos, "coc_uid", "coc_name")
  }
  data
}
