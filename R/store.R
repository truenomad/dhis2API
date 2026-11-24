# Database tables and column types. Keep raw values, monthly analytics and
# weekly or daily analytics separate to avoid counting the same data twice.
.dhis2_store_schema <- list(
  data_values = c(
    data_element = "VARCHAR", period = "VARCHAR", org_unit = "VARCHAR",
    cat_option_combo = "VARCHAR", attr_option_combo = "VARCHAR",
    value = "VARCHAR", stored_by = "VARCHAR", created = "VARCHAR",
    last_updated = "VARCHAR", comment = "VARCHAR", followup = "BOOLEAN",
    deleted = "BOOLEAN"
  ),
  analytics_values = c(
    data_element = "VARCHAR", period = "VARCHAR", org_unit = "VARCHAR",
    cat_option_combo = "VARCHAR", value = "VARCHAR"
  ),
  analytics_subannual = c(
    data_element = "VARCHAR", period = "VARCHAR", period_type = "VARCHAR",
    org_unit = "VARCHAR", cat_option_combo = "VARCHAR", value = "VARCHAR"
  ),
  download_log = c(
    run_at = "VARCHAR", mode = "VARCHAR", element_group = "VARCHAR",
    org_unit = "VARCHAR", window_start = "VARCHAR", window_end = "VARCHAR",
    rows_returned = "INTEGER", selection = "VARCHAR"
  )
)

# Columns that uniquely identify a row when inserting or replacing values.
.dhis2_store_keys <- list(
  data_values = c(
    "data_element", "period", "org_unit", "cat_option_combo",
    "attr_option_combo"
  ),
  analytics_values = c(
    "data_element", "period", "org_unit", "cat_option_combo"
  ),
  analytics_subannual = c(
    "data_element", "period", "org_unit", "cat_option_combo"
  )
)

.dhis2_value_tables <- c(
  "data_values", "analytics_values", "analytics_subannual"
)

# A zero-row tibble with the table's column names and types.
.dhis2_empty <- function(table) {
  types <- .dhis2_store_schema[[table]]
  cols <- purrr::map(types, function(type) {
    switch(type,
      BOOLEAN = logical(),
      INTEGER = integer(),
      character()
    )
  })
  tibble::as_tibble(cols)
}

.dhis2_create_sql <- function(con, table) {
  types <- .dhis2_store_schema[[table]]
  cols <- paste(
    DBI::dbQuoteIdentifier(con, names(types)), types,
    collapse = ", "
  )
  keys <- .dhis2_store_keys[[table]]
  key <- if (is.null(keys)) "" else {
    paste0(
      ", PRIMARY KEY (",
      paste(DBI::dbQuoteIdentifier(con, keys), collapse = ", "), ")"
    )
  }
  paste0("CREATE TABLE IF NOT EXISTS ", table, " (", cols, key, ")")
}

# Add columns that a store created by an earlier version lacks. Existing
# rows get NULL in the new columns.
.dhis2_store_migrate <- function(con) {
  DBI::dbWithTransaction(con, {
    purrr::iwalk(.dhis2_store_schema, function(types, table) {
      have <- DBI::dbListFields(con, table)
      purrr::iwalk(types[setdiff(names(types), have)], function(type, col) {
        DBI::dbExecute(con, paste0(
          "ALTER TABLE ", table, " ADD COLUMN IF NOT EXISTS ",
          DBI::dbQuoteIdentifier(con, col), " ", type
        ))
      })
    })
  })
  invisible(con)
}

#' Open the local DuckDB store
#'
#' Creates the database file and tables if needed, and adds any columns
#' that a store created by an earlier version of the package lacks. Close
#' connections in other R sessions before opening the file for writing. Use
#' `read_only = TRUE` to read an existing database when no process is writing.
#'
#' The tables are `data_values`, `analytics_values`, `analytics_subannual`
#' and `download_log`. The value tables can contain the same data at
#' different levels of detail. Adding values across them would double count.
#'
#' @param path Path to the `.duckdb` file.
#' @param read_only Open without write access, and without creating
#'   anything.
#' @return A DBI connection. Close it with [dhis2_store_close()].
#' @export
dhis2_store_open <- function(path, read_only = FALSE) {
  if (read_only && !fs::file_exists(path)) {
    cli::cli_abort("No store at {.path {path}}.")
  }
  if (!read_only) {
    fs::dir_create(fs::path_dir(path))
    # a read-only connection kept by dhis2_store_tbl() would make this one
    # read-only too
    .dhis2_store_release(path)
  }

  con <- tryCatch(
    DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = read_only),
    error = function(e) {
      if (!grepl("lock", conditionMessage(e), ignore.case = TRUE)) {
        rlang::cnd_signal(e)
      }
      cli::cli_abort(c(
        "Another R session holds the store open for writing.",
        "i" = "Run {.code DBI::dbDisconnect(con, shutdown = TRUE)} there, or
               restart that session.",
        "i" = "{.path {path}}"
      ), parent = e)
    }
  )

  if (!read_only) {
    purrr::walk(names(.dhis2_store_schema), function(table) {
      DBI::dbExecute(con, .dhis2_create_sql(con, table))
    })
    .dhis2_store_migrate(con)
  }
  con
}

#' Close a store connection
#'
#' @param con A connection from [dhis2_store_open()].
#' @return `NULL`, invisibly.
#' @export
dhis2_store_close <- function(con) {
  if (DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
  invisible(NULL)
}

#' Copy the store for reading during a download
#'
#' DuckDB locks the file while a download writes to it, so analysis reads a
#' copy. The copy is made through a database connection, so it includes
#' every committed change, including changes not yet written from the
#' write-ahead log into the database file. Changes in an open transaction
#' are not included.
#'
#' When another process holds the store open for writing, the copy cannot be
#' made and the function stops with an error. An existing file at `dest` is
#' replaced.
#'
#' @param path Path to the store.
#' @param dest Path for the copy. Must differ from `path`.
#' @param con An open connection to the store from [dhis2_store_open()].
#'   Pass it when the store is open in this R session; it is left open.
#'   When `NULL`, the function opens its own connection and shuts the
#'   database down afterwards.
#' @return `dest`, invisibly.
#' @export
dhis2_store_snapshot <- function(path, dest, con = NULL) {
  if (!fs::file_exists(path)) {
    cli::cli_abort("No store at {.path {path}}.")
  }
  source_files <- c(fs::path_abs(path), paste0(fs::path_abs(path), ".wal"))
  if (fs::path_abs(dest) %in% source_files) {
    cli::cli_abort("{.arg dest} must be a different file from {.arg path}.")
  }
  if (is.null(con)) {
    # a read-only connection cannot attach the new file
    con <- dhis2_store_open(path)
    on.exit(dhis2_store_close(con), add = TRUE)
  }

  fs::dir_create(fs::path_dir(dest))
  leftovers <- c(dest, paste0(dest, ".wal"))
  fs::file_delete(leftovers[fs::file_exists(leftovers)])

  source <- DBI::dbGetQuery(con, "SELECT current_database() AS name")$name
  DBI::dbExecute(con, paste0(
    "ATTACH ", DBI::dbQuoteString(con, as.character(dest)), " AS snapshot"
  ))
  # detach before the connection opened above is closed
  on.exit(DBI::dbExecute(con, "DETACH snapshot"), add = TRUE, after = FALSE)
  DBI::dbExecute(con, paste0(
    "COPY FROM DATABASE ", DBI::dbQuoteIdentifier(con, source),
    " TO snapshot"
  ))
  cli::cli_alert_info("Snapshot written to {.path {dest}}")
  invisible(dest)
}

#' Load saved Parquet files into the database
#'
#' Inserts new rows and replaces existing rows with matching identifiers.
#' Give either a folder, whose `.parquet` files are all loaded, or the
#' exact files to load.
#'
#' @param con A connection from [dhis2_store_open()].
#' @param interim_dir Folder holding the saved files, searched
#'   recursively. Ignored when `files` is given.
#' @param table The value table to load into.
#' @param delete_files Remove files after loading. Set `FALSE` to keep
#'   them as a record of completed downloads for resumption.
#' @param files Paths of the files to load. Files that do not exist are
#'   skipped.
#' @return The number of files loaded.
#' @export
dhis2_store_load <- function(con, interim_dir = NULL,
                             table = c(
                               "data_values", "analytics_values",
                               "analytics_subannual"
                             ),
                             delete_files = TRUE, files = NULL) {
  table <- rlang::arg_match(table)
  if (is.null(files)) {
    if (is.null(interim_dir) || !fs::dir_exists(interim_dir)) {
      return(0L)
    }
    files <- fs::dir_ls(interim_dir, glob = "*.parquet", recurse = TRUE)
  }
  files <- as.character(files)
  files <- files[fs::file_exists(files)]
  if (length(files) == 0) {
    return(0L)
  }
  .dhis2_store_insert(con, table, files)
  if (isTRUE(delete_files)) {
    fs::file_delete(files)
  }
  length(files)
}

# INSERT OR REPLACE the schema columns of one or more Parquet files. With
# `where`, an SQL condition, only the rows it selects are inserted.
.dhis2_store_insert <- function(con, table, files, where = NULL) {
  cols <- paste(
    DBI::dbQuoteIdentifier(con, names(.dhis2_store_schema[[table]])),
    collapse = ", "
  )
  source <- paste(
    DBI::dbQuoteString(con, as.character(files)), collapse = ", "
  )
  DBI::dbExecute(con, paste0(
    "INSERT OR REPLACE INTO ", table, " SELECT ", cols,
    " FROM read_parquet([", source, "])",
    if (!is.null(where)) paste0(" WHERE ", where)
  ))
}

# Replace the rows of one analytics request: delete every row inside its
# scope, then insert the rows of the file that fall inside the same scope,
# so a saved file from wider earlier access cannot write rows for units the
# account can no longer read. Without `org_unit` the scope is unknown:
# nothing is deleted and the whole file is inserted. Run inside a
# transaction.
.dhis2_store_replace_scope <- function(con, table, file, data_element, period,
                                       org_unit = NULL, period_type = NULL) {
  if (is.null(org_unit)) {
    return(.dhis2_store_insert(con, table, file))
  }
  quote_in <- function(x) {
    paste(DBI::dbQuoteString(con, as.character(x)), collapse = ", ")
  }
  where <- c(
    paste0("data_element IN (", quote_in(data_element), ")"),
    paste0("period IN (", quote_in(period), ")"),
    if (length(org_unit) > 0) {
      paste0("org_unit IN (", quote_in(org_unit), ")")
    } else {
      "FALSE"
    },
    if (!is.null(period_type)) {
      paste0("period_type = ", DBI::dbQuoteString(con, period_type))
    }
  )
  where <- paste(where, collapse = " AND ")
  DBI::dbExecute(con, paste0("DELETE FROM ", table, " WHERE ", where))
  .dhis2_store_insert(con, table, file, where = where)
}

# Write to a temporary path, then rename the completed file so an interrupted
# write is not mistaken for a completed download.
.dhis2_write_parquet <- function(data, path) {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb::duckdb_register(con, "parked", data)
  tmp <- paste0(path, ".writing")
  DBI::dbExecute(con, paste0(
    "COPY parked TO ", DBI::dbQuoteString(con, tmp), " (FORMAT parquet)"
  ))
  fs::file_move(tmp, path)
  invisible(path)
}

#' Summarise the store
#'
#' Returns one row per value table, with counts of values, organisation
#' units, data elements and periods, and the first and last period
#' identifiers. Counts are reported separately to avoid double counting
#' data that appears in more than one table.
#'
#' @param con A connection from [dhis2_store_open()].
#' @return A tibble.
#' @export
dhis2_store_summary <- function(con) {
  tables <- intersect(.dhis2_value_tables, DBI::dbListTables(con))
  purrr::map(tables, function(table) {
    values <- dplyr::tbl(con, table)
    summary <- values |>
      dplyr::summarise(
        values_stored = dplyr::n(),
        org_units = dplyr::n_distinct(org_unit),
        elements = dplyr::n_distinct(data_element),
        periods = dplyr::n_distinct(period),
        first_period = min(period, na.rm = TRUE),
        last_period = max(period, na.rm = TRUE)
      ) |>
      dplyr::collect()
    deleted_values <- if ("deleted" %in% colnames(values)) {
      values |>
        dplyr::filter(deleted) |>
        dplyr::count() |>
        dplyr::pull(n)
    } else {
      NA_real_
    }
    summary |>
      dplyr::mutate(
        value_table = table,
        deleted_values = as.numeric(deleted_values),
        .before = 1
      )
  }) |>
    purrr::list_rbind()
}

# Read-only connections used by dhis2_store_tbl(), one per store.
.dhis2_readers <- new.env(parent = emptyenv())

# Size and modification time of the database file and its write-ahead log.
# A change means the store was written since it was last read.
.dhis2_store_fingerprint <- function(path) {
  files <- c(path, paste0(path, ".wal"))
  info <- fs::file_info(files[fs::file_exists(files)])
  paste(
    fs::path_file(info$path), info$size,
    format(info$modification_time, "%Y%m%d%H%M%OS6"),
    collapse = "|"
  )
}

.dhis2_connection_alive <- function(con) {
  DBI::dbIsValid(con) && !inherits(
    tryCatch(DBI::dbGetQuery(con, "SELECT 1"), error = function(e) e),
    "error"
  )
}

# Drop the read-only connection kept for a store. The database is shut
# down only when nothing else in this process has it open for writing, so
# a writer's connection is never closed from here.
.dhis2_store_release <- function(path) {
  key <- as.character(fs::path_abs(path))
  held <- .dhis2_readers[[key]]
  if (is.null(held)) {
    return(invisible(NULL))
  }
  if (DBI::dbIsValid(held$con)) {
    readonly <- tryCatch(
      DBI::dbGetQuery(
        held$con,
        "SELECT readonly FROM duckdb_databases()
         WHERE database_name = current_database()"
      )$readonly,
      error = function(e) FALSE
    )
    DBI::dbDisconnect(held$con, shutdown = isTRUE(readonly))
  }
  rm(list = key, envir = .dhis2_readers)
  invisible(NULL)
}

# A read-only connection to the store. The connection is reopened when the
# file has changed since it was opened, or when a connection elsewhere has
# shut the database down. Lock and I/O errors are raised, not hidden.
.dhis2_store_reader <- function(path) {
  path <- as.character(fs::path_abs(path))
  if (!fs::file_exists(path)) {
    cli::cli_abort("No store at {.path {path}}.")
  }
  mark <- .dhis2_store_fingerprint(path)
  held <- .dhis2_readers[[path]]
  if (!is.null(held)) {
    if (identical(held$mark, mark) && .dhis2_connection_alive(held$con)) {
      return(held$con)
    }
    .dhis2_store_release(path)
  }
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
  .dhis2_readers[[path]] <- list(con = con, mark = mark)
  con
}

#' Query a database table before loading it into R
#'
#' Returns a duckplyr data frame so you can filter rows and select columns
#' before loading results into R with `dplyr::collect()`. The database must
#' be available for reading. Use [dhis2_store_snapshot()] to create a
#' separate copy for analysis.
#'
#' The table is read through a read-only connection that the package keeps
#' open and reopens when the file changes, so a read after a download sees
#' the new rows. Collect a result before the store is written again: a
#' frame that is still lazy when the file changes may fail to load.
#'
#' @param path Path to the store.
#' @param table Table name.
#' @return A duckplyr data frame.
#' @export
dhis2_store_tbl <- function(path, table = "data_values") {
  con <- .dhis2_store_reader(path)
  duckplyr::read_sql_duckdb(
    paste0("SELECT * FROM ", DBI::dbQuoteIdentifier(con, table)),
    con = con
  )
}

# Cache the data element lists to avoid repeatedly scanning the value tables.
.dhis2_index_cache <- new.env(parent = emptyenv())

#' Data elements in each value table
#'
#' The lists are cached and read again when the store file changes.
#'
#' @param path Path to the store.
#' @param refresh Read the tables again instead of using the cached lists.
#' @return A named list of data element UIDs per value table.
#' @export
dhis2_store_element_index <- function(path, refresh = FALSE) {
  key <- as.character(fs::path_abs(path))
  mark <- .dhis2_store_fingerprint(key)
  held <- .dhis2_index_cache[[key]]
  if (!refresh && !is.null(held) && identical(held$mark, mark)) {
    return(held$index)
  }
  seen <- function(table) {
    out <- dhis2_store_tbl(path, table) |>
      dplyr::distinct(data_element) |>
      dplyr::collect()
    sort(out$data_element)
  }
  index <- purrr::map(rlang::set_names(.dhis2_value_tables), seen)
  .dhis2_index_cache[[key]] <- list(index = index, mark = mark)
  index
}

# Missing-value defaults for fields not included in analytics results.
.dhis2_audit_fill <- list(
  attr_option_combo = NA_character_, stored_by = NA_character_,
  created = NA_character_, last_updated = NA_character_,
  comment = NA_character_, followup = NA, deleted = FALSE
)

#' Read monthly values from the store
#'
#' Reads each data element from `data_values` if it is present there, or
#' from `analytics_values` otherwise. Raw values retain attribute option
#' combinations and fields such as who entered a value and when.
#' Deleted values are excluded. To read weekly or daily analytics, use
#' [dhis2_store_subannual()].
#'
#' @param path Path to the store.
#' @param element_uids Data element UIDs to read.
#' @param years Years to keep, taken from the period identifier; all when
#'   `NULL`.
#' @param combos Category option combination UIDs to keep; all when `NULL`.
#' @param cols Columns to return. For analytics rows, `stored_by`, `created`
#'   and `last_updated` are `NA`.
#' @return A tibble with the requested columns. `value`, when requested,
#'   is a number.
#' @export
dhis2_store_values <- function(path, element_uids, years = NULL,
                               combos = NULL,
                               cols = c(
                                 "data_element", "period", "org_unit",
                                 "cat_option_combo", "value"
                               )) {
  index <- dhis2_store_element_index(path)
  from_dv <- intersect(element_uids, index$data_values)
  from_av <- setdiff(intersect(element_uids, index$analytics_values), from_dv)

  read_table <- function(table, uids) {
    if (length(uids) == 0) {
      return(NULL)
    }
    q <- dhis2_store_tbl(path, table) |>
      dplyr::filter(data_element %in% uids)
    if (table == "data_values") {
      q <- dplyr::filter(q, !deleted)
    }
    if (!is.null(years)) {
      keep_years <- as.character(years)
      q <- dplyr::filter(q, substr(period, 1L, 4L) %in% keep_years)
    }
    if (!is.null(combos)) {
      q <- dplyr::filter(q, .data$cat_option_combo %in% combos)
    }
    have <- intersect(cols, colnames(q))
    missing <- setdiff(cols, have)
    fills <- purrr::map(
      rlang::set_names(missing),
      \(m) .dhis2_audit_fill[[m]] %||% NA_character_
    )
    q |>
      dplyr::select(dplyr::all_of(have)) |>
      dplyr::collect() |>
      dplyr::mutate(!!!fills) |>
      dplyr::select(dplyr::all_of(cols))
  }

  out <- dplyr::bind_rows(
    read_table("data_values", from_dv),
    read_table("analytics_values", from_av)
  )
  if (nrow(out) == 0) {
    out <- .dhis2_empty("data_values") |>
      dplyr::select(dplyr::any_of(cols))
    out[setdiff(cols, names(out))] <- list(character())
    out <- dplyr::select(out, dplyr::all_of(cols))
  }
  if ("value" %in% cols) {
    out <- dplyr::mutate(
      out, value = suppressWarnings(as.numeric(.data$value))
    )
  }
  tibble::as_tibble(out)
}

#' Read weekly or daily analytics from the database
#'
#' Returns values from `analytics_subannual`. These may also appear in
#' monthly totals returned by [dhis2_store_values()]. Keep the results
#' separate to avoid counting the same data twice.
#'
#' @inheritParams dhis2_store_values
#' @param element_uids Data element UIDs; every element in the table when
#'   `NULL`.
#' @param period_type Period types to keep, such as `"WeeklySunday"`.
#' @return A tibble with `value` as a number.
#' @export
dhis2_store_subannual <- function(path, element_uids = NULL,
                                  period_type = NULL, years = NULL) {
  index <- dhis2_store_element_index(path)
  have <- intersect(
    element_uids %||% index$analytics_subannual, index$analytics_subannual
  )
  if (length(have) == 0) {
    return(.dhis2_empty("analytics_subannual") |>
      dplyr::mutate(value = numeric()))
  }
  q <- dhis2_store_tbl(path, "analytics_subannual") |>
    dplyr::filter(data_element %in% have)
  if (!is.null(period_type)) {
    types <- period_type
    q <- dplyr::filter(q, .data$period_type %in% types)
  }
  if (!is.null(years)) {
    keep_years <- as.character(years)
    q <- dplyr::filter(q, substr(period, 1L, 4L) %in% keep_years)
  }
  q |>
    dplyr::collect() |>
    dplyr::mutate(value = suppressWarnings(as.numeric(.data$value))) |>
    tibble::as_tibble()
}

#' Check whether data elements can be summed
#'
#' `dhis2_element_agg()` lists each element's aggregation type.
#' `dhis2_assert_summable()` stops with an error if any selected element's
#' aggregation type is not `SUM`. Values such as percentages and stock
#' balances need the aggregation method defined for that element.
#'
#' @param elements Output of [dhis2_data_elements()].
#' @param uids Data element UIDs; use `NULL` to include all elements.
#' @return `dhis2_element_agg()` gives a tibble; `dhis2_assert_summable()`
#'   gives `TRUE`, invisibly.
#' @export
dhis2_element_agg <- function(elements, uids = NULL) {
  out <- dplyr::select(
    elements, de_uid, de_name, value_type, aggregation_type
  )
  if (!is.null(uids)) {
    out <- dplyr::filter(out, de_uid %in% uids)
  }
  out
}

#' @rdname dhis2_element_agg
#' @export
dhis2_assert_summable <- function(elements, uids) {
  bad <- dhis2_element_agg(elements, uids) |>
    dplyr::filter(aggregation_type != "SUM")
  if (nrow(bad) > 0) {
    cli::cli_abort(c(
      "{nrow(bad)} element{?s} {?is/are} not SUM and cannot be added up.",
      "i" = "{.val {utils::head(bad$de_name, 3)}}",
      "i" = "Use the method in {.field aggregation_type} returned by
             {.fn dhis2API::dhis2_element_agg}."
    ))
  }
  invisible(TRUE)
}
