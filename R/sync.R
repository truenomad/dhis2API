# Organisation unit UIDs from a character vector or a tibble with `ou_uid`.
.dhis2_unit_ids <- function(units) {
  if (is.data.frame(units)) {
    if (!"ou_uid" %in% names(units)) {
      cli::cli_abort("{.arg units} needs an {.field ou_uid} column.")
    }
    return(unique(units$ou_uid))
  }
  unique(as.character(units))
}

# A label for the data set and data element group a download selects, as
# recorded in the download log. Empty when neither is given.
.dhis2_selection <- function(data_set = NULL, data_element_group = NULL) {
  parts <- c(
    if (!is.null(data_set)) {
      paste0("dataSet=", paste(sort(data_set), collapse = ";"))
    },
    if (!is.null(data_element_group)) {
      paste0(
        "dataElementGroup=", paste(sort(data_element_group), collapse = ";")
      )
    }
  )
  paste(parts, collapse = "&")
}

# The parts of `from` to `to` not covered by the date ranges in `covered`,
# a data frame with Date columns `start` and `end`. Ranges that touch, such
# as one ending on 31 December and one starting on 1 January, join.
.dhis2_uncovered <- function(from, to, covered) {
  covered <- covered |>
    dplyr::filter(.data$end >= from, .data$start <= to) |>
    dplyr::arrange(.data$start)
  gaps <- list()
  cursor <- from
  for (i in seq_len(nrow(covered))) {
    if (covered$start[[i]] > cursor) {
      gaps[[length(gaps) + 1]] <- list(
        start = cursor, end = covered$start[[i]] - 1L
      )
    }
    cursor <- max(cursor, covered$end[[i]] + 1L)
    if (cursor > to) {
      break
    }
  }
  if (cursor <= to) {
    gaps[[length(gaps) + 1]] <- list(start = cursor, end = to)
  }
  if (length(gaps) == 0) {
    return(tibble::tibble(
      start = as.Date(character()), end = as.Date(character())
    ))
  }
  tibble::tibble(
    start = purrr::map_vec(gaps, "start"), end = purrr::map_vec(gaps, "end")
  )
}

#' Choose a full download or an update
#'
#' Checks the download log for each organisation unit. A unit's full
#' download is complete when its logged full downloads for this element
#' group, taken together, cover every day from `window_start` to
#' `window_end`. Until that holds for every unit, the mode is `"backfill"`
#' and the units with missing dates are selected, along with the dates each
#' one lacks. After that, the mode is `"delta"` and all units are selected
#' for updates.
#'
#' Extending `window_end` therefore adds the new dates to the download
#' instead of treating them as already complete. When `selection` is given,
#' only full downloads logged with the same selection count. Downloads
#' logged by an earlier version of the package have no selection recorded,
#' so they do not count for an explicit selection: the first run with a
#' selection downloads the history once more, keeping the earlier rows.
#' With `selection = NULL`, every logged full download counts, as before.
#'
#' @param con A connection from [dhis2_store_open()].
#' @param units Organisation unit UIDs, or a tibble with `ou_uid`.
#' @param window_start First date of the full download range.
#' @param element_group Group name used in the download log.
#' @param window_end Last date of the full download range. When `NULL`,
#'   coverage of `window_start` alone is checked.
#' @param selection The data set and data element group of the download,
#'   as written to the log by [dhis2_sync_group()]; `NULL` to accept any.
#' @return A list with `mode` (`"backfill"` or `"delta"`), `banked` (units
#'   whose full download is complete), `todo` (units to download), `gaps`
#'   (a tibble with `org_unit`, `start` and `end` for each missing date
#'   range) and `partial` (units with some dates downloaded but not all).
#' @export
dhis2_plan_run <- function(con, units, window_start, element_group,
                           window_end = NULL, selection = NULL) {
  units <- .dhis2_unit_ids(units)
  start <- as.Date(window_start)
  end <- if (is.null(window_end)) start else as.Date(window_end)
  group <- element_group

  log <- dplyr::tbl(con, "download_log")
  has_selection <- "selection" %in% colnames(log)
  done <- log |>
    dplyr::filter(
      .data$mode == "backfill",
      .data$element_group == group,
      .data$org_unit %in% units
    ) |>
    dplyr::select(dplyr::any_of(
      c("org_unit", "window_start", "window_end", "selection")
    )) |>
    dplyr::collect()
  if (!has_selection) {
    done$selection <- NA_character_
  }
  # a row without a selection is no proof that this selection was downloaded
  if (!is.null(selection)) {
    done <- dplyr::filter(done, .data$selection == !!selection)
  }
  done <- done |>
    dplyr::transmute(
      org_unit,
      start = as.Date(.data$window_start),
      end = as.Date(.data$window_end)
    )

  gaps <- purrr::map(units, function(u) {
    .dhis2_uncovered(start, end, dplyr::filter(done, org_unit == u)) |>
      dplyr::mutate(org_unit = u, .before = 1)
  }) |>
    purrr::list_rbind()
  if (nrow(gaps) == 0) {
    gaps <- tibble::tibble(
      org_unit = character(), start = as.Date(character()),
      end = as.Date(character())
    )
  }

  incomplete <- unique(gaps$org_unit)
  banked <- setdiff(units, incomplete)
  mode <- if (length(incomplete) == 0) "delta" else "backfill"
  list(
    mode = mode,
    banked = banked,
    todo = if (mode == "backfill") incomplete else units,
    gaps = gaps,
    partial = intersect(incomplete, unique(done$org_unit))
  )
}

#' Record a run in the download log
#'
#' @inheritParams dhis2_plan_run
#' @param run_at Date and time of the run, as `YYYY-MM-DDTHH:MM:SS`.
#' @param mode `"backfill"` for a full download, or `"delta"` for updates.
#' @param org_units Units the run covered.
#' @param window_end Last date of the download range.
#' @param rows_returned Row count for each unit, or one count repeated for
#'   every unit.
#' @param selection The data set and data element group downloaded, as
#'   recorded by [dhis2_sync_group()]; `NULL` leaves the column empty.
#' @return `NULL`, invisibly.
#' @export
dhis2_log_run <- function(con, run_at, mode, element_group, org_units,
                          window_start, window_end, rows_returned = 0L,
                          selection = NULL) {
  if (length(org_units) == 0) {
    return(invisible(NULL))
  }
  DBI::dbAppendTable(
    con, "download_log",
    data.frame(
      run_at = run_at,
      mode = mode,
      element_group = element_group,
      org_unit = org_units,
      window_start = format(as.Date(window_start)),
      window_end = format(as.Date(window_end)),
      rows_returned = as.integer(rows_returned),
      selection = selection %||% NA_character_
    )
  )
  invisible(NULL)
}

#' Calculate how many days of changes to download
#'
#' Uses the time since the group's least recently updated unit, rounded up
#' to whole days, plus one day of overlap. Returns at least `floor_days`.
#'
#' @inheritParams dhis2_plan_run
#' @param floor_days Minimum number of days; defaults to the `delta_days`
#'   setting of `session`, then the R option, then the package default.
#' @param session A session from [dhis2_session()] whose settings apply.
#'   `NULL` reads the option or default without opening a session.
#' @return Days, as an integer.
#' @export
dhis2_delta_window <- function(con, element_group, floor_days = NULL,
                               session = NULL) {
  floor_days <- as.integer(
    floor_days %||% .dhis2_setting(session, "delta_days")
  )
  group <- element_group
  last <- dplyr::tbl(con, "download_log") |>
    dplyr::filter(.data$element_group == group) |>
    dplyr::summarise(last_run = max(run_at, na.rm = TRUE), .by = org_unit) |>
    dplyr::summarise(last_run = min(last_run, na.rm = TRUE)) |>
    dplyr::pull(last_run)

  seen <- as.POSIXct(last, format = "%Y-%m-%dT%H:%M:%S")
  if (length(seen) == 0 || is.na(seen)) {
    return(floor_days)
  }
  gap <- as.numeric(difftime(Sys.time(), seen, units = "days"))
  max(floor_days, as.integer(ceiling(gap)) + 1L)
}

# Download one unit to a Parquet file. Each worker writes a separate file.
.dhis2_fetch_to_interim <- function(ou_uid, interim_dir, session, ...) {
  values <- dhis2_data_values(org_unit = ou_uid, ..., session = session)
  if (nrow(values) == 0) {
    return(0L)
  }
  .dhis2_write_parquet(values, .dhis2_unit_files(interim_dir, ou_uid))
  nrow(values)
}

# The file each unit's download is saved to.
.dhis2_unit_files <- function(dir, units) {
  as.character(fs::path(dir, paste0("u_", units, ".parquet")))
}

# Identify completed unit downloads from their file names.
.dhis2_parked_units <- function(dir) {
  if (!fs::dir_exists(dir)) {
    return(character(0))
  }
  files <- fs::dir_ls(dir, glob = "*.parquet", type = "file")
  sub("^u_", "", fs::path_ext_remove(fs::path_file(files)))
}

# What a folder of unit downloads was requested from: the server and
# account, and the arguments sent with each request. No secret is included.
.dhis2_fetch_manifest <- function(session, fetch_args) {
  args <- purrr::map(fetch_args, function(x) {
    if (inherits(x, "Date")) format(x) else x
  })
  list(
    identity = .dhis2_session_identity(session),
    args = args[order(names(args))]
  )
}

# Keep the saved files in `dir` only when they came from the same request.
# Files from another server, account or request are removed so they are
# neither reused nor loaded.
.dhis2_claim_interim <- function(dir, manifest, resume) {
  file <- fs::path(dir, "manifest.qs2")
  saved <- if (fs::file_exists(file)) qs2::qs_read(file) else NULL
  same <- identical(saved, manifest)
  if (!resume || !same) {
    stale <- fs::dir_ls(dir, glob = "*.parquet", type = "file")
    if (length(stale) > 0 && !same && resume) {
      cli::cli_alert_info(
        "Removing {length(stale)} saved file{?s} from a different request."
      )
    }
    fs::file_delete(stale)
  }
  qs2::qs_save(manifest, file)
  invisible(same)
}

#' Download data values for organisation units in parallel
#'
#' Downloads each unit separately and saves non-empty results as Parquet
#' files named by UID. On resumption, units with saved files are skipped.
#' Failed units are retried one at a time after the other downloads finish.
#'
#' A manifest in `interim_dir` records the server, account and request
#' arguments the files came from. Saved files are reused only when the
#' manifest matches the current request; otherwise they are removed first.
#'
#' Workers run in separate R processes through future, so the package must
#' be installed, not only loaded with `devtools::load_all()`, when
#' `workers > 1`. The caller's future plan is restored afterwards.
#'
#' @param units Organisation unit UIDs, or a tibble with `ou_uid`.
#' @param interim_dir Folder for downloaded files awaiting database loading.
#' @param ... Arguments passed to [dhis2_data_values()], including dates
#'   and a data set or data element group.
#' @param workers Number of R processes; defaults to the `workers` setting.
#' @param resume Skip units with saved files from the same request. When
#'   `FALSE`, saved files are removed first.
#' @param progress Show a progress bar.
#' @inheritParams dhis2_request
#' @return Rows downloaded per unit, named by UID: 0 for a unit skipped on
#'   resume or with no data, `NA` for a unit that failed twice.
#' @export
dhis2_fetch_units <- function(units, interim_dir, ..., workers = NULL,
                              resume = TRUE, progress = TRUE,
                              session = NULL) {
  session <- .dhis2_session(session)
  units <- .dhis2_unit_ids(units)
  workers <- as.integer(workers %||% .dhis2_setting(session, "workers"))
  fs::dir_create(interim_dir)

  # a half-written file from a killed run is never valid
  partial <- fs::dir_ls(interim_dir, glob = "*.writing")
  fs::file_delete(partial)

  # captured as a list, since a worker cannot see this call's dots
  fetch_args <- list(...)
  .dhis2_claim_interim(
    interim_dir, .dhis2_fetch_manifest(session, fetch_args), resume
  )

  done <- character(0)
  if (resume) {
    done <- intersect(units, .dhis2_parked_units(interim_dir))
    if (length(done) > 0) {
      cli::cli_alert_info(
        "Resuming: {length(done)} unit{?s} already saved to files."
      )
    }
  }

  out <- rlang::set_names(rep(0L, length(units)), units)
  pending <- setdiff(units, done)
  if (length(pending) == 0) {
    return(out)
  }

  safe_fetch <- purrr::possibly(.dhis2_fetch_to_interim, NA_integer_)
  fetch_one <- function(ou) {
    rlang::exec(safe_fetch, ou, interim_dir, session, !!!fetch_args)
  }
  n_workers <- min(workers, length(pending))
  cli::cli_alert_info(
    "{n_workers} worker{?s} fetching {length(pending)} unit{?s}"
  )

  run <- function() {
    tick <- progressr::progressor(steps = length(pending))
    one <- function(ou) {
      rows <- fetch_one(ou)
      tick()
      rows
    }
    if (n_workers > 1L) {
      # one unit at a time, so a worker that draws a slow unit does not hold
      # a whole chunk while the others wait
      return(furrr::future_map_int(
        pending, one,
        .options = furrr::furrr_options(seed = NULL, chunk_size = 1)
      ))
    }
    purrr::map_int(pending, one)
  }

  if (n_workers > 1L) {
    old_plan <- future::plan(future::multisession, workers = n_workers)
    on.exit(future::plan(old_plan), add = TRUE)
  }

  got <- if (isTRUE(progress)) {
    progressr::with_progress(run(), handlers = progressr::handler_cli())
  } else {
    progressr::without_progress(run())
  }

  retry <- pending[is.na(got)]
  if (length(retry) > 0) {
    cli::cli_alert_info("Reading {length(retry)} failed unit{?s} again")
    got[is.na(got)] <- purrr::map_int(retry, fetch_one)
  }

  out[pending] <- got
  out
}

#' Download or update a data element group in the database
#'
#' Downloads values, loads the saved files into the database and records
#' completed units in the download log.
#'
#' Each run downloads the dates a unit's full download still lacks, one
#' calendar year per request, and can resume after an interruption. Units
#' whose full download is already complete, or partly complete, also
#' receive an update: only values changed in the last `delta_days`,
#' including deletions, across the whole range. Extending `window_end` or
#' adding units therefore downloads the new history in full, while existing
#' units stay up to date. A unit that fails twice is not recorded as
#' complete and is retried on the next run.
#'
#' The download log records the data set and data element group with each
#' full download, so changing them under the same `group_name` starts a new
#' full download rather than reusing the old one.
#'
#' Only files saved by this run are loaded. Saved files are reused only
#' when they came from the same server, account and request.
#'
#' @param con A connection from [dhis2_store_open()].
#' @param group_name Group name used in the download log and file paths.
#' @param units Organisation unit UIDs, or a tibble with `ou_uid`. Includes
#'   units below each selected unit, such as facilities within a district.
#' @param interim_dir Folder for downloaded files; each group has a subfolder.
#' @param window_start,window_end First and last dates of the full range.
#' @param data_element_group,data_set UIDs selecting the values.
#' @param delta_days Number of days to check for changes; calculated by
#'   [dhis2_delta_window()] from the session's `delta_days` setting when
#'   `NULL`.
#' @param format `"csv"` or `"json"`; see [dhis2_data_values()].
#' @param workers Number of R processes used for downloading.
#' @param run_at Date and time written to the log.
#' @inheritParams dhis2_request
#' @return Rows fetched, invisibly.
#' @export
dhis2_sync_group <- function(con, group_name, units, interim_dir,
                             window_start, window_end,
                             data_element_group = NULL, data_set = NULL,
                             delta_days = NULL, format = c("csv", "json"),
                             workers = NULL,
                             run_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                             session = NULL) {
  session <- .dhis2_session(session)
  format <- rlang::arg_match(format)
  cli::cli_h2(group_name)

  group_dir <- fs::path(interim_dir, group_name)
  units <- .dhis2_unit_ids(units)
  window_start <- as.Date(window_start)
  window_end <- as.Date(window_end)
  selection <- .dhis2_selection(data_set, data_element_group)
  plan <- dhis2_plan_run(
    con, units, window_start, group_name,
    window_end = window_end, selection = selection
  )

  # units with any history already downloaded receive the update
  update <- if (plan$mode == "delta") units else {
    union(plan$banked, plan$partial)
  }
  cli::cli_alert_info(
    "Full download complete for {length(plan$banked)} of {length(units)}
     unit{?s}. Mode: {.strong {plan$mode}}. Downloading history for
     {length(plan$todo)} unit{?s} and updates for {length(update)} unit{?s}."
  )

  got <- 0L
  files <- character(0)
  failed_history <- character(0)
  fetch <- function(todo, dir, ...) {
    rows <- dhis2_fetch_units(
      units = todo, interim_dir = dir, workers = workers,
      data_set = data_set, data_element_group = data_element_group,
      format = format, ..., session = session
    )
    got <<- got + sum(rows, na.rm = TRUE)
    saved <- .dhis2_unit_files(dir, todo)
    files <<- c(files, saved[fs::file_exists(saved)])
    names(rows)[is.na(rows)]
  }

  # Separate folders per date range so resumption only finds files for
  # this request.
  ranges <- plan$gaps |>
    dplyr::summarise(units = list(.data$org_unit), .by = c("start", "end"))
  for (i in seq_len(nrow(ranges))) {
    chunks <- dhis2_period_chunks(ranges$start[[i]], ranges$end[[i]])
    for (chunk in chunks) {
      cli::cli_alert_info("Date range: {chunk$start} to {chunk$end}")
      label <- paste(format(chunk$start), format(chunk$end), sep = "_")
      failed <- fetch(
        ranges$units[[i]], fs::path(group_dir, "backfill", label),
        resume = TRUE, start_date = chunk$start, end_date = chunk$end
      )
      failed_history <- union(failed_history, failed)
    }
  }

  failed_update <- character(0)
  if (length(update) > 0) {
    delta_days <- delta_days %||%
      dhis2_delta_window(con, group_name, session = session)
    cli::cli_alert_info("Updates: changes in the last {delta_days} day{?s}")
    # Update runs must replace files saved for an earlier update interval.
    failed_update <- fetch(
      update, fs::path(group_dir, "delta"),
      resume = FALSE, start_date = window_start, end_date = window_end,
      last_updated_duration = paste0(delta_days, "d")
    )
  }

  n_files <- 0L
  DBI::dbWithTransaction(con, {
    n_files <- dhis2_store_load(con, table = "data_values", files = files)
    for (i in seq_len(nrow(ranges))) {
      dhis2_log_run(
        con, run_at, "backfill", group_name,
        setdiff(ranges$units[[i]], failed_history),
        ranges$start[[i]], ranges$end[[i]], selection = selection
      )
    }
    dhis2_log_run(
      con, run_at, "delta", group_name, setdiff(update, failed_update),
      window_start, window_end, selection = selection
    )
  })

  failed <- union(failed_history, failed_update)
  if (length(failed) > 0) {
    cli::cli_alert_warning(
      "{length(failed)} unit{?s} failed twice and {?was/were} not logged"
    )
  }
  cli::cli_alert_success("{got} row{?s} loaded from {n_files} file{?s}")
  invisible(got)
}

#' Download analytics to the database
#'
#' Splits the download by data element and period, retries failed requests
#' over smaller period ranges, and saves completed responses as Parquet
#' files. Loads these files into `analytics_values` or
#' `analytics_subannual`.
#'
#' Each request that succeeds replaces the table's rows for its data
#' elements, periods and organisation units, so values the server no
#' longer returns are removed, including when the response is empty. Rows
#' for a request that fails, or only partly succeeds, are left as they
#' were. The organisation units a selection covers are worked out from the
#' selection itself: UIDs; `USER_ORGUNIT`, `USER_ORGUNIT_CHILDREN` and
#' `USER_ORGUNIT_GRANDCHILDREN` from the account's units; `LEVEL-<n>`,
#' `LEVEL-<n>-<uid>` and `OU_GROUP-<uid>` from the organisation unit
#' metadata, within the UIDs or user units given alongside them and within
#' the account's data view organisation units, because metadata can list
#' units whose data the account cannot read. The data view units are read
#' from the server on every run, never from the metadata cache, and both
#' the rows removed and the rows written from a saved file are limited to
#' that scope. If the data view units cannot be read, the function stops
#' before downloading or changing anything. An item that is not recognised
#' stops the function from removing anything: its old rows are kept and a
#' warning is given.
#'
#' Each saved file is bound to the server, account, table, period type and
#' selection it came from. With `resume = TRUE`, requests whose saved file
#' matches are skipped, and only the files of this run are loaded. Empty
#' responses are saved as completed downloads; failed requests are retried
#' on the next run. This function does not check for changed values on the
#' server. Set `resume = FALSE` to refresh previously saved results.
#'
#' @param con A connection from [dhis2_store_open()].
#' @param dx Data element UIDs.
#' @param periods Period identifiers.
#' @param ou Organisation unit selections, such as `"LEVEL-5"`.
#' @param interim_dir Folder for downloaded files awaiting database loading.
#' @param table `"analytics_values"` for monthly rows,
#'   `"analytics_subannual"` for weekly or daily ones.
#' @param period_type Period type recorded with weekly or daily rows.
#' @param dx_per_request,periods_per_request Items per request.
#' @param resume Skip requests with saved files.
#' @param keep_files Keep downloaded files after loading, allowing later
#'   runs to skip them. Defaults to `TRUE` for `analytics_subannual` and
#'   `FALSE` for `analytics_values`.
#' @inheritParams dhis2_request
#' @param org_units Output of [dhis2_org_units()], used to work out which
#'   units `ou` covers. Read from the server when `NULL` and needed.
#' @return A list with `rows` (newly downloaded rows) and `failed` (number
#'   of failed requests), invisibly.
#' @export
dhis2_sync_analytics <- function(con, dx, periods, ou, interim_dir,
                                 table = c(
                                   "analytics_values", "analytics_subannual"
                                 ),
                                 period_type = NULL, dx_per_request = 10L,
                                 periods_per_request = 3L, resume = TRUE,
                                 keep_files = NULL, session = NULL,
                                 org_units = NULL) {
  session <- .dhis2_session(session)
  table <- rlang::arg_match(table)
  subannual <- table == "analytics_subannual"
  if (subannual && is.null(period_type)) {
    cli::cli_abort("Sub-annual rows need {.arg period_type}.")
  }
  keep_files <- keep_files %||% subannual
  fs::dir_create(interim_dir)

  scope_units <- .dhis2_analytics_units(ou, org_units, session = session)
  if (is.null(scope_units)) {
    cli::cli_warn(c(
      "The units covered by {.val {ou}} cannot be worked out: an item is
       not a UID, USER_ORGUNIT item, LEVEL- item or OU_GROUP- item.",
      "i" = "Values the server no longer returns stay in {.field {table}}."
    ))
  }

  identity <- .dhis2_session_identity(session)
  grid <- tidyr::expand_grid(
    dx = .dhis2_chunk(dx, dx_per_request),
    pe = .dhis2_chunk(periods, periods_per_request)
  )
  grid$scope <- purrr::map2(grid$dx, grid$pe, function(d, p) {
    list(
      identity = identity, dx = sort(d), pe = sort(p), ou = ou,
      table = table, period_type = period_type
    )
  })
  grid$file <- purrr::map_chr(grid$scope, function(scope) {
    as.character(fs::path(
      interim_dir, paste0("a_", rlang::hash(scope), ".parquet")
    ))
  })
  grid$manifest <- paste0(fs::path_ext_remove(grid$file), ".qs2")
  grid$saved <- purrr::pmap_lgl(
    grid[c("file", "manifest", "scope")],
    function(file, manifest, scope) {
      resume && fs::file_exists(file) && fs::file_exists(manifest) &&
        identical(qs2::qs_read(manifest), scope)
    }
  )
  todo <- which(!grid$saved)
  cli::cli_alert_info(
    "{length(todo)} of {nrow(grid)} analytics request{?s} to download"
  )

  # a request is loaded when its file was reused or written by this run
  ready <- grid$saved
  rows <- 0L
  failed <- 0L
  if (length(todo) > 0) {
    cli::cli_progress_bar("Analytics", total = length(todo))
    for (i in todo) {
      slice <- .dhis2_analytics_slice(
        grid$dx[[i]], grid$pe[[i]], ou, session = session
      )
      if (isTRUE(slice$failed)) {
        failed <- failed + 1L
      } else {
        shaped <- if (subannual) {
          dhis2_analytics_to_subannual(slice$data, period_type)
        } else {
          dhis2_analytics_to_store(slice$data)
        }
        .dhis2_write_parquet(shaped, grid$file[[i]])
        qs2::qs_save(grid$scope[[i]], grid$manifest[[i]])
        rows <- rows + nrow(shaped)
        ready[[i]] <- TRUE
      }
      cli::cli_progress_update()
    }
    cli::cli_progress_done()
  }

  ready <- grid[ready, ]
  DBI::dbWithTransaction(con, {
    for (i in seq_len(nrow(ready))) {
      .dhis2_store_replace_scope(
        con, table, ready$file[[i]],
        data_element = ready$dx[[i]], period = ready$pe[[i]],
        org_unit = scope_units,
        period_type = if (subannual) period_type else NULL
      )
    }
  })
  if (!keep_files) {
    fs::file_delete(c(ready$file, ready$manifest))
  }

  if (failed > 0) {
    cli::cli_alert_warning(
      "{failed} request{?s} failed and will be retried on the next run"
    )
  }
  cli::cli_alert_success(
    "{rows} new row{?s}; {nrow(ready)} file{?s} loaded into {.field {table}}"
  )
  invisible(list(rows = rows, failed = failed))
}
