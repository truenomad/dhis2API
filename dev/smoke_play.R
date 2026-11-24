# Live smoke test against the public DHIS2 demo servers (fake data, public
# admin/district login). Prints one line per check, counts only.
# Run from the package root after devtools::install().

versions <- c(
  "stable-2-41-11", "stable-2-42-7-1", "stable-2-43-2-1", "dev"
)
data_set <- "BfMAe6Itzgt"
element <- "fbfJHSPpUQD"
country <- "ImspTQPwCqd"
program <- "IpHINAT79UW"

check <- function(name, code) {
  started <- Sys.time()
  detail <- tryCatch(
    format(code),
    error = function(e) paste("FAIL:", gsub("\\s+", " ", conditionMessage(e)))
  )
  secs <- round(as.numeric(difftime(Sys.time(), started, units = "secs")))
  tibble::tibble(check = name, detail = substr(detail[[1]], 1, 90), secs = secs)
}

smoke <- function(version) {
  s <- dhis2API::dhis2_session(
    paste0("https://play.im.dhis2.org/", version),
    username = "admin", password = "district", keyring = FALSE,
    set_default = FALSE, requests_per_minute = 600, timeout = 300
  )
  tmp <- withr::local_tempdir()
  options(cli.default_handler = function(msg) invisible())

  districts <- NULL
  dplyr::bind_rows(
    check("version", as.character(dhis2API::dhis2_server_version(s))),
    check("org units to level 3, with ancestors", {
      units <- dhis2API::dhis2_org_units(level = 1:3, session = s)
      districts <<- units$ou_uid[units$ou_level == 3][1:4]
      paste(nrow(units), "units;", sum(!is.na(units$level2_name)),
            "with a level 2 name")
    }),
    check("data elements", nrow(dhis2API::dhis2_data_elements(session = s))),
    check("analytics, 12 months by district, split", {
      a <- dhis2API::dhis2_analytics(
        element,
        dhis2API::dhis2_monthly_periods(as.Date("2025-01-01"),
                                        as.Date("2025-12-31")),
        "LEVEL-3", pe_per_request = 6, session = s
      )
      paste(nrow(a), "rows")
    }),
    check("weekly calendar, WeeklySunday", {
      w <- dhis2API::dhis2_check_weekly_periods(
        element, country, years = 2019:2027, session = s
      )
      paste(sum(w$agrees), "of", nrow(w), "years agree")
    }),
    check("weekly calendar, ISO Weekly", {
      w <- dhis2API::dhis2_check_weekly_periods(
        element, country, period_type = "Weekly", years = 2019:2027,
        session = s
      )
      paste(sum(w$agrees), "of", nrow(w), "years agree")
    }),
    check("dataValueSets csv vs json, one district", {
      args <- list(
        org_unit = districts[[1]], start_date = "2024-01-01",
        end_date = "2024-12-31", data_set = data_set, session = s
      )
      csv <- do.call(dhis2API::dhis2_data_values, args)
      json <- do.call(dhis2API::dhis2_data_values, c(args, format = "json"))
      paste("csv", nrow(csv), "json", nrow(json), "rows")
    }),
    check("sync group, 4 districts, 2 workers", {
      con <- dhis2API::dhis2_store_open(fs::path(tmp, "smoke.duckdb"))
      on.exit(dhis2API::dhis2_store_close(con))
      got <- dhis2API::dhis2_sync_group(
        con, "smoke", districts, fs::path(tmp, "interim"),
        window_start = as.Date("2024-10-01"),
        window_end = as.Date("2025-03-31"),
        data_set = data_set, workers = 2, session = s
      )
      plan <- dhis2API::dhis2_plan_run(con, districts, "2024-10-01", "smoke")
      paste(got, "rows; next run is a", plan$mode)
    }),
    check("tracker events", {
      ev <- dhis2API::dhis2_tracker(
        "events", program = program, org_unit = districts[[1]],
        fields = "event,dataValues", page_size = 200, session = s
      )
      paste(nrow(ev), "events;", nrow(dhis2API::dhis2_event_values(ev)),
            "values")
    }),
    check("tracker enrollments", {
      en <- dhis2API::dhis2_tracker(
        "enrollments", program = program, org_unit = districts[[1]],
        fields = "enrollment", page_size = 200, session = s
      )
      paste(nrow(en), "enrollments")
    })
  ) |>
    dplyr::mutate(server = version, .before = 1)
}

results <- purrr::map(versions, smoke) |> purrr::list_rbind()
print(results, n = Inf, width = 200)
