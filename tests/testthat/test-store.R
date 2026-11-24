fake_store <- function(env = parent.frame()) {
  path <- fs::path(withr::local_tempdir(.local_envir = env), "fake.duckdb")
  con <- dhis2_store_open(path)
  withr::defer(dhis2_store_close(con), envir = env)
  list(path = path, con = con)
}

fake_dv <- function(element, value, deleted = FALSE) {
  .dhis2_values_from_csv(fake_values_csv("OU_D1", n = 1)) |>
    dplyr::mutate(
      data_element = !!element, value = !!value, deleted = !!deleted
    )
}

test_that("opening creates every table", {
  store <- fake_store()
  expect_setequal(
    DBI::dbListTables(store$con), names(.dhis2_store_schema)
  )
  expect_error(
    dhis2_store_open(fs::path(tempdir(), "none.duckdb"), read_only = TRUE),
    "No store"
  )
})

test_that("saved files insert or replace rows and are removed after loading", {
  store <- fake_store()
  dir <- withr::local_tempdir()
  .dhis2_write_parquet(fake_dv("DE_1", "5"), fs::path(dir, "u_A.parquet"))
  expect_equal(dhis2_store_load(store$con, dir, "data_values"), 1)
  expect_length(fs::dir_ls(dir), 0)

  .dhis2_write_parquet(fake_dv("DE_1", "6"), fs::path(dir, "u_A.parquet"))
  dhis2_store_load(store$con, dir, "data_values")
  held <- DBI::dbGetQuery(store$con, "SELECT value FROM data_values")
  expect_equal(held$value, "6")
  expect_equal(dhis2_store_load(store$con, fs::path(dir, "none")), 0)
})

test_that("the summary gives one row per value table", {
  store <- fake_store()
  dir <- withr::local_tempdir()
  .dhis2_write_parquet(fake_dv("DE_1", "5", deleted = TRUE),
                       fs::path(dir, "u_A.parquet"))
  dhis2_store_load(store$con, dir, "data_values")
  out <- dhis2_store_summary(store$con)
  expect_equal(nrow(out), 3)
  dv <- dplyr::filter(out, value_table == "data_values")
  expect_equal(dv$values_stored, 1)
  expect_equal(dv$deleted_values, 1)
})

test_that("values come from data_values first and drop deleted rows", {
  store <- fake_store()
  dir <- withr::local_tempdir()
  .dhis2_write_parquet(
    dplyr::bind_rows(
      fake_dv("DE_1", "5"),
      fake_dv("DE_2", "9", deleted = TRUE) |>
        dplyr::mutate(period = "200102")
    ),
    fs::path(dir, "u_A.parquet")
  )
  dhis2_store_load(store$con, dir, "data_values")
  .dhis2_write_parquet(
    tibble::tibble(
      data_element = c("DE_1", "DE_3"), period = "200101",
      org_unit = "OU_D1", cat_option_combo = "COC_1", value = c("99", "3")
    ),
    fs::path(dir, "a.parquet")
  )
  dhis2_store_load(store$con, dir, "analytics_values")
  dhis2_store_close(store$con)

  out <- dhis2_store_values(
    store$path, c("DE_1", "DE_2", "DE_3"),
    cols = c("data_element", "value", "stored_by")
  ) |>
    dplyr::arrange(data_element)
  expect_equal(out$data_element, c("DE_1", "DE_3"))
  expect_equal(out$value, c(5, 3))
  expect_equal(out$stored_by, c("fake_user", NA))

  idx <- dhis2_store_element_index(store$path)
  expect_equal(idx$analytics_values, c("DE_1", "DE_3"))
  expect_equal(nrow(dhis2_store_subannual(store$path)), 0)
})

test_that("sub-annual values filter by period type", {
  store <- fake_store()
  dir <- withr::local_tempdir()
  d <- tibble::tibble(
    dx = "DE_1", pe = c("2026SunW1", "20260105"), ou = "OU_D1",
    co = "COC_1", value = "2"
  )
  .dhis2_write_parquet(
    dplyr::bind_rows(
      dhis2_analytics_to_subannual(d[1, ], "WeeklySunday"),
      dhis2_analytics_to_subannual(d[2, ], "Daily")
    ),
    fs::path(dir, "s.parquet")
  )
  dhis2_store_load(store$con, dir, "analytics_subannual", delete_files = FALSE)
  expect_length(fs::dir_ls(dir), 1)
  dhis2_store_close(store$con)
  out <- dhis2_store_subannual(
    store$path, period_type = "WeeklySunday", years = 2026
  )
  expect_equal(out$period, "2026SunW1")
  expect_equal(out$value, 2)
})

test_that("a snapshot holds every committed row, even with a writer open", {
  quiet_cli()
  store <- fake_store()
  dir <- withr::local_tempdir()
  DBI::dbExecute(store$con, "CHECKPOINT")
  DBI::dbAppendTable(store$con, "data_values", fake_dv("DE_1", "5"))
  dest <- fs::path(dir, "copy.duckdb")
  dhis2_store_snapshot(store$path, dest, con = store$con)
  expect_true(DBI::dbIsValid(store$con))
  copy <- dhis2_store_open(dest, read_only = TRUE)
  expect_equal(
    DBI::dbGetQuery(copy, "SELECT count(*) AS n FROM data_values")$n, 1
  )
  dhis2_store_close(copy)

  # a closed source, copied through a connection of the function's own,
  # replacing the earlier copy
  DBI::dbAppendTable(store$con, "data_values", fake_dv("DE_2", "6"))
  dhis2_store_close(store$con)
  dhis2_store_snapshot(store$path, dest)
  copy <- dhis2_store_open(dest, read_only = TRUE)
  expect_equal(
    DBI::dbGetQuery(copy, "SELECT count(*) AS n FROM data_values")$n, 2
  )
  dhis2_store_close(copy)
  expect_error(
    dhis2_store_snapshot(store$path, store$path), "different file"
  )
  expect_error(
    dhis2_store_snapshot(fs::path(dir, "none.duckdb"), dest), "No store"
  )
})

test_that("reads see rows written after the store was first read", {
  store <- fake_store()
  DBI::dbAppendTable(store$con, "data_values", fake_dv("DE_1", "5"))
  dhis2_store_close(store$con)
  expect_equal(dhis2_store_element_index(store$path)$data_values, "DE_1")
  expect_equal(nrow(dhis2_store_values(store$path, "DE_1")), 1)

  con <- dhis2_store_open(store$path)
  DBI::dbAppendTable(con, "data_values", fake_dv("DE_2", "6"))
  dhis2_store_close(con)
  expect_equal(
    dhis2_store_element_index(store$path)$data_values, c("DE_1", "DE_2")
  )
  out <- dhis2_store_values(store$path, "DE_2")
  expect_equal(out$value, 6)

  # a replaced file is read afresh too
  other <- fake_store()
  DBI::dbAppendTable(other$con, "data_values", fake_dv("DE_9", "9"))
  dhis2_store_close(other$con)
  fs::file_copy(other$path, store$path, overwrite = TRUE)
  expect_equal(
    dhis2_store_element_index(store$path, refresh = TRUE)$data_values, "DE_9"
  )
  expect_equal(nrow(dhis2_store_values(store$path, "DE_1")), 0)
})

test_that("a missing store is an error, not an empty index", {
  expect_error(
    dhis2_store_element_index(fs::path(tempdir(), "none.duckdb")),
    "No store"
  )
})

test_that("the requested columns are returned with or without value", {
  store <- fake_store()
  DBI::dbAppendTable(store$con, "data_values", fake_dv("DE_1", "5"))
  dhis2_store_close(store$con)
  only <- dhis2_store_values(store$path, "DE_1", cols = "data_element")
  expect_equal(names(only), "data_element")
  expect_equal(only$data_element, "DE_1")
  two <- dhis2_store_values(store$path, "DE_1", cols = c("value", "period"))
  expect_equal(names(two), c("value", "period"))
  expect_equal(two$value, 5)
  none <- dhis2_store_values(store$path, "DE_X", cols = c("period", "value"))
  expect_equal(names(none), c("period", "value"))
  expect_equal(nrow(none), 0)
  expect_type(none$value, "double")
  expect_type(dhis2_store_values(store$path, "DE_X")$period, "character")
})

test_that("an older store gains the columns it lacks", {
  store <- fake_store()
  DBI::dbExecute(store$con, "ALTER TABLE download_log DROP COLUMN selection")
  dhis2_store_close(store$con)
  con <- dhis2_store_open(store$path)
  withr::defer(dhis2_store_close(con))
  expect_equal(
    DBI::dbListFields(con, "download_log"),
    names(.dhis2_store_schema$download_log)
  )
})

test_that("only SUM elements pass the summable check", {
  elements <- tibble::tibble(
    de_uid = c("DE_1", "DE_2"), de_name = c("Fake doses", "Fake rate"),
    value_type = "NUMBER", aggregation_type = c("SUM", "AVERAGE")
  )
  expect_true(dhis2_assert_summable(elements, "DE_1"))
  expect_error(dhis2_assert_summable(elements, c("DE_1", "DE_2")),
               "not SUM")
  expect_equal(nrow(dhis2_element_agg(elements)), 2)
})
