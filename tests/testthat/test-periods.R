test_that("monthly ids cover the window", {
  expect_equal(
    dhis2_monthly_periods(as.Date("2025-11-15"), as.Date("2026-02-01")),
    c("202511", "202512", "202601", "202602")
  )
})

test_that("ISO weekly years have 53 weeks where the calendar says so", {
  expect_equal(dhis2_weeks_in_year(c(2020, 2021, 2026), "Weekly"),
               c(53L, 52L, 53L))
  expect_equal(dhis2_weekly_period_start("2026W1"), as.Date("2025-12-29"))
})

test_that("a WeeklySunday year opens on the Sunday of the week holding 4 Jan", {
  expect_equal(dhis2_week_year_start(2026), as.Date("2026-01-04"))
  expect_equal(dhis2_week_year_start(2027), as.Date("2027-01-03"))
  expect_equal(
    dhis2_weekly_period_start(c("2026SunW1", "2026SunW2")),
    as.Date(c("2026-01-04", "2026-01-11"))
  )
  expect_error(dhis2_weekly_period_start("202601"), "Not a weekly")
  expect_error(dhis2_week_year_start(2026, "Fortnightly"), "Unknown weekly")
})

test_that("weekly ids never name a week the year lacks", {
  ids <- dhis2_weekly_periods(as.Date("2020-01-01"), as.Date("2027-12-31"))
  years <- as.integer(substr(ids, 1, 4))
  weeks <- as.integer(sub("^.*SunW", "", ids))
  last_week <- tapply(weeks, years, max)
  expect_equal(
    as.vector(last_week),
    dhis2_weeks_in_year(as.integer(names(last_week)))
  )
  expect_false(anyDuplicated(ids) > 0)
})

test_that("weekly ids stop at the week open on the end date", {
  ids <- dhis2_weekly_periods(as.Date("2026-01-01"), as.Date("2026-01-20"))
  expect_equal(ids, c("2026SunW1", "2026SunW2", "2026SunW3"))
})

test_that("windows split by calendar year", {
  chunks <- dhis2_period_chunks(as.Date("2024-06-01"), as.Date("2026-03-31"))
  expect_equal(purrr::map_chr(chunks, "label"), c("2024", "2025", "2026"))
  expect_equal(chunks[[1]]$start, as.Date("2024-06-01"))
  expect_equal(chunks[[3]]$end, as.Date("2026-03-31"))
})

test_that("vectors chunk to a size", {
  expect_equal(.dhis2_chunk(1:5, 2), list(1:2, 3:4, 5L))
  expect_equal(.dhis2_chunk(1:5), list(1:5))
  expect_equal(.dhis2_chunk(character()), list())
})
