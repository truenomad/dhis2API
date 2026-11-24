test_that("a CSV download returns the data_values columns", {
  log <- local_fake_server(function(path, query) {
    csv_resp(fake_values_csv(query$orgUnit, n = 3))
  })
  out <- dhis2_data_values(
    "OU_D1", "2001-01-01", "2001-12-31", data_element_group = "DEG_1",
    last_updated_duration = "8d", session = fake_session()
  )
  expect_equal(names(out), names(.dhis2_store_schema$data_values))
  expect_equal(nrow(out), 3)
  expect_equal(out$attr_option_combo, rep("AOC_1", 3))
  expect_false(any(out$deleted))
  q <- log$calls[[1]]$query
  expect_equal(log$calls[[1]]$path, "dataValueSets.csv")
  expect_equal(q$children, "true")
  expect_equal(q$includeDeleted, "true")
  expect_equal(q$lastUpdatedDuration, "8d")
  expect_null(q$dataSet)
})

test_that("older CSV headers are read too", {
  text <- paste(
    "dataelement,period,orgunit,catoptcombo,attroptcombo,value,deleted",
    "DE_1,200101,OU_D1,COC_1,AOC_1,4,true", sep = "\n"
  )
  out <- .dhis2_values_from_csv(text)
  expect_equal(out$cat_option_combo, "COC_1")
  expect_true(out$deleted)
  expect_true(is.na(out$stored_by))
})

test_that("a CSV without the core columns aborts", {
  text <- "elem,pe,ou,val\nDE_1,200101,OU_D1,4"
  expect_error(.dhis2_values_from_csv(text), "format = \"json\"")
})

test_that("an empty CSV gives a typed empty table", {
  out <- .dhis2_values_from_csv(fake_values_csv("OU_D1", n = 0))
  expect_equal(nrow(out), 0)
  expect_type(out$deleted, "logical")
  expect_equal(nrow(.dhis2_values_from_csv("")), 0)
})

test_that("a JSON download keeps created and fills missing fields", {
  local_fake_server(function(path, query) {
    json_resp(list(dataValues = list(
      list(
        dataElement = "DE_1", period = "200101", orgUnit = "OU_D1",
        categoryOptionCombo = "COC_1", attributeOptionCombo = "AOC_1",
        value = "7", created = "2001-02-01T00:00:00.000"
      )
    )))
  })
  out <- dhis2_data_values(
    "OU_D1", "2001-01-01", "2001-12-31", data_set = "DS_1",
    format = "json", session = fake_session()
  )
  expect_equal(out$created, "2001-02-01T00:00:00.000")
  expect_false(out$deleted)
  expect_true(is.na(out$followup))
})

test_that("a download needs a data set or an element group", {
  expect_error(
    dhis2_data_values("OU_D1", "2001-01-01", "2001-12-31",
                      session = fake_session()),
    "data_set"
  )
})
