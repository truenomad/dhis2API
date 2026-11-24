test_that("filters are written in the API's syntax", {
  expect_equal(dhis2_filter("level", "eq", 3), "level:eq:3")
  expect_equal(dhis2_filter("id", "in", c("a", "b")), "id:in:[a,b]")
  expect_equal(dhis2_filter("code", "null"), "code:null")
})

fake_metadata_route <- function(path, query) {
  switch(path,
    "organisationUnitLevels.json" = json_resp(fake_levels_body),
    "organisationUnits.json" = json_resp(fake_units_body),
    "dataElements.json" = json_resp(list(
      pager = list(page = 1, pageCount = 1),
      dataElements = list(
        list(
          id = "DE_1", name = "Fake doses", valueType = "NUMBER",
          aggregationType = "SUM",
          categoryCombo = list(id = "CC_1", name = "Fake combo")
        ),
        list(
          id = "DE_2", name = "Fake rate", valueType = "PERCENTAGE",
          aggregationType = "AVERAGE",
          categoryCombo = list(id = "CC_1", name = "Fake combo")
        )
      )
    ))
  )
}

test_that("org units carry their ancestors at every level", {
  local_fake_server(fake_metadata_route)
  out <- dhis2_org_units(session = fake_session())
  d1 <- dplyr::filter(out, ou_uid == "OU_D1")
  expect_equal(d1$ou_level_name, "District")
  expect_equal(d1$level1_name, "Fakeland")
  expect_equal(d1$level2_uid, "OU_P1")
  expect_equal(d1$level3_name, "District A1")
  expect_equal(d1$opening_date, as.Date("2001-01-01"))
  root <- dplyr::filter(out, ou_uid == "OU_C")
  expect_true(is.na(root$parent_uid))
  expect_true(is.na(root$level2_uid))
})

test_that("a level filter is sent to the server", {
  log <- local_fake_server(fake_metadata_route)
  dhis2_org_units(level = 3, ancestors = FALSE, session = fake_session())
  units_call <- purrr::keep(
    log$calls, \(x) x$path == "organisationUnits.json"
  )[[1]]
  expect_equal(units_call$query$filter, "level:in:[3]")
  expect_false(grepl("ancestors", units_call$query$fields))
})

test_that("metadata is cached on disk and reused", {
  log <- local_fake_server(fake_metadata_route)
  s <- fake_session(cache_dir = withr::local_tempdir())
  first <- dhis2_data_elements(session = s)
  second <- dhis2_data_elements(session = s)
  expect_identical(first, second)
  expect_length(log$calls, 1)
  expect_equal(dhis2_cache_clear(session = s), 1)
  dhis2_data_elements(session = s)
  expect_length(log$calls, 2)
})

test_that("each account has its own cache, and tokens count as accounts", {
  log <- local_fake_server(fake_metadata_route)
  dir <- withr::local_tempdir()
  dhis2_data_elements(session = fake_session(cache_dir = dir))
  expect_length(log$calls, 1)
  dhis2_data_elements(session = fake_session_as(cache_dir = dir))
  expect_length(log$calls, 2)
  dhis2_data_elements(session = fake_session_as(cache_dir = dir))
  expect_length(log$calls, 2)

  by_token <- function(token) {
    dhis2_session(
      fake_base, token = token, auth = "token", keyring = FALSE,
      set_default = FALSE, requests_per_minute = Inf, max_tries = 1,
      cache_dir = dir
    )
  }
  dhis2_data_elements(session = by_token("fake_token_1"))
  expect_length(log$calls, 3)
  dhis2_data_elements(session = by_token("fake_token_1"))
  expect_length(log$calls, 3)
  dhis2_data_elements(session = by_token("fake_token_2"))
  expect_length(log$calls, 4)
  files <- fs::path_file(fs::dir_ls(dir))
  expect_false(any(grepl("fake_token|fake_pass|other_pass", files)))
})

test_that("the cache of the default session is cleared", {
  local_fake_server(fake_metadata_route)
  dir <- withr::local_tempdir()
  withr::defer(dhis2_clear_session())
  dhis2_set_session(fake_session(cache_dir = dir))
  dhis2_data_elements()
  expect_length(fs::dir_ls(dir), 1)
  expect_equal(dhis2_cache_clear(), 1)
  expect_length(fs::dir_ls(dir), 0)

  # with no default session, the option names the folder; no login needed
  dhis2_clear_session()
  withr::local_envvar(
    DHIS2_BASE_URL = "", DHIS2_USERNAME = "", DHIS2_PASSWORD = "",
    DHIS2_TOKEN = ""
  )
  fs::file_create(fs::path(dir, "stale.qs2"))
  withr::local_options(dhis2api.cache_dir = dir)
  expect_equal(dhis2_cache_clear(), 1)
  expect_equal(dhis2_cache_clear(), 0)
})

test_that("data elements come back in snake case", {
  local_fake_server(fake_metadata_route)
  out <- dhis2_data_elements(session = fake_session())
  expect_equal(out$de_uid, c("DE_1", "DE_2"))
  expect_equal(out$cat_combo_uid, c("CC_1", "CC_1"))
  expect_true(all(is.na(out$de_code)))
})

test_that("names are added beside each uid column", {
  values <- tibble::tibble(
    data_element = c("DE_1", "DE_X"), org_unit = "OU_D1",
    cat_option_combo = "COC_1", value = c("1", "2")
  )
  out <- dhis2_add_names(
    values,
    elements = tibble::tibble(de_uid = "DE_1", de_name = "Fake doses"),
    org_units = tibble::tibble(ou_uid = "OU_D1", ou_name = "District A1"),
    combos = tibble::tibble(coc_uid = "COC_1", coc_name = "Fake option")
  )
  expect_equal(out$data_element_name, c("Fake doses", NA))
  expect_equal(out$org_unit_name, c("District A1", "District A1"))
  expect_equal(names(out)[1:2], c("data_element", "data_element_name"))
})

test_that("element lists abort when empty", {
  local_fake_server(function(path, query) {
    json_resp(list(dataSetElements = list()))
  })
  expect_error(
    dhis2_data_set_elements("DS_1", session = fake_session()),
    "no data elements"
  )
})
