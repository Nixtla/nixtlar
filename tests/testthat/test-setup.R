test_that("setup stores key and base_url in options", {
  withr::local_options(NIXTLA_API_KEY = NULL, NIXTLA_BASE_URL = NULL)
  suppressMessages(nixtla_client_setup(base_url = "https://x.test/", api_key = "k"))
  expect_identical(getOption("NIXTLA_API_KEY"), "k")
  expect_identical(getOption("NIXTLA_BASE_URL"), "https://x.test/")
})

test_that("setup without base_url leaves it unset", {
  withr::local_options(NIXTLA_API_KEY = NULL, NIXTLA_BASE_URL = NULL)
  suppressMessages(nixtla_client_setup(api_key = "k"))
  expect_null(getOption("NIXTLA_BASE_URL"))
})

test_that("validate returns FALSE on a bad key", {
  skip_if_no_api()
  withr::local_envvar(NIXTLA_API_KEY = "bad-key")
  expect_false(nixtla_validate_api_key())
})

test_that("validate returns TRUE on a good key", {
  skip_if_no_api()
  expect_true(nixtla_validate_api_key())
})
