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

test_that("validate: NA on Azure, FALSE on Python errors, R errors re-raised", {
  fake <- function(cls) function() list(validate_api_key = function(log) {
    stop(structure(class = c(cls, "error", "condition"),
                   list(message = "boom", call = NULL)))
  })
  local_mocked_bindings(.get_client = fake("python.builtin.NotImplementedError"))
  expect_message(res <- nixtla_validate_api_key(), "Azure")
  expect_identical(res, NA)
  skip_if_no_python()
  local_mocked_bindings(.get_client = function() list(validate_api_key = function(log) reticulate::import_builtins()$int("x")))
  expect_false(nixtla_validate_api_key())
  local_mocked_bindings(.get_client = function() stop("Please set your NIXTLA_API_KEY"))
  expect_error(nixtla_validate_api_key(), "NIXTLA_API_KEY")
})
