test_that("whole doubles become integers", {
  a <- .coerce_args("forecast", list(h = 12, finetune_steps = 0, freq = "D"))
  expect_identical(a$h, 12L); expect_identical(a$finetune_steps, 0L); expect_identical(a$freq, "D")
})
test_that("NULL args are dropped", {
  expect_false("model" %in% names(.coerce_args("forecast", list(h = 1, model = NULL))))
})
test_that("level becomes a list except for online anomalies", {
  expect_identical(.coerce_args("forecast", list(level = 90))$level, list(90))
  expect_identical(.coerce_args("detect_anomalies_online", list(level = 99))$level, 99)
  expect_identical(.coerce_args("forecast", list(hist_exog_list = "x"))$hist_exog_list, list("x"))
})
test_that("tsibble is flattened", {
  skip_if_not_installed("tsibble")
  ts <- tsibble::as_tsibble(data.frame(unique_id = "a", ds = as.Date("2020-01-01") + 0:4, y = 1:5), key = unique_id, index = ds)
  expect_identical(class(.prep_df(ts)), "data.frame")
})
test_that("non data frames are rejected", {
  expect_error(.prep_df(1:3), "Only data frames, tibbles, and tsibbles are allowed.")
})
test_that("config reads env then options and errors without a key", {
  withr::local_envvar(NIXTLA_API_KEY = NA, NIXTLA_BASE_URL = NA)
  withr::local_options(NIXTLA_API_KEY = NULL, NIXTLA_BASE_URL = NULL)
  expect_error(.client_config(), "Please set your NIXTLA_API_KEY")
  withr::local_options(NIXTLA_API_KEY = "opt")
  expect_identical(.client_config(), list(api_key = "opt", base_url = NULL))
  withr::local_envvar(NIXTLA_API_KEY = "env")
  expect_identical(.client_config()$api_key, "env")
})
test_that("onLoad does not import nixtla eagerly", {
  skip_if_not_installed("callr")
  skip_if_not_installed("pkgload")
  expect_false(callr::r(function(path) {
    pkgload::load_all(path, quiet = TRUE); reticulate::py_available(initialize = FALSE)
  }, args = list(normalizePath(testthat::test_path("../..")))))
})
test_that("id_col NULL adds and drops unique_id", {
  skip_if_no_api()
  df <- nixtlar::electricity[nixtlar::electricity$unique_id == "BE", c("ds", "y")]
  out <- .call_client("forecast", df = df, h = 2, id_col = NULL, freq = "h")
  expect_false("unique_id" %in% names(out)); expect_equal(nrow(out), 2)
})
test_that("pandas result is a plain data.frame", {
  skip_if_no_api()
  out <- .call_client("forecast", df = nixtlar::electricity, h = 2, freq = "h")
  expect_identical(class(out), "data.frame"); expect_null(attr(out, "pandas.index"))
})
