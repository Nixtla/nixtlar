test_that("whole doubles become integers", {
  a <- .coerce_args("forecast", list(h = 12, finetune_steps = 0, freq = "D"))
  expect_identical(a$h, 12L); expect_identical(a$finetune_steps, 0L); expect_identical(a$freq, "D")
})
test_that("NULL args are dropped", {
  expect_false("model" %in% names(.coerce_args("forecast", list(h = 1, model = NULL))))
})
test_that("level becomes a list except for online anomalies", {
  expect_identical(.coerce_args("forecast", list(level = 90))$level, list(90L))
  expect_identical(.coerce_args("detect_anomalies_online", list(level = 99))$level, 99L)
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
  src <- normalizePath(testthat::test_path("../.."))
  if (file.exists(file.path(src, "DESCRIPTION"))) skip_if_not_installed("pkgload")
  expect_false(callr::r(function(path) {
    if (file.exists(file.path(path, "DESCRIPTION"))) {
      pkgload::load_all(path, quiet = TRUE)
    } else {
      loadNamespace("nixtlar")
    }
    reticulate::py_available(initialize = FALSE)
  }, args = list(src)))
})
test_that("load survives Python without nixtla", {
  skip_on_cran()
  skip_if_not_installed("callr")
  has_nixtla <- function(py) system2(py, c("-c", shQuote("import nixtla")), stdout = FALSE, stderr = FALSE) == 0
  cands <- c(Sys.getenv("NIXTLAR_TEST_BARE_PYTHON"),
             tryCatch(reticulate::virtualenv_starter(all = TRUE)$path, error = function(e) character()),
             "/opt/homebrew/bin/python3", "/usr/local/bin/python3", "/usr/bin/python3")
  cands <- cands[nzchar(cands) & file.exists(cands) & !grepl("/.cache/uv/", cands, fixed = TRUE)]
  cands <- Filter(Negate(has_nixtla), cands)
  skip_if(length(cands) == 0, "no Python without nixtla outside the uv cache")
  py <- cands[[1]]
  src <- normalizePath(testthat::test_path("../.."))
  if (file.exists(file.path(src, "DESCRIPTION"))) skip_if_not_installed("pkgload")
  res <- callr::r(function(path, py) {
    reticulate::use_python(py, required = TRUE)
    reticulate::py_config()
    if (file.exists(file.path(path, "DESCRIPTION"))) {
      pkgload::load_all(path, quiet = TRUE)
    } else {
      loadNamespace("nixtlar")
    }
    msg <- tryCatch({ nixtlar:::nixtla_py$NixtlaClient; "" }, error = conditionMessage)
    list(loaded = TRUE, msg = msg)
  }, args = list(src, py))
  expect_true(res$loaded)
  expect_match(res$msg, "nixtla==0.9.0", fixed = TRUE)
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

test_that(".prep_df converts Date columns to POSIXct", {
  out <- .prep_df(data.frame(ds = as.Date("2024-01-01") + 0:1, y = 1:2))
  expect_s3_class(out$ds, "POSIXct")
})

test_that("job_timeout_seconds is an integer arg; huge whole doubles stay double", {
  expect_identical(.coerce_args("forecast", list(job_timeout_seconds = 60))$job_timeout_seconds, 60L)
  expect_identical(.coerce_args("simulate", list(seed = 2^40))$seed, 2^40)
})
test_that("date_features wraps only character values", {
  expect_identical(.coerce_args("forecast", list(date_features = TRUE))$date_features, TRUE)
  expect_identical(.coerce_args("forecast", list(date_features = c("month", "year")))$date_features, list("month", "year"))
  expect_identical(.coerce_args("forecast", list(date_features_to_one_hot = FALSE))$date_features_to_one_hot, FALSE)
  expect_identical(.coerce_args("forecast", list(date_features_to_one_hot = "month"))$date_features_to_one_hot, list("month"))
})
test_that("naive pandas datetimes come back as UTC and the date is unchanged", {
  skip_if_no_python()
  pd <- reticulate::import("pandas")
  r <- .to_r(pd$DataFrame(list(ds = pd$to_datetime(list("2024-01-01")))))
  expect_identical(attr(r$ds, "tzone"), "UTC")
  expect_identical(as.Date(r$ds), as.Date("2024-01-01"))
})
test_that("whole-number level becomes integer", {
  expect_identical(.coerce_args("forecast", list(level = c(80, 95)))$level, list(80L, 95L))
  expect_identical(.coerce_args("detect_anomalies_online", list(level = 99))$level, 99L)
  expect_identical(.coerce_args("forecast", list(level = c(80, 99.5)))$level, list(80, 99.5))
})
test_that("numeric freq becomes integer, character freq untouched", {
  expect_identical(.coerce_args("forecast", list(freq = 1))$freq, 1L)
  expect_identical(.coerce_args("forecast", list(freq = "D"))$freq, "D")
})
test_that("POSIXct keeps wall-clock time", {
  x <- as.POSIXct(c("2024-03-09", "2024-03-10", "2024-03-11"), tz = "America/New_York")
  out <- .prep_df(data.frame(ds = x, y = 1:3))
  expect_identical(attr(out$ds, "tzone"), "UTC")
  expect_identical(format(out$ds, "%Y-%m-%d %H:%M"), c("2024-03-09 00:00", "2024-03-10 00:00", "2024-03-11 00:00"))
})
test_that("tsibble yearmonth index becomes a timestamp", {
  skip_if_not_installed("tsibble")
  df <- data.frame(ds = tsibble::yearmonth(c("2024 Jan", "2024 Feb")), y = 1:2)
  out <- .prep_df(df)
  expect_s3_class(out$ds, "POSIXct")
  expect_identical(format(out$ds, "%Y-%m-%d"), c("2024-01-01", "2024-02-01"))
})

test_that("sub-daily DST data keeps instants: no duplicates, regular steps", {
  x <- seq(as.POSIXct("2024-10-26 12:00", tz = "Europe/Berlin"), by = "hour", length.out = 72)
  out <- .prep_df(data.frame(ds = x, y = 1:72))
  expect_false(anyDuplicated(out$ds) > 0)
  expect_true(all(diff(as.numeric(out$ds)) == 3600))
  expect_null(attr(out, "forced_tz")$ds)
  skip_if_no_python()
  skip_if_not(reticulate::py_module_available("nixtla"))
  pd <- reticulate::import("pandas")
  pp <- reticulate::import("nixtla._preprocessing")
  d <- pd$DataFrame(list(unique_id = rep("a", 72), ds = out$ds, y = out$y))
  expect_no_error(pp$`_validate_freq_regularity`(d, "h", "unique_id", "ds"))
})
test_that("results come back in the caller's zone for date-like input", {
  x <- as.POSIXct(c("2024-03-09", "2024-03-10"), tz = "America/New_York")
  local_mocked_bindings(.get_client = function() list(forecast = function(df, ...) {
    data.frame(ds = as.POSIXct(format(df$ds), tz = "UTC"), y = df$y)
  }), .package = "nixtlar")
  out <- .call_client("forecast", df = data.frame(ds = x, y = 1:2))
  expect_identical(attr(out$ds, "tzone"), "America/New_York")
  expect_identical(format(out$ds, "%H:%M"), c("00:00", "00:00"))
  expect_equal(out$ds, x)
})
