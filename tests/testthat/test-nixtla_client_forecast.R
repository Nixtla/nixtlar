test_that("forecast calls the Python forecast method and forwards args and ...", {
  got <- NULL
  local_mocked_bindings(
    .call_client = function(method, ...) { got <<- list(method = method, args = list(...)); data.frame() },
    .package = "nixtlar"
  )
  nixtla_client_forecast(data.frame(), h = 4, level = c(80, 95), date_features = TRUE)
  expect_equal(got$method, "forecast")
  expect_equal(got$args$h, 4)
  expect_equal(got$args$level, c(80, 95))
  expect_true(got$args$date_features)
  expect_null(got$args$model)
  expect_equal(got$args$id_col, "unique_id")
})

test_that("nixtla_client_forecast", {
  skip_if_no_api()
  test_data <- nixtlar::electricity
  response <- nixtla_client_forecast(test_data, h = 8, id_col = "unique_id", level = c(80, 95))
  expect_s3_class(response, "data.frame")
  expect_true(all(c("unique_id", "ds", "TimeGPT", "TimeGPT-lo-80", "TimeGPT-hi-95") %in% names(response)))
  expect_true(is.numeric(response$TimeGPT))
  expect_equal(nrow(response), 8 * length(unique(test_data$unique_id)))
})

test_that("nixtla_client_forecast with add_history=TRUE returns fitted + future values", {
  skip_if_no_api()
  test_data <- nixtlar::electricity

  fcst_only <- nixtla_client_forecast(test_data, h = 8, id_col = "unique_id")
  fcst_hist <- nixtla_client_forecast(test_data, h = 8, id_col = "unique_id", add_history = TRUE)

  expect_s3_class(fcst_hist, "data.frame")
  expect_true(all(c("unique_id", "ds", "TimeGPT") %in% names(fcst_hist)))
  expect_true(is.numeric(fcst_hist$TimeGPT))
  expect_gt(nrow(fcst_hist), nrow(fcst_only))
  expect_lt(min(fcst_hist$ds), min(fcst_only$ds))
})

test_that("nixtla_client_forecast accepts date_features via ...", {
  skip_if_no_api()
  response <- nixtla_client_forecast(nixtlar::electricity, h = 8, id_col = "unique_id",
                                     date_features = TRUE)
  expect_s3_class(response, "data.frame")
  expect_true("TimeGPT" %in% names(response))
})
