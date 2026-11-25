test_that("cross_validation calls the Python cross_validation method and forwards args and ...", {
  got <- NULL
  local_mocked_bindings(
    .call_client = function(method, ...) { got <<- list(method = method, args = list(...)); data.frame() },
    .package = "nixtlar"
  )
  nixtla_client_cross_validation(data.frame(), h = 4, n_windows = 2, date_features = TRUE)
  expect_equal(got$method, "cross_validation")
  expect_equal(got$args$h, 4)
  expect_equal(got$args$n_windows, 2)
  expect_true(got$args$date_features)
  expect_null(got$args$model)
})

test_that("nixtla_client_cross_validation", {
  skip_if_no_api()
  test_data <- nixtlar::electricity
  response <- nixtla_client_cross_validation(test_data, h = 4, id_col = "unique_id", n_windows = 2)
  expect_s3_class(response, "data.frame")
  expect_true(all(c("unique_id", "ds", "cutoff", "y", "TimeGPT") %in% names(response)))
  expect_true(is.numeric(response$TimeGPT))
  expect_equal(nrow(response), 4 * 2 * length(unique(test_data$unique_id)))
})
