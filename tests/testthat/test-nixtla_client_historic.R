fake_hist <- data.frame(
  unique_id = rep(c("a", "b"), each = 4),
  ds = rep(1:4, 2),
  TimeGPT = as.numeric(1:8)
)

test_that("historic calls forecast with h = 1, add_history = TRUE and drops each series' last row", {
  got <- NULL
  local_mocked_bindings(
    .call_client = function(method, ...) { got <<- list(method = method, args = list(...)); fake_hist },
    .package = "nixtlar"
  )
  out <- nixtla_client_historic(data.frame(), level = 80, date_features = TRUE)
  expect_equal(got$method, "forecast")
  expect_equal(got$args$h, 1)
  expect_true(got$args$add_history)
  expect_true(got$args$date_features)
  expect_null(got$args$model)
  expect_equal(out$TimeGPT, c(1, 2, 3, 5, 6, 7))
  expect_s3_class(out, "data.frame")
  expect_false(inherits(out, "grouped_df"))
})

test_that("historic drops only the single last row when id_col = NULL", {
  local_mocked_bindings(
    .call_client = function(method, ...) fake_hist[1:4, c("ds", "TimeGPT")],
    .package = "nixtlar"
  )
  out <- nixtla_client_historic(data.frame(), id_col = NULL)
  expect_equal(out$ds, 1:3)
})

test_that("nixtla_client_historic", {
  skip_if_no_api()
  test_data <- nixtlar::electricity
  response <- nixtla_client_historic(test_data, level = c(80, 95))
  expect_s3_class(response, "data.frame")
  expect_true(all(c("unique_id", "ds", "TimeGPT") %in% names(response)))
  expect_true(is.numeric(response$TimeGPT))
  last_ds <- tapply(as.POSIXct(test_data$ds, tz = "UTC"), test_data$unique_id, max)
  for (id in unique(response$unique_id)) {
    expect_true(all(as.POSIXct(response$ds[response$unique_id == id], tz = "UTC") <= last_ds[[id]]))
  }
})
