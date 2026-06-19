with_mock_dir("../mocks", {
  test_that("nixtla_client_forecast", {
    skip_if_no_token()
    test_data <- nixtlar::electricity
    response <- nixtla_client_forecast(test_data, h = 8, id_col = "unique_id", level = c(80,95))
    expect_s3_class(response, "data.frame")
    expect_true(all(c("unique_id", "ds", "TimeGPT") %in% names(response)))
    expect_true(is.numeric(response$TimeGPT))
  })

  test_that("nixtla_client_forecast with add_history=TRUE returns fitted + future values", {
    skip_if_no_token()
    test_data <- nixtlar::electricity

    fcst_only <- nixtla_client_forecast(test_data, h = 8, id_col = "unique_id")
    fcst_hist <- nixtla_client_forecast(test_data, h = 8, id_col = "unique_id", add_history = TRUE)

    # Same shape as a plain forecast
    expect_s3_class(fcst_hist, "data.frame")
    expect_true(all(c("unique_id", "ds", "TimeGPT") %in% names(fcst_hist)))
    expect_true(is.numeric(fcst_hist$TimeGPT))

    # add_history appends in-sample fitted values, so there are strictly more rows
    expect_gt(nrow(fcst_hist), nrow(fcst_only))

    # Fitted values reach back before the forecast horizon starts
    expect_lt(min(fcst_hist$ds), min(fcst_only$ds))
  })
})
