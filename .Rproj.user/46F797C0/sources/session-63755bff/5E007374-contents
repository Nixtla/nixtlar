with_mock_dir("mocks", {
  test_that("timegpt_anomaly_detection", {
    test_data <- nixtlar::electricity
    response <- timegpt_anomaly_detection(test_data, id_col = "unique_id")
    expect_s3_class(response, "data.frame")
    expect_true(all(c("unique_id", "ds", "y", "anomaly", "TimeGPT-lo-99", "TimeGPT", "TimeGPT-hi-99") %in% names(response)))
    expect_true(all(response$anomaly %in% c(0,1)))
    expect_true(is.numeric(response$TimeGPT))
    expect_true(is.numeric(response$`TimeGPT-lo-99`))
    expect_true(is.numeric(response$`TimeGPT-hi-99`))
  })
})
