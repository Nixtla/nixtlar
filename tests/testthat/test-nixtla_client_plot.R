test_that("plot handles online anomaly output (logical anomaly column)", {
  df <- data.frame(unique_id = "a", ds = as.POSIXct("2020-01-01", tz = "UTC") + 3600 * 0:9, y = 1:10)
  fc <- transform(df[7:10, ], TimeGPT = y, `TimeGPT-lo-99` = y - 1, `TimeGPT-hi-99` = y + 1,
                  anomaly = c(FALSE, TRUE, FALSE, FALSE), anomaly_score = 0, check.names = FALSE)
  expect_s3_class(nixtla_client_plot(df, fc, plot_anomalies = TRUE), "ggplot")
})
