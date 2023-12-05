with_mock_dir("timegpt_forecast", {
  test_that("test forecast", {
    test_data <- read.csv('https://raw.githubusercontent.com/Nixtla/transfer-learning-time-series/main/datasets/electricity-short.csv')
    response <- timegpt_forecast(test_data, id_col="unique_id")
    expect_s3_class(response, "data.frame") # output should be a data frame given that test_data is a data frame
    expect_true(all(c("unique_id", "ds", "TimeGPT") %in% names(response))) # should at least have columns ds and TimeGPT
    expect_type(response$TimeGPT, "double") #TimeGPT should be numeric
  })
})
