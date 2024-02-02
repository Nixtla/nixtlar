with_mock_dir("../mocks", {
  test_that("timegpt_forecast", {
    skip_if_no_token()
    test_data <- nixtlar::electricity
    response <- timegpt_forecast(test_data, h = 8, id_col = "unique_id", level = c(80,95))
    expect_s3_class(response, "data.frame")
    expect_true(all(c("unique_id", "ds", "TimeGPT") %in% names(response)))
    expect_true(is.numeric(response$TimeGPT))
  })
})
