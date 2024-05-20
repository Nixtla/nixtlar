with_mock_dir("../mocks", {
  test_that("nixtla_client_cross_validation", {
    skip_if_no_token()
    test_data <- nixtlar::electricity
    response <- nixtla_client_cross_validation(test_data, h = 8, id_col = "unique_id", n_windows = 5)
    expect_s3_class(response, "data.frame")
    expect_true(all(c("unique_id", "ds", "cutoff", "y", "TimeGPT") %in% names(response)))
    expect_true(is.numeric(response$TimeGPT))
  })
})
