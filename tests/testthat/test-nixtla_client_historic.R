with_mock_dir("../mocks", {
  test_that("nixtla_client_historic", {
    skip_if_no_token()
    test_data <- nixtlar::electricity
    response <- nixtla_client_historic(test_data, id_col = "unique_id", level = c(80,95))
    expect_s3_class(response, "data.frame")
    expect_true(all(c("unique_id", "ds", "TimeGPT") %in% names(response)))
    expect_true(is.numeric(response$TimeGPT))
  })
})




