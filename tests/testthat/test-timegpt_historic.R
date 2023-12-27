# with_mock_dir("mocks", {
#   test_that("test historic", {
#     test_data <- nixtlar::electricity
#     response <- timegpt_historic(test_data, id_col="unique_id")
#     expect_s3_class(response, "data.frame") # output should be a data frame given that nixtlar::electricity is a data frame
#     expect_true(all(c("unique_id", "ds", "TimeGPT") %in% names(response))) # should have columns ds and TimeGPT
#     expect_type(response$TimeGPT, "double") #TimeGPT should be numeric
#   })
# })
