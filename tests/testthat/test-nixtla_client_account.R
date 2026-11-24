test_that("usage and list_models call the client", {
  got <- character()
  local_mocked_bindings(
    .call_client = function(method, ...) { got <<- c(got, method); list("timegpt-1", "timegpt-1-long-horizon") },
    .package = "nixtlar"
  )
  expect_equal(nixtla_client_list_models(), c("timegpt-1", "timegpt-1-long-horizon"))
  nixtla_client_usage()
  expect_equal(got, c("list_models", "usage"))
})

test_that("usage and list_models", {
  skip_if_no_api()
  expect_type(nixtla_client_usage(), "list")
  m <- nixtla_client_list_models()
  expect_type(m, "character")
  expect_gt(length(m), 0)
})
