test_that("explain forwards args and ...", {
  got <- NULL
  local_mocked_bindings(
    .call_client = function(method, ...) { got <<- list(method = method, args = list(...)); data.frame() },
    .package = "nixtlar"
  )
  nixtla_client_explain(data.frame(), freq = "h", h = 2)
  expect_equal(got$method, "explain")
  expect_equal(got$args$freq, "h")
  expect_equal(got$args$h, 2)
})

test_that("nixtla_client_explain", {
  skip_if_no_api()
  r <- nixtla_client_explain(nixtlar::electricity_exo_vars)
  expect_s3_class(r, "data.frame")
  expect_gt(nrow(r), 0)
})
