test_that("simulate forwards args and ...", {
  got <- NULL
  local_mocked_bindings(
    .call_client = function(method, ...) { got <<- list(method = method, args = list(...)); data.frame() },
    .package = "nixtlar"
  )
  nixtla_client_simulate(data.frame(), h = 4, n_paths = 3, num_partitions = 2)
  expect_equal(got$method, "simulate")
  expect_equal(got$args$h, 4)
  expect_equal(got$args$n_paths, 3)
  expect_equal(got$args$num_partitions, 2)
})

test_that("nixtla_client_simulate", {
  skip_if_no_api()
  r <- nixtla_client_simulate(nixtlar::electricity, h = 4, n_paths = 3)
  expect_s3_class(r, "data.frame")
  expect_true(all(c("unique_id", "ds") %in% names(r)))
  expect_gt(nrow(r), 0)
})
