test_that("detect_anomalies requires h and detection_size", {
  expect_error(nixtla_client_detect_anomalies(nixtlar::electricity), 'argument "h" is missing')
  expect_error(nixtla_client_detect_anomalies(nixtlar::electricity, h = 24), 'argument "detection_size" is missing')
})

test_that("detect_anomalies calls detect_anomalies_online and forwards args and ...", {
  got <- NULL
  local_mocked_bindings(
    .call_client = function(method, ...) { got <<- list(method = method, args = list(...)); data.frame() },
    .package = "nixtlar"
  )
  nixtla_client_detect_anomalies(data.frame(), h = 24, detection_size = 48, level = 99, num_partitions = 2)
  expect_equal(got$method, "detect_anomalies_online")
  expect_equal(got$args$h, 24)
  expect_equal(got$args$detection_size, 48)
  expect_equal(got$args$level, 99)
  expect_equal(got$args$num_partitions, 2)
  expect_null(got$args$model)
})

test_that("online detection flags the trailing window", {
  skip_if_no_api()
  out <- nixtla_client_detect_anomalies(nixtlar::electricity, h = 24, detection_size = 48, level = 99)
  expect_true(all(c("unique_id", "ds", "y", "TimeGPT", "anomaly") %in% names(out)))
  expect_equal(nrow(out), 48 * dplyr::n_distinct(nixtlar::electricity$unique_id))
})
