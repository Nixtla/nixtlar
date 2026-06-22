with_mock_dir("../mocks", {
  test_that("nixtla_client_forecast", {
    skip_if_no_token()
    test_data <- nixtlar::electricity
    response <- nixtla_client_forecast(test_data, h = 8, id_col = "unique_id", level = c(80,95))
    expect_s3_class(response, "data.frame")
    expect_true(all(c("unique_id", "ds", "TimeGPT") %in% names(response)))
    expect_true(is.numeric(response$TimeGPT))
  })

  test_that("nixtla_client_forecast with add_history=TRUE returns fitted + future values", {
    skip_if_no_token()
    test_data <- nixtlar::electricity

    fcst_only <- nixtla_client_forecast(test_data, h = 8, id_col = "unique_id")
    fcst_hist <- nixtla_client_forecast(test_data, h = 8, id_col = "unique_id", add_history = TRUE)

    # Same shape as a plain forecast
    expect_s3_class(fcst_hist, "data.frame")
    expect_true(all(c("unique_id", "ds", "TimeGPT") %in% names(fcst_hist)))
    expect_true(is.numeric(fcst_hist$TimeGPT))

    # add_history appends in-sample fitted values, so there are strictly more rows
    expect_gt(nrow(fcst_hist), nrow(fcst_only))

    # Fitted values reach back before the forecast horizon starts
    expect_lt(min(fcst_hist$ds), min(fcst_only$ds))
  })
})

test_that("add_history=TRUE forwards `model` to nixtla_client_historic", {
  # Verifies the wiring added in nixtla_client_forecast(): when add_history=TRUE,
  # the `model` argument must be passed through to nixtla_client_historic().
  # Runs fully offline by mocking the network seams, so no token is needed.

  h <- 4
  # Minimal single series; long enough to survive the input-size checks.
  test_data <- data.frame(
    unique_id = "ts_0",
    ds = as.character(seq(as.Date("2020-01-01"), by = "day", length.out = 10)),
    y = as.numeric(1:10)
  )

  captured_model <- NULL

  local_mocked_bindings(
    # Capture the model handed to the historic call; return a minimal frame so
    # the caller's bind_rows() succeeds.
    nixtla_client_historic = function(..., model = NULL) {
      captured_model <<- model
      data.frame(unique_id = character(), ds = as.Date(character()), TimeGPT = numeric())
    },
    # Avoid needing NIXTLA_API_KEY / base_url.
    .get_client_steup = function() list(base_url = "http://localhost/", api_key = "test"),
    # Avoid the model_params network call.
    .get_model_params = function(model, freq) list(input_size = 2L, horizon = h)
  )

  # Mock the httr2 request/perform/parse seam so no real HTTP happens. The
  # forecast builds `fc` from resp$mean, so return `h` values.
  local_mocked_bindings(
    req_perform = function(req, ...) structure(list(), class = "httr2_response"),
    resp_body_json = function(resp, ...) list(mean = as.list(rep(0, h))),
    .package = "httr2"
  )

  result <- nixtla_client_forecast(
    test_data, h = h, id_col = "unique_id",
    add_history = TRUE, model = "timegpt-1-long-horizon"
  )

  expect_equal(captured_model, "timegpt-1-long-horizon")
})
