test_that("finetune wrappers forward to the right methods", {
  got <- list()
  local_mocked_bindings(
    .call_client = function(method, ...) { got[[method]] <<- list(...); "x" },
    .package = "nixtlar"
  )
  nixtla_client_finetune(data.frame(), finetune_steps = 1, output_model_id = "m", model_foo = 1)
  expect_equal(got$finetune$finetune_steps, 1)
  expect_equal(got$finetune$output_model_id, "m")
  expect_equal(got$finetune$model_foo, 1)
  nixtla_client_finetuned_models()
  expect_true(got$finetuned_models$as_df)
  nixtla_client_delete_finetuned_model("m")
  expect_equal(got$delete_finetuned_model$finetuned_model_id, "m")
})

test_that("finetune round trip", {
  skip_if_no_api()
  id <- nixtla_client_finetune(nixtlar::electricity, finetune_steps = 1,
                               output_model_id = paste0("nixtlar-test-", as.integer(Sys.time())))
  expect_type(id, "character")
  expect_true(id %in% nixtla_client_finetuned_models()$id)
  expect_equal(nixtla_client_finetuned_model(id)$id, id)
  expect_s3_class(nixtla_client_forecast(nixtlar::electricity, h = 2, finetuned_model_id = id), "data.frame")
  expect_true(nixtla_client_delete_finetuned_model(id))
})
