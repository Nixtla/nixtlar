#' `TimeGPT` fine-tune
#'
#' Fine-tune `TimeGPT` on your data and save the resulting model.
#'
#' @param df A data frame with time series data.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param finetune_steps Number of steps used to fine-tune 'TimeGPT'.
#' @param finetune_depth The depth of the fine-tuning. Uses a scale from 1 to 5.
#' @param finetune_loss Loss function to use for fine-tuning.
#' @param output_model_id ID to assign to the fine-tuned model.
#' @param finetuned_model_id ID of a previously fine-tuned model to continue from.
#' @param model Model to fine-tune, e.g. "timegpt-1".
#' @param ... Passed to the Python method.
#'
#' @return The ID of the fine-tuned model (character).
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_set_api_key("YOUR_API_KEY")
#'   id <- nixtlar::nixtla_client_finetune(nixtlar::electricity, finetune_steps = 10)
#' }
nixtla_client_finetune <- function(df, freq = NULL, id_col = "unique_id", time_col = "ds", target_col = "y", finetune_steps = NULL, finetune_depth = NULL, finetune_loss = NULL, output_model_id = NULL, finetuned_model_id = NULL, model = NULL, ...) {
  .call_client("finetune", df = df, freq = freq, id_col = id_col, time_col = time_col,
               target_col = target_col, finetune_steps = finetune_steps,
               finetune_depth = finetune_depth, finetune_loss = finetune_loss,
               output_model_id = output_model_id, finetuned_model_id = finetuned_model_id,
               model = model, ...)
}

#' List fine-tuned models
#'
#' @param ... Passed to the Python method.
#'
#' @return A data frame with the fine-tuned models.
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_client_finetuned_models()
#' }
nixtla_client_finetuned_models <- function(...) {
  .call_client("finetuned_models", as_df = TRUE, ...)
}

#' Get a fine-tuned model
#'
#' @param finetuned_model_id ID of the fine-tuned model.
#'
#' @return A list with the model's details.
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_client_finetuned_model("my-model")
#' }
nixtla_client_finetuned_model <- function(finetuned_model_id) {
  .to_r(.get_client()$finetuned_model(finetuned_model_id)$model_dump())
}

#' Delete a fine-tuned model
#'
#' @param finetuned_model_id ID of the fine-tuned model.
#'
#' @return `TRUE` if the model was deleted.
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_client_delete_finetuned_model("my-model")
#' }
nixtla_client_delete_finetuned_model <- function(finetuned_model_id) {
  .call_client("delete_finetuned_model", finetuned_model_id = finetuned_model_id)
}
