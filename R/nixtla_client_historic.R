#' `TimeGPT` historic forecast
#'
#' @param df A tsibble or a data frame with time series data.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param level The confidence levels (0-100) for the prediction intervals.
#' @param quantiles Quantiles to forecast. Should be between 0 and 1.
#' @param finetune_steps Number of steps used to fine-tune 'TimeGPT' in the new data.
#' @param finetune_depth The depth of the fine-tuning. Uses a scale from 1 to 5, where 1 means little fine-tuning and 5 means that the entire model is fine-tuned.
#' @param finetune_loss Loss function to use for fine-tuning. Options are: "default", "mae", "mse", "rmse", "mape", and "smape".
#' @param clean_ex_first Clean exogenous signal before making the forecasts using 'TimeGPT'.
#' @param model Model to use, e.g. "timegpt-1" or "timegpt-1-long-horizon". If NULL (default), the Python client's default model is used.
#' @param ... Additional arguments passed on to the Python client, e.g. `date_features`, `num_partitions`, `feature_contributions`.
#'
#' @return 'TimeGPT''s forecast for the in-sample period.
#' @export
#' @keywords private
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_set_api_key("YOUR_API_KEY")
#'   df <- nixtlar::electricity
#'   fcst <- nixtlar::nixtla_client_historic(df, id_col="unique_id", level=c(80,95))
#' }
#'
nixtla_client_historic <- function(df, freq = NULL, id_col = "unique_id", time_col = "ds", target_col = "y", level = NULL, quantiles = NULL, finetune_steps = NULL, finetune_depth = NULL, finetune_loss = NULL, clean_ex_first = NULL, model = NULL, ...) {
  # h = 1 + add_history returns fitted values plus one future step per series; drop that step.
  out <- .call_client("forecast", df = df, h = 1, freq = freq, id_col = id_col, time_col = time_col,
                      target_col = target_col, level = level, quantiles = quantiles,
                      finetune_steps = finetune_steps, finetune_depth = finetune_depth,
                      finetune_loss = finetune_loss, clean_ex_first = clean_ex_first,
                      add_history = TRUE, model = model, ...)
  if (is.null(id_col)) return(out[-nrow(out), , drop = FALSE])
  out |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::slice_head(n = -1) |>
    dplyr::ungroup() |>
    as.data.frame()
}
