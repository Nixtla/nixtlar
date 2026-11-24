#' `TimeGPT` forecast
#'
#' @param df A data frame with time series data.
#' @param h Forecast horizon.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param X_df A tsibble or a data frame with future exogenous variables.
#' @param level The confidence levels (0-100) for the prediction intervals.
#' @param quantiles Quantiles to forecast. Should be between 0 and 1.
#' @param finetune_steps Number of steps used to fine-tune 'TimeGPT' in the new data.
#' @param finetune_depth The depth of the fine-tuning. Uses a scale from 1 to 5, where 1 means little fine-tuning and 5 means that the entire model is fine-tuned.
#' @param finetune_loss Loss function to use for fine-tuning. Options are: "default", "mae", "mse", "rmse", "mape", and "smape".
#' @param clean_ex_first Clean exogenous signal before making the forecasts using 'TimeGPT'.
#' @param hist_exog_list A vector containing the column names of the historical exogenous features.
#' @param add_history Return fitted values of the model.
#' @param model Model to use, e.g. "timegpt-1" or "timegpt-1-long-horizon". If NULL (default), the Python client's default model is used.
#' @param ... Additional arguments passed on to the Python client, e.g. `date_features`, `num_partitions`, `feature_contributions`.
#'
#' @return 'TimeGPT''s forecast.
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_set_api_key("YOUR_API_KEY")
#'   df <- nixtlar::electricity
#'   fcst <- nixtlar::nixtla_client_forecast(df, h=8, id_col="unique_id", level=c(80,95))
#' }
#'
nixtla_client_forecast <- function(df, h = 8, freq = NULL, id_col = "unique_id", time_col = "ds", target_col = "y", X_df = NULL, level = NULL, quantiles = NULL, finetune_steps = NULL, finetune_depth = NULL, finetune_loss = NULL, clean_ex_first = NULL, hist_exog_list = NULL, add_history = NULL, model = NULL, ...) {
  .call_client("forecast", df = df, h = h, freq = freq, id_col = id_col, time_col = time_col,
               target_col = target_col, X_df = X_df, level = level, quantiles = quantiles,
               finetune_steps = finetune_steps, finetune_depth = finetune_depth,
               finetune_loss = finetune_loss, clean_ex_first = clean_ex_first,
               hist_exog_list = hist_exog_list, add_history = add_history, model = model, ...)
}
