#' `TimeGPT` detect anomalies
#'
#' Online anomaly detection: forecasts the last `detection_size` observations of each series using the preceding history, in windows of `h` steps, and flags the points that fall outside the prediction interval.
#'
#' @param df A data frame with time series data.
#' @param h Forecast horizon used for each detection window.
#' @param detection_size Number of trailing observations of each series to evaluate for anomalies.
#' @param threshold_method Method used to decide what counts as an anomaly, e.g. "univariate" or "multivariate". If NULL (default), the Python client's default is used.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param level The confidence level (0-100) for the prediction interval used in anomaly detection. Must be a single value.
#' @param clean_ex_first Clean exogenous signal before making the forecasts using 'TimeGPT'.
#' @param step_size Step size between detection windows. If NULL (default), it equals `h`.
#' @param finetune_steps Number of steps used to fine-tune 'TimeGPT' in the new data.
#' @param finetune_depth The depth of the fine-tuning. Uses a scale from 1 to 5.
#' @param finetune_loss Loss function to use for fine-tuning.
#' @param model Model to use, e.g. "timegpt-1" or "timegpt-1-long-horizon". If NULL (default), the Python client's default model is used.
#' @param ... Additional arguments passed on to the Python client, e.g. `num_partitions`.
#'
#' @return A data frame with the anomalies detected in the last `detection_size` observations of each series.
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_set_api_key("YOUR_API_KEY")
#'   df <- nixtlar::electricity
#'   anomalies <- nixtlar::nixtla_client_detect_anomalies(df, h=24, detection_size=48, level=99)
#' }
#'
nixtla_client_detect_anomalies <- function(df, h, detection_size, threshold_method = NULL, freq = NULL, id_col = "unique_id", time_col = "ds", target_col = "y", level = NULL, clean_ex_first = NULL, step_size = NULL, finetune_steps = NULL, finetune_depth = NULL, finetune_loss = NULL, model = NULL, ...) {
  force(h); force(detection_size)
  .call_client("detect_anomalies_online", df = df, h = h, detection_size = detection_size,
               threshold_method = threshold_method, freq = freq, id_col = id_col, time_col = time_col,
               target_col = target_col, level = level, clean_ex_first = clean_ex_first,
               step_size = step_size, finetune_steps = finetune_steps, finetune_depth = finetune_depth,
               finetune_loss = finetune_loss, model = model, ...)
}
