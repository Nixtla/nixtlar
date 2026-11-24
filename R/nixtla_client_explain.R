#' `TimeGPT` explain
#'
#' Explain the contribution of exogenous variables and the series itself to the forecast.
#'
#' @param df A data frame with time series data (including exogenous variables).
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param ... Passed to the Python method, e.g. `h` and `X_df`.
#'
#' @return A data frame with the explanations.
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_set_api_key("YOUR_API_KEY")
#'   expl <- nixtlar::nixtla_client_explain(nixtlar::electricity_exo_vars)
#' }
nixtla_client_explain <- function(df, freq = NULL, id_col = "unique_id", time_col = "ds", target_col = "y", ...) {
  .call_client("explain", df = df, freq = freq, id_col = id_col, time_col = time_col,
               target_col = target_col, ...)
}
