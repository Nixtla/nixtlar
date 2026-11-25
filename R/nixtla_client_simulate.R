#' `TimeGPT` simulate
#'
#' Simulate future sample paths of each series.
#'
#' @param df A data frame with time series data.
#' @param h Forecast horizon.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param X_df A data frame with future exogenous variables.
#' @param n_paths Number of sample paths to simulate.
#' @param quantiles Strictly increasing marginal quantiles inside (0, 1), between 2 and 200 values. They refine the marginal distribution the paths are drawn from and add no columns to the result.
#' @param seed Random seed.
#' @param model Model used to generate the marginal forecasts. If NULL (default), the Python client's default model is used.
#' @param ... Passed to the Python method.
#'
#' @return A data frame with the simulated paths.
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_set_api_key("YOUR_API_KEY")
#'   sims <- nixtlar::nixtla_client_simulate(nixtlar::electricity, h = 8, n_paths = 5)
#' }
nixtla_client_simulate <- function(df, h, freq = NULL, id_col = "unique_id", time_col = "ds", target_col = "y", X_df = NULL, n_paths = NULL, quantiles = NULL, seed = NULL, model = NULL, ...) {
  force(h)
  .call_client("simulate", df = df, h = h, freq = freq, id_col = id_col, time_col = time_col,
               target_col = target_col, X_df = X_df, n_paths = n_paths, quantiles = quantiles,
               seed = seed, model = model, ...)
}
