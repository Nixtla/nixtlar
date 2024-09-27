#' Validate future exogenous variables (if applicable)
#' This is a private function of 'nixtlar'
#'
#' @param df A tsibble or a data frame with time series data.
#' @param h Forecast horizon.
#' @param X_df A tsibble or a data frame with future exogenous variables.
#'
#' @return Returns a vector with the exogenous variable names if the validation is successful. If the validation fails, it stops execution and returns an error message, indicating what went wrong.
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' df <- nixtlar::electricity_exo_vars
#' X_df <- nixtlar::electricity_future_exo_vars
#' .validate_exogenous(df, h=24, X_df)
#' }
#'
.validate_exogenous <- function(df, h, X_df){

  # Check if df and X_df contain the same exogenous variables
  vals_df <- setdiff(names(df), c("unique_id", "ds", "y"))
  vals_X_df <- setdiff(names(X_df), c("unique_id", "ds"))

  if(!setequal(vals_df, vals_X_df)){
    stop("df and X_df must contain the same exogenous variables.")
  }

  # Check if the future values of the exogenous variables cover the forecast horizon
  future_vals <- X_df |>
    dplyr::group_by(.data$unique_id) |>
    dplyr::filter(dplyr::n() == h)

  if(length(unique(future_vals$unique_id)) != length(unique(X_df$unique_id))){
    stop("The future values of the exogenous variables must cover the forecast horizon.")
  }

  return(vals_df)
}
