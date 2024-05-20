#' Electricity dataset
#'
#' Contains prices of different electricity markets.
#'
#' @format ## `electricity`
#' A data frame with 8400 rows and 3 columns:
#' \describe{
#'   \item{unique_id}{Unique identifiers of the electricity markets.}
#'   \item{ds}{Date in format YYYY:MM:DD hh:mm:ss.}
#'   \item{y}{Price for the given market and date.}
#' }
#' @source <https://raw.githubusercontent.com/Nixtla/transfer-learning-time-series/main/datasets/electricity-short.csv>
"electricity"
#'
#' Electricity dataset with exogenous variables
#'
#' Contains prices of different electricity markets with exogenous variables.
#'
#' @format ## `electricity_exo_vars`
#' A data frame with 8400 rows and 12 columns:
#' \describe{
#'   \item{unique_id}{Unique identifiers of the electricity markets.}
#'   \item{ds}{Date in format YYYY:MM:DD hh:mm:ss.}
#'   \item{y}{Price for the given market and date.}
#'   \item{Exogenous1}{An external factor influencing prices. For all markets, some form of day-ahead load forecast.}
#'   \item{Exogenous2}{An external factor influencing prices.
#'   For "BE" and "FR" markets, the day-ahead generation forecast.
#'   For "NP", the day-ahead wind generation forecast.
#'   For "PJM", the day-ahead load forecast in a specific zone.
#'   For "DE", the aggregated day-ahead wind and solar generation forecasts.
#'   }
#'   \item{day_0}{Binary variable indicating weekday.}
#'   \item{day_1}{Binary variable indicating weekday.}
#'   \item{day_2}{Binary variable indicating weekday.}
#'   \item{day_3}{Binary variable indicating weekday.}
#'   \item{day_4}{Binary variable indicating weekday.}
#'   \item{day_5}{Binary variable indicating weekday.}
#'   \item{day_6}{Binary variable indicating weekday.}
#' }
#' @source <https://raw.githubusercontent.com/Nixtla/transfer-learning-time-series/main/datasets/electricity-short.csv>
"electricity_exo_vars"
#'
#' Future values for the electricity dataset with exogenous variables
#'
#' Contains the future values of the exogenous variables of the electricity dataset (24 steps-ahead). To be used with `electricity_exo_vars`.
#'
#' @format ## `electricity_future_exo_vars`
#' A data frame with 120 rows and 11 columns:
#' \describe{
#'   \item{unique_id}{Unique identifiers of the electricity markets.}
#'   \item{ds}{Date in format YYYY:MM:DD hh:mm:ss.}
#'   \item{Exogenous1}{An external factor influencing prices. For all markets, some form of day-ahead load forecast.}
#'   \item{Exogenous2}{An external factor influencing prices.
#'   For "BE" and "FR" markets, the day-ahead generation forecast.
#'   For "NP", the day-ahead wind generation forecast.
#'   For "PJM", the day-ahead load forecast in a specific zone.
#'   For "DE", the aggregated day-ahead wind and solar generation forecasts.
#'   }
#'   \item{day_0}{Binary variable indicating weekday.}
#'   \item{day_1}{Binary variable indicating weekday.}
#'   \item{day_2}{Binary variable indicating weekday.}
#'   \item{day_3}{Binary variable indicating weekday.}
#'   \item{day_4}{Binary variable indicating weekday.}
#'   \item{day_5}{Binary variable indicating weekday.}
#'   \item{day_6}{Binary variable indicating weekday.}
#' }
#' @source <https://raw.githubusercontent.com/Nixtla/transfer-learning-time-series/main/datasets/electricity-short-future-ex-vars.csv>
"electricity_future_exo_vars"

