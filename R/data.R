#' Electricity short dataset
#'
#' Contains prices of different electricity markets
#'
#' @format ## `electricity`
#' A data frame with 3,600 rows and 3 columns:
#' \describe{
#'   \item{unique_id}{Unique identifiers of the electricity markets}
#'   \item{ds}{Date in format YYYY:MM:DD hh:mm:ss}
#'   \item{y}{Price for the given market and date}
#' }
#' @source <https://raw.githubusercontent.com/Nixtla/transfer-learning-time-series/main/datasets/electricity-short.csv>
"electricity"
