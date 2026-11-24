#' `TimeGPT` API usage
#'
#' @return A list with the API usage of the account.
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_client_usage()
#' }
nixtla_client_usage <- function() .call_client("usage")

#' List available `TimeGPT` models
#'
#' @return A character vector with the model names.
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_client_list_models()
#' }
nixtla_client_list_models <- function() unlist(.call_client("list_models"))
