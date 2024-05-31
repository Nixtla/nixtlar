#' Set 'API' key in global environment
#'
#' @param api_key The user's 'API' key. Get yours here: https://dashboard.nixtla.io/
#'
#' @return A message indicating the 'API' key has been set in the global environment.
#' @export
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_set_api_key("YOUR_API_KEY")
#' }
#'
nixtla_set_api_key <- function(api_key) {
  options("NIXTLA_API_KEY"=api_key)
  message("API key has been set for the current session.")
}
