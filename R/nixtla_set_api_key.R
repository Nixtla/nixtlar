#' Set 'API' key in global environment
#'
#' This function will be deprecated in future versions. Please use `nixtla_client_setup` instead.
#'
#' @param api_key The user's 'API' key. Get yours here: https://dashboard.nixtla.io/
#'
#' @return A message indicating the 'API' key has been set in the global environment.
#' @export
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_set_api_key("Your API key")
#' }
#'
nixtla_set_api_key <- function(api_key) {
  nixtla_client_setup(base_url = NULL, api_key = api_key)
}
