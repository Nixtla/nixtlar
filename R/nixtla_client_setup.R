#' Set base 'ULR' and 'API' key in global environment
#'
#' @param base_url Custom base 'URL'. If NULL, defaults to "https://api.nixtla.io/".
#' @param api_key The user's 'API' key. Get yours here: https://dashboard.nixtla.io/
#'
#' @return A message indicating the configuration status.
#' @export
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_client_setup(
#'     base_url = "Base URL",
#'     api_key = "Your API key"
#'   )
#' }
#'
nixtla_client_setup <- function(base_url = NULL, api_key = NULL) {
  if (is.null(base_url)) {
    base_url <- "https://api.nixtla.io/"
  } else {
    message("Base URL has been set to: ", base_url)
  }
  options("NIXTLA_BASE_URL" = base_url)

  if (is.null(api_key)) {
    stop("API key must be provided. Get yours at https://dashboard.nixtla.io/")
  } else {
    options("NIXTLA_API_KEY" = api_key)
    message("API key has been set for the current session.")
  }
}
