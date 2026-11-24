#' Set base 'ULR' and 'API' key in global environment
#'
#' @param base_url Custom base 'URL'. If NULL, the Python client uses its default.
#' @param api_key The user's 'API' key. Get yours here: https://www.nixtla.io/dashboard
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
  if (!is.null(base_url)) {
    options("NIXTLA_BASE_URL" = base_url)
    message("Base URL has been set to: ", base_url)
  }

  if (is.null(api_key)) {
    stop("API key must be provided. Get yours at https://www.nixtla.io/dashboard")
  } else {
    options("NIXTLA_API_KEY" = api_key)
    message("API key has been set for the current session.")
  }
}
