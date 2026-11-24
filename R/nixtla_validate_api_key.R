#' Validate 'API' key
#'
#' @return TRUE if the API key is valid, FALSE otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_client_setup(api_key = "Your API key")
#'   nixtlar::nixtla_validate_api_key()
#' }
#'
nixtla_validate_api_key <- function(){
  tryCatch(isTRUE(.get_client()$validate_api_key(log = FALSE)),
           error = function(e) FALSE)
}
