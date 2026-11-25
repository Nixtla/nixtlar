#' Validate 'API' key
#'
#' @return TRUE if the API key is valid, FALSE otherwise, or NA on 'Azure' endpoints,
#'   where validation is not supported.
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
           error = function(e) {
             if (inherits(e, "python.builtin.NotImplementedError")) {
               message("API key validation is not supported on Azure endpoints.")
               return(NA)
             }
             FALSE
           })
}
