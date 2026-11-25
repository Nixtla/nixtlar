#' Validate 'API' key
#'
#' @return TRUE if the API key is valid, FALSE if Python rejects it, or NA on 'Azure' endpoints,
#'   where validation is not supported. Other errors, such as a missing
#'   API key or Python package, are raised.
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
             if (inherits(e, "python.builtin.BaseException")) return(FALSE)
             stop(e)
           })
}
