#' Get NIXTLA_API_KEY from options or from .Renviron
#' This is a private function of 'nixtlar'
#'
#' @return If available, the NIXTLA_API_KEY. Otherwise it returns an error and a message asking the user to set the 'API' key.
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#'   .get_api_key()
#' }
#'
.get_api_key <- function(){

  # Get 'API' key from options
  api_key <- Sys.getenv("NIXTLA_API_KEY", unset = NA)
  # If not available, get it from .Renviron
  if(is.na(api_key)) {
    api_key <- getOption("NIXTLA_API_KEY", default = NA)
  }

  # Return 'API' key or, if not available, stop
  if(!is.na(api_key)){
    return(api_key)
  }else{
    stop("Please set NIXTLA_API_KEY. Use nixtla_set_api_key() or set it as an environment variable in .Renviron")
  }
}
