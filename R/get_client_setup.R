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
.get_client_steup <- function(){

  # Get setup from .Renviron
  base_url <- Sys.getenv("NIXTLA_BASE_URL", unset = NA)
  api_key <- Sys.getenv("NIXTLA_API_KEY", unset = NA)

  # If not available, get it from options
  if(is.na(base_url)){
    base_url <- getOption("NIXTLA_BASE_URL", default = NA)
  }
  if(is.na(base_url)){
    base_url <- "https://api.nixtla.io/"
  }

  if(is.na(api_key)) {
    api_key <- getOption("NIXTLA_API_KEY", default = NA)
  }
  if(is.na(api_key)){
    stop("Please set your NIXTLA_API_KEY. Use nixtla_client_setup() or set it as an environment variable in .Renviron")
  }

  setup <- list(
    base_url = base_url,
    api_key = api_key
  )

  return(setup)
}
