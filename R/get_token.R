#' Get TIMEGPT_TOKEN from options or from .Renviron
#' This is a private function of nixtlar
#'
#' @return If available, the TIMEGTP_TOKEN. Otherwise it returns an error and a message asking the user to set the token.
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#'   .get_token()
#' }
#'
.get_token <- function(){

  # Get token from options
  token <- Sys.getenv("TIMEGPT_TOKEN", unset = NA)
  # If not available, get it from .Renviron
  if(is.na(token)) {
    token <- getOption("TIMEGPT_TOKEN", default = NA)
  }

  # Return token or, if not available, stop
  if(!is.na(token)){
    return(token)
  }else{
    stop("Please set TIMEGPT_TOKEN. Use nixtla_set_token() or set it as an environment variable in .Renviron")
  }
}
