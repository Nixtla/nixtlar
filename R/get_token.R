#' Get TIMEGPT_TOKEN from options or from .Renviron
#' This is a private function of nixtlar
#'
#' @return If available, the TIMEGTP_TOKEN
#' @export
#' @keywords internal
#'
.get_token <- function(){

  # Get token from options
  token <- getOption("TIMEGPT_TOKEN")

  # If not available, get it from .Renviron
  if(is.null(token)){
    token <- Sys.getenv("TIMEGPT_TOKEN")
  }

  # Return token or, if not available, stop
  if(nzchar(token)){
    return(token)
  }else{
    stop("Please set TIMEGPT_TOKEN. Use nixtla_set_token() or set it as an environment variable in .Renviron")
  }
}
