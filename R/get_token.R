#' Get TIMEGPT_TOKEN from options or from .Renviron
#' This is a private function of the package
#'
#' @return If available, the TIMEGTP_TOKEN
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
    stop("Please set TIMEGPT_TOKEN. Use set_token() or set it as an environment variable in .Renviron")
  }
}
