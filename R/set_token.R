#' Set token in global environment
#'
#' @param token The user's token. Get yours here: https://dashboard.nixtla.io/
#'
#' @return A message indicating the token has been set in the global environment.
#' @export
#'
set_token <- function(token) {
  assign("NIXTLAR_TOKEN", token, envir = nixtlaR_env)
  message("Token has been set for the current session.")
}
