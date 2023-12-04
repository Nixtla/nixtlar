#' Set token in global environment
#'
#' @param token The user's token. Get yours here: https://dashboard.nixtla.io/
#'
#' @return A message indicating the token has been set in the global environment.
#' @export
#'
nixtla_set_token <- function(token) {
  options("TIMEGPT_TOKEN"=token)
  message("Token has been set for the current session.")
}
