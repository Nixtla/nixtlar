#' Set token in global environment
#'
#' @param token The user's token. Get yours here: https://dashboard.nixtla.io/
#'
#' @return A message indicating the token has been set in the global environment.
#' @export
#'
set_token <- function(token) {
  token_line <- paste0("NIXTLAR_TOKEN=", token)
  renviron_path <- file.path(normalizePath("~"), ".Renviron")
  if (!file.exists(renviron_path)) {
    file.create(renviron_path)
  }
  current_content <- readLines(renviron_path, warn = FALSE)
  if (!any(grepl("NIXTLAR_TOKEN", current_content))) {
    write(token_line, renviron_path, append = TRUE)
    message("Token has been set.")
  } else {
    message("Token is already set up.")
  }
}
