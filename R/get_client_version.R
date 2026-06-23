#' Get the 'nixtlar' client version string
#' This is a private function of 'nixtlar'
#'
#' @return A character string of the form `"R-x.y.z"`, where `x.y.z` is the
#' installed 'nixtlar' package version (read from the DESCRIPTION file).
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#'   .get_client_version()
#' }
#'
.get_client_version <- function(){
  paste0("R-", as.character(utils::packageVersion("nixtlar")))
}
