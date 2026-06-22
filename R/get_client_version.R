#' Get the 'nixtlar' client version string
#' This is a private function of 'nixtlar'
#'
#' @return A string with the format "R-<version>", where <version> is the
#' installed 'nixtlar' package version (from the DESCRIPTION file).
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
