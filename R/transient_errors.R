#' A function used by httr2::req_retry() to determine if the response represents a transient error
#' This is a private function of 'nixtlar'
#'
#' @param resp The response to a HTTP request
#'
#' @return TRUE if the response status is 500 or 502, FALSE otherwise.
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' .transient_errors(resp)
#' }
#'
.transient_errors <- function(resp){
  status <- httr2::resp_status(resp)
  status %in% c(500, 502) # codes considered transient
}
