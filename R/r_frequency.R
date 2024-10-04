#' Convert 'TimeGPT' frequency alias to a character string recognized by R.
#' This is a private function of 'nixtlar'
#'
#' @param freq A character string specifying the frequency alias used by 'TimeGPT'.
#'
#' @return A character string recognized by R for generating a regular sequence of times.
#' @export
#' @keywords internal
#'
#' @examples
#' r_frequency("MS")   # Returns "month"
#' r_frequency("10h")  # Returns "10 h"
#' r_frequency("h")    # Returns "h" (unchanged)
.r_frequency <- function(freq){
  r_freq <- NULL
  contains_units <- grepl("\\d(h|min|s)", freq)

  if(contains_units){
    r_freq <- gsub("(\\d)(h|min|s)", "\\1 \\2", freq)
  }else{
    r_freq <- switch(
      freq,
      "Y" = "year",
      "Q" = "quarter",
      "MS" = "month",
      "W" = "week",
      "D" = "day",
      1
    )
  }

  return(r_freq)
}



