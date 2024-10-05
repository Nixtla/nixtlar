#' Convert period or offset aliases to a character string recognized by R.
#' This is a private function of 'nixtlar'
#'
#' @param freq The period or offset alias used by 'TimeGPT'.
#'
#' @return A character string recognized by R for generating a regular sequence of times.
#' @export
#' @keywords internal
#'
#' @examples
#' .r_frequency("MS")   # Returns "month"
#' .r_frequency("10h")  # Returns "10 h"
#' .r_frequency("h")    # Returns "h" (unchanged)
.r_frequency <- function(freq){

  r_freq <- freq
  contains_units <- grepl("\\d(h|min|s)", freq)

  if(contains_units){
    r_freq <- gsub("(\\d)(h|min|s)", "\\1 \\2", freq)
  }else if(freq %in% c("YS", "YE", "Y")){
    r_freq <- "year"
  }else if(freq %in% c("QS", "QE", "Q")){
    r_freq <- "quarter"
  }else if(freq %in% c("MS", "ME", "M")){
    r_freq <- "month"
  }else if(grepl("^W", freq)){
    r_freq <- "week"
  }else if(freq == "D"){
    r_freq <- "day"
  }

  return(r_freq)
}


