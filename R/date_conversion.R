#' Infer frequency of a tsibble and convert its index to date or string.
#'
#' @param df A tsibble.
#'
#' @return A list with the inferred frequency and data frame with dates in format yyyy-mm-dd.
#' @export
#'
#' @examples
#' df <- AirPassengers
#' tsbl <- tsibble::as_tsibble(df)
#' names(tsbl) <- c("ds", "y")
#' date_conversion(tsbl)
#'
date_conversion <- function(df){
  cls <- class(df$ds)[1]

  if(cls == "integer"){
    freq <- "Y"
    df$ds <- paste0(df$ds, "-01-01")

  }else if(cls %in% c("yearquarter", "yearmonth", "yearweek")){
    freq <- switch(cls,
                   yearquarter = "Q",
                   yearmonth = "MS",
                   yearweek = "W")
    df$ds <- as.Date(df$ds)
    df$ds <- as.character(df$ds)

  }else if(cls == "Date"){
    freq <- "D"

  }else if(cls %in% c("POSIXt", "POSIXct", "POSIXlt")){
    freq <- "H"

  }else{
    freq <- NULL

  }

  if(!is.null(freq)){
    message(paste0("Frequency chosen: ", freq))
  }

  res <- list(df = df, freq = freq)
  return(res)
}
