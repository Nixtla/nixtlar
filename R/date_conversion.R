#' Transforms time column from input data. If applicable, infers frequency and extracts timezone.
#' This is a private function of 'nixtlar'.
#'
#' @param df Data frame, tibble, or tsibble with input data.
#' @param freq The frequency of the data. If NULL, the function will attempt to infer it.
#' @param cls The class of the time column. Expected values include "numeric", "yearquarter", "yearmonth", "yearweek", "Date", "POSIXt", "POSIXct", and "POSIXlt".
#'
#' @return A list containing three elements: df, freq, and tzone.
#' - df: The input data with the transformed time column.
#' - freq: The frequency of the data, either provided or inferred.
#' - tzone: The timezone of the data.
#'
#' @export
#' @keywords internal
#'
#' @examples
#' df <- AirPassengers
#' tsbl <- tsibble::as_tsibble(df)
#' names(tsbl) <- c("ds", "y")
#' date_conversion(tsbl, NULL, class(tsbl)[1])
#'
.date_conversion <- function(df, freq, cls){

  fq <- NULL
  tzone <- NULL

  if(cls == "numeric"){
    # assumes year in format YYYY (e.g. 2024)
    fq <- "Y"
    df$ds <- paste0(df$ds, "-01-01")

  }else if(cls %in% c("yearquarter", "yearmonth", "yearweek")){
    fq <- switch(cls,
                 yearquarter = "Q",
                 yearmonth = "MS",
                 yearweek = "W")
    df$ds <- as.Date(df$ds)
    df$ds <- as.character(df$ds)

  }else if(cls %in% c("POSIXt", "POSIXct", "POSIXlt")){
    tzone <- attr(df$ds, "tzone")
  }

  if(is.null(freq) & !is.null(fq)){
    freq <- fq
    message(paste0("Frequency chosen: ", freq))
  }

  res <- list(df = df, freq = freq, tzone = tzone)

  return(res)
}
