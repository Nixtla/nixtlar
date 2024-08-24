#' Transform and format dates in 'TimeGPT' output
##' This is a private function of 'nixtlar'
#'
#' @param fcst Dataframe with the 'TimeGPT' output, where column 'ds' contains date strings.
#' @param freq Frequency of the data.
#' @param flag Indicator where 1 denotes 'tsibble' and 0 denotes 'dataframe'.
#'
#' @return 'TimeGPT' output with the formatted dates
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   fcst <- .transform_output_dates(fcst, freq, flag)
#' }
#'
.transform_output_dates <- function(fcst, freq, flag){

  transform_ds <- function(date) {
    switch(freq,
           "Y" = as.numeric(substr(date, 1, 4)),
           "A" = as.numeric(substr(date, 1, 4)),
           "Q" = tsibble::yearquarter(date),
           "MS" = tsibble::yearmonth(date),
           "W" = tsibble::yearweek(date),
           "H" = lubridate::ymd_hms(date),
           lubridate::ymd(date)  # default (daily or other)
    )
  }

  if(flag == 1){
    new_ds <- future.apply::future_lapply(fcst$ds, transform_ds)
    fcst$ds <- do.call(c, new_ds)
    if(is.null(id_col)){
      fcst <- tsibble::as_tsibble(fcst, index="ds")
    }else{
      fcst <- tsibble::as_tsibble(fcst, key="unique_id", index="ds")
    }
  }else{
    # flag == 0
    if(freq == "H") {
      new_ds <- future.apply::future_lapply(fcst$ds, lubridate::ymd_hms)
    }else{
      new_ds <- future.apply::future_lapply(fcst$ds, lubridate::ymd)
    }
    fcst$ds <- do.call(c, new_ds)
  }

  return(fcst)
}
