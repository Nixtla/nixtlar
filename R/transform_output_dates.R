#' Transform and format dates in 'TimeGPT' output
##' This is a private function of 'nixtlar'
#'
#' @param df Dataframe with the 'TimeGPT' output, where column 'col' contains date strings.
#' @param col Name of the column with the dates to transform.
#' @param freq Frequency of the data.
#' @param flag Indicator where 1 denotes 'tsibble' and 0 denotes 'dataframe'.
#'
#' @return 'TimeGPT' output with the formatted dates
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   fcst <- .transform_output_dates(fcst, col, freq, flag)
#' }
#'
.transform_output_dates <- function(df, col, freq, flag){

  index_col <- which(names(df) == col)

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
    new_ds <- future.apply::future_lapply(df[,index_col], transform_ds)
    df[,index_col] <- do.call(c, new_ds)
  }else{
    if(freq == "H") {
      new_ds <- future.apply::future_lapply(df[,index_col], lubridate::ymd_hms)
    }else{
      new_ds <- future.apply::future_lapply(df[,index_col], lubridate::ymd)
    }
    df[,index_col] <- do.call(c, new_ds)
  }

  if(flag == 1 && !(tsibble::is_tsibble(df))){
    if(is.null(id_col)){
      df <- tsibble::as_tsibble(df, index="ds")
    }else{
      df <- tsibble::as_tsibble(df, key="unique_id", index="ds")
    }
  }

  return(df)
}
