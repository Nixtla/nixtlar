#' Generate output dates for forecast method.
#' This is a private function of 'nixtlar'
#'
#' @param df_info A data frame that is created by the forecast method with the last dates of every unique id.
#' @param freq The frequency of the data, as a period or offset alias.
#' @param h The forecast horizon.
#'
#' @return A data frame with dates for the forecast.
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   dates_df <- .generate_output_dates(df_info, freq, h)
#' }
#'
.generate_output_dates <- function(df_info, freq, h){

  new_dates <- vector("list", nrow(df_info))
  r_freq <- .r_frequency(freq)

  for(i in 1:nrow(df_info)){
    start_date <- df_info$dates[i]

    if(freq %in% c("QE", "Q")){
      dt <- seq(from = start_date, by = r_freq, length.out = h+1)
      month <- lubridate::month(start_date)
      dt <- seq(from = start_date, by = "quarter", length.out = h+1)

      # Calendar adjustments
      if (month %in% c(3, 12)) {
        for (j in 1:length(dt)) {
          mt <- lubridate::month(dt[j])
          if (mt %in% c(7, 10)) {
            dt[j] <- dt[j] - lubridate::days(1)
          }
        }
      } else {
        # month %in% c(6, 9)
        for (j in 1:length(dt)) {
          mt <- lubridate::month(dt[j])
          if (mt %in% c(3, 12)) {
            dt[j] <- dt[j] + lubridate::days(1)
          }
        }
      }

    }else if(freq %in% c("ME", "M")){
      start_date <- start_date+lubridate::days(1)
      dt <- seq(from = start_date, by = r_freq, length.out = h+1)-lubridate::days(1)
    }else{
      dt <- seq(df_info$dates[i], by = r_freq, length.out = h+1)
    }

    new_dates[[i]] <- dt[2:length(dt)]
  }

  dates_df <- data.frame(lapply(new_dates, as.POSIXct))

  ids <- df_info$unique_id
  if(inherits(df_info$unique_id, "numeric") | inherits(df_info$unique_id, "integer")){
    ids <- as.character(df_info$unique_id)
  }
  names(dates_df) <- ids

  return(dates_df)
}
