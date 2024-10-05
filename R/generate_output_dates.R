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
.generate_output_dates <- function(df_info, freq, h) {
  new_dates <- lapply(1:nrow(df_info), function(i) {
    start_date <- df_info$dates[i]
    r_freq <- .r_frequency(freq)

    if(freq == "QE") {
      dt <- seq(from = start_date, by = "quarter", length.out = h+1)
      month <- lubridate::month(start_date)
      if (month %in% c(3, 12)) {
        dt <- ifelse(lubridate::month(dt) %in% c(7, 10), dt - lubridate::days(1), dt)
      } else {
        dt <- ifelse(lubridate::month(dt) %in% c(3, 12), dt + lubridate::days(1), dt)
      }
    } else if(freq == "ME") {
      dt <- seq(from = start_date + lubridate::days(1), by = r_freq, length.out = h+1) - lubridate::days(1)
    } else {
      dt <- seq(from = start_date, by = r_freq, length.out = h+1)
    }

    dt[2:length(dt)]
  })

  names(new_dates) <- df_info$unique_id
  dates_df <- data.frame(lapply(new_dates, as.POSIXct))

  return(dates_df)
}
