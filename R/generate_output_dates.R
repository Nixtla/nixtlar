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

    if (freq %in% c("QE", "Q")) {
      # End of quarter dates are: "YYYY-03-31", "YYYY-06-30", "YYYY-09-30", "YYYY-12-31".
      dt <- seq(from = start_date, by = "quarter", length.out = h+1)
      month <- lubridate::month(start_date)

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
      dt <- format(dt, "%Y-%m-%d")
    } else if (freq %in% c("ME", "M")) {
      dt <- seq(from = start_date + lubridate::days(1), by = r_freq, length.out = h+1) - lubridate::days(1)
      dt <- format(dt, "%Y-%m-%d")
    } else if(freq %in% c("B")){
      dt <- seq(from = start_date, by = "day", length.out = h+1+ceiling(h/5)*2+2) # ceiling(h/5)*2+2 ~ number of weeks*2 days (Saturday and Sunday) plus an extra weekend to be on the safe side
      dt <- dt[!weekdays(dt) %in% c("Saturday", "Sunday")]
      dt <- format(dt, "%Y-%m-%d")
    } else {
      dt <- seq(from = start_date, by = r_freq, length.out = h+1)
    }

    dt[2:(h+1)]
  })

  dates_df <- data.frame(lapply(new_dates, as.POSIXct))

  ids <- df_info$unique_id
  if (inherits(df_info$unique_id, "numeric") | inherits(df_info$unique_id, "integer")) {
    ids <- as.character(df_info$unique_id)
  }
  names(dates_df) <- ids

  return(dates_df)
}
