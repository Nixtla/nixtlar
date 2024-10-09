#' Infer frequency of a data frame.
#'
#' @param df A data frame with time series data.
#' @param freq The frequency of the data as specified by the user; NULL otherwise.
#'
#' @return The inferred frequency.
#' @export
#'
#' @examples
#' df <- nixtlar::electricity
#' freq <- NULL
#' infer_frequency(df, freq)
#'
infer_frequency <- function(df, freq){
  if(!is.null(freq)){
    return(freq)
  }

  if(length(unique(df$ds)) > 1){ # this is done to avoid the vanishing dates issue
    dt <- sample(df$ds, 2)
  }else{
    dt <- df$ds[1]
  }

  # Vanishing dates issue: Dates that correspond to midnight only show YYYY-MM-DD, excluding 00:00:00

  num_chars <- max(nchar(as.character(dt)))

  if(num_chars <= 10){
    # assumes dates in format YYYY-MM-DD
    if(inherits(df$ds, "character")){
      dates <- lubridate::ymd(sort(unique(df$ds)))
    }else{
      dates <- sort(unique(df$ds))
    }
    dates_diff <- diff(dates)
    dates_table <- table(dates_diff)
    mode <- as.numeric(names(which.max(dates_table)))

    freq_list = list(
      list(alias = "Y", value = c(365,366)),
      list(alias = "Q", value = c(91,92)),
      list(alias = "M", value = c(30,31)),
      list(alias = "W", value = c(7)),
      list(alias = "D", value = c(24,1))
    )

    for(item in freq_list){
      if(mode %in% item$value){
        freq <- item$alias
        break
      }
    }

    message(paste0("Frequency chosen: ", freq))
    return(freq)

  }else{
    # assumes dates in format YYYY-MM-DD hh:mm:ss
    if(inherits(df$ds, "character")){
      dates <- lubridate::ymd_hms(sort(unique(df$ds)))
    }else{
      dates <- sort(unique(df$ds))
    }
    dates_diff <- diff(dates)
    dates_table <- table(dates_diff)
    mode <- as.numeric(names(which.max(dates_table)))

    units <- attr(dates_diff, "units")

    freq <- switch(
      units,
      "hours" = ifelse(mode == 1, "h", paste0(mode, "h")),
      "mins" = ifelse(mode == 1, "min", paste0(mode, "min")),
      "secs" = ifelse(mode == 1, "s", paste0(mode, "s"))
    )

    message(paste0("Frequency chosen: ", freq))
    return(freq)
  }

  if(is.null(freq)){
    stop("I can't figure out the frequency of the data. Please specify it with the `freq` parameter")
  }
}

