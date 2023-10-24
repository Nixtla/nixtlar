#' Find frequency of time series data
#'
#' @param df A tsibble or a data frame with time series data.
#' @param time_col Column that contains each time step.
#'
#' @return An alias (character) for the frequency.
#' @export
#'
find_frequency <- function(df, time_col){

  if(!tsibble::is_tsibble(df) & !is.data.frame(df)){
    stop("Only tsibbles or data frames are allowed.")
  }

  idx <- which(colnames(df) == time_col)
  colnames(df)[idx] <- "ds" # rename to use tsibble::guess_frequency()
  df$ds <- as.Date(df$ds)

  dates <- sort(unique(df$ds))
  dates_diff <- diff(dates)
  freq_table <- sort(table(dates_diff))
  mode <- freq_table[length(freq_table)]
  freq_num <- as.numeric(names(mode))

  freq_list = list(
    list(alias = "Y", value = c(365,366)),
    list(alias = "Q", value = c(91,92)),
    list(alias = "MS", value = c(30,31)),
    list(alias = "W", value = c(7)),
    list(alias = "D", value = c(1))
  )

  freq <- NA
  for(i in 1:length(freq_list)){
    if(freq_num %in% freq_list[i][[1]]$value){
      freq <- freq_list[i][[1]]$alias
    }
  }

  if(is.na(freq)){
    freq <- 1
    message("I'm not sure about the frequency of the data. Will default to 1. Please provide it if you know it.")
  }

  message(paste0("Frequency chosen: ", freq))

  return(freq)
}
