#' Infer frequency of a data frame.
#'
#' @param df A data frame with time series data.
#'
#' @return The inferred frequency.
#' @export
#'
#' @examples
#' df <- nixtlar::electricity
#' infer_frequency(df)
#'
infer_frequency <- function(df){
  freq <- NULL
  dates <- sort(unique(df$ds))

  # Check if it's hourly data
  nchrs <- lapply(as.character(dates), nchar)
  ntable <- sort(table(unlist(nchrs)))
  nmode <- ntable[length(ntable)]
  nmode <- as.numeric(names(nmode))

  if(nmode > 10){
    freq <- "H" # We'll assume hourly data
    message("Frequency chosen: H")
    return(freq)
  }

  # If it's not hourly data, check the time differences in days
  ddiff <- diff(as.Date(dates))
  table <- sort(table(ddiff))
  mode <- table[length(table)]
  mode <- as.numeric(names(mode))

  freq_list = list(
    list(alias = "Y", value = c(365,366)),
    list(alias = "Q", value = c(91,92)),
    list(alias = "MS", value = c(30,31)),
    list(alias = "W", value = c(7)),
    list(alias = "D", value = c(1))
  )

  for(i in 1:length(freq_list)){
    if(mode %in% freq_list[i][[1]]$value){
      freq <- freq_list[i][[1]]$alias
    }
  }

  if(is.null(freq)){
    freq <- "D"
    message("I'm not sure about the frequency of the data. Will default to daily (D). Please provide it if you know it.")
  }

  message(paste0("Frequency chosen: ", freq))

  return(freq)
}
