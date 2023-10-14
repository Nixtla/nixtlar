#' Generate TimeGPT forecast
#'
#' @param df A tsibble or a data frame with time series data.
#' @param h Forecast horizon.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param level Confidence levels (0-100) for the prediction intervals.
#' @param finetune_steps Number of steps used to finetune TimeGPT in the new data.
#' @param clean_ex_first Clean exogenous signal before making the forecasts using TimeGPT.
#' @param add_history Return fitted values of the model.
#'
#' @return TimeGPT forecasts for point predictions and probabilistic predictions (if level is not NULL).
#' @export
#'
timegpt_forecast <- function(df, h, freq=NULL, id_col=NULL, time_col="ds", target_col="y", level=NULL, finetune_steps=0, clean_ex_first=TRUE, add_history=FALSE){

  token <- Sys.getenv("NIXTLAR_TOKEN")

  if(!tsibble::is_tsibble(df) & !is.data.frame(df)){
    stop("Only tsibbles or data frames are allowed.")
  }

  # Check if single or multi-series and prepare data
  if(is.null(id_col)){
    url <- "https://dashboard.nixtla.io/api/timegpt"
    series_type <- "single"
    y <- prepare_single_series(df, time_col, target_col)
  }else{
    url <- "https://dashboard.nixtla.io/api/timegpt_multi_series"
    series_type <- "multi"
    y <- prepare_multi_series(df, id_col, time_col, target_col)
  }

  # Prepare request
  if(is.null(freq)){
    freq <- find_frequency(df)
  }

  timegpt_data <- list(
    fh = h,
    y = y,
    freq = freq,
    finetune_steps = finetune_steps,
    clean_ex_first = clean_ex_first
    )

  if(!is.null(level)){
    timegpt_data[["level"]] <- level
  }

  # Make request
  resp <- httr2::request(url) |>
    httr2::req_headers(
      "accept" = "application/json",
      "content-type" = "application/json",
      "authorization" = paste("Bearer", token)
      ) |>
    httr2::req_body_json(data = timegpt_data) |>
    httr2::req_perform()

  # Extract TimeGPT forecast
  fc <- httr2::resp_body_json(resp)

  if(series_type == "single"){
    fcst <- data.frame(
      ds = do.call(rbind, fc$data$timestamp),
      TimeGPT = do.call(rbind, fc$data$value)
      )
  }else{
    # series_type == "multi"
    fc_list <- lapply(fc$data$forecast$data, unlist)
    fcst <- data.frame(do.call(rbind, fc_list))
    fcst[,ncol(fcst)] <- as.numeric(fcst[,ncol(fcst)])
    colnames(fcst) <- fc$data$forecast$columns
  }

  if(series_type == "single"){ # for now, only single time series have fitted values
    if(add_history == TRUE){
      url_historic <-  "https://dashboard.nixtla.io/api/timegpt_historic"
      timegpt_historic <- list(
        y = y,
        freq = freq
      )

      if(!is.null(level)){
        timegpt_historic[["level"]] <- level
      }

      # Make request
      resp_hist <- httr2::request(url_historic) |>
        httr2::req_headers(
          "accept" = "application/json",
          "content-type" = "application/json",
          "authorization" = paste("Bearer", token)
        ) |>
        httr2::req_body_json(data = timegpt_historic) |>
        httr2::req_perform()

      # Extract fitted values
      hist <- httr2::resp_body_json(resp_hist)
      fitted <- data.frame(
        ds = do.call(rbind, hist$data$timestamp),
        TimeGPT = do.call(rbind, hist$data$value)
      )

      # Combine fitted values and TimeGPT forecast
      fcst <- rbind(fitted, fcst)
    }
  }

  return(fcst)
}
