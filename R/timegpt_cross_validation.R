#' Perform cross validation with TimeGPT.
#'
#' @param df A tsibble or a data frame with time series data.
#' @param h Forecast horizon.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param X_df A tsibble or a data frame with future exogenous variables.
#' @param level The confidence levels (0-100) for the prediction intervals.
#' @param n_windows Number of windows to evaluate.
#' @param step_size Step size between each cross validation window. If NULL, it will equal the forecast horizon (h).
#' @param finetune_steps Number of steps used to finetune TimeGPT in the new data.
#' @param clean_ex_first Clean exogenous signal before making the forecasts using TimeGPT.
#' @param model Model to use, either "timegpt-1" or "timegpt-1-long-horizon". Use "timegpt-1-long-horizon" if you want to forecast more than one seasonal period given the frequency of the data.
#'
#' @return A tsibble or a data frame with TimeGPT's cross validation.
#' @export
#'
timegpt_cross_validation <- function(df, h=8, freq=NULL, id_col=NULL, time_col="ds", target_col="y", X_df=NULL, level=NULL, n_windows=1, step_size=NULL, finetune_steps=0, clean_ex_first=TRUE, model="timegpt-1"){

  # Validation ----
  token <- get("NIXTLAR_TOKEN", envir = nixtlaR_env)

  if(!tsibble::is_tsibble(df) & !is.data.frame(df)){
    stop("Only tsibbles or data frames are allowed.")
  }

  # Prepare data ----
  if(is.null(id_col)){
    url_cv <- "Write here the url for the single series case"
  }else{
    url_cv <- "https://dashboard.nixtla.io/api/timegpt_multi_series_cross_validation"
  }

  data <- timegpt_data(df, freq, id_col, time_col, target_col)
  freq <- data$freq
  y <- data$y

  if(is.null(step_size)){
    step_size = h
  }

  timegpt_data <- list(
    model = model,
    fh = h,
    y = y,
    freq = freq,
    n_windows = n_windows,
    step_size = step_size,
    finetune_steps = finetune_steps,
    clean_ex_first = clean_ex_first
  )

  if(!is.null(X_df)){
    names(X_df)[which(names(X_df) == time_col)] <- "ds"
    if(!is.null(id_col)){
      names(X_df)[which(names(X_df) == id_col)] <- "unique_id"
    }

    exogenous <-  df |>
      dplyr::select(-y)

    exogenous <- rbind(exogenous, X_df)

    x <- list(
      columns = names(exogenous),
      data = lapply(1:nrow(exogenous), function(i) as.list(exogenous[i,]))
    )

    timegpt_data[['x']] <- x
  }

  if(!is.null(level)){
    level <- as.list(level)
    timegpt_data[["level"]] <- level
  }

  # Make request ----
  resp_cv <- httr2::request(url_cv) |>
    httr2::req_headers(
      "accept" = "application/json",
      "content-type" = "application/json",
      "authorization" = paste("Bearer", token)
    ) |>
    httr2::req_body_json(data = timegpt_data) |>
    httr2::req_perform()

  # Extract cross-validation ----
  cv <- httr2::resp_body_json(resp_cv)
  if(is.null(id_col)){
    # Write here the code for the single series case once the url is available
    res = 42
  }else{
    cv_list <- lapply(cv$data$forecast$data, unlist)
    res <- data.frame(do.call(rbind, cv_list))
    colnames(res) <- cv$data$forecast$columns
    res[,4:ncol(res)] <- lapply(res[,4:ncol(res)], as.numeric)
    res$cutoff <- lubridate::ymd_hms(res$cutoff)
  }

  # Data transformation ----
  if(tsibble::is_tsibble(df)){
    res$ds <- switch(freq,
                     "Y" = as.numeric(substr(res$ds, 1, 4)),
                     "A" = as.numeric(substr(res$ds, 1, 4)),
                     "Q" = tsibble::yearquarter(res$ds),
                     "MS" = tsibble::yearmonth(res$ds),
                     "W" = tsibble::yearweek(res$ds),
                     "H" = lubridate::ymd_hms(res$ds),
                     lubridate::ymd(res$ds) # default (daily or other)
                     )
    res$cutoff <- switch(freq,
                         "Y" = as.numeric(substr(res$cutoff, 1, 4)),
                         "A" = as.numeric(substr(res$cutoff, 1, 4)),
                         "Q" = tsibble::yearquarter(res$cutoff),
                         "MS" = tsibble::yearmonth(res$cutoff),
                         "W" = tsibble::yearweek(res$cutoff),
                         "H" = lubridate::ymd_hms(res$cutoff),
                         lubridate::ymd(res$cutoff) # default (daily or other)
                         )
    if(is.null(id_col)){
      res <- tsibble::as_tsibble(res, index="ds")
    }else{
      res <- tsibble::as_tsibble(res, key="unique_id", index="ds")
    }
  }else{
    # If df is a data frame, convert ds to dates
    if(freq == "H"){
      res$ds <- lubridate::ymd_hms(res$ds)
      res$cutoff <- lubridate::ymd_hms(res$cutoff)
    }else{
      res$ds <- lubridate::ymd(res$ds)
      res$cutoff <- lubridate::ymd(res$cutoff)
    }
  }

  # Rename columns ----
  colnames(res)[which(colnames(res) == "ds")] <- time_col
  if(!is.null(id_col)){
    colnames(res)[which(colnames(res) == "unique_id")] <- id_col
  }

  return(res)
}
