#' Plot the output of the following nixtla_client functions: forecast, historic, anomaly_detection, and cross_validation.
#'
#' @param df A tsibble or a data frame with time series data (insample values).
#' @param fcst A tsibble or a data frame with the 'TimeGPT' point forecast and the prediction intervals (if available).
#' @param h Forecast horizon.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param unique_ids Time series to plot. If NULL (default), selection will be random.
#' @param max_insample_length Max number of insample observations to be plotted.
#' @param plot_anomalies Whether or not to plot anomalies.
#'
#' @return Plot with historical data and 'TimeGPT''s output (if available).
#' @export
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_set_api_key("YOUR_API_KEY")
#'   df <- nixtlar::electricity
#'   fcst <- nixtlar::nixtla_client_forecast(df, h=8, id_col="unique_id", level=c(80,95))
#'   nixtlar::timegpt_plot(df, fcst, h=8, id_col="unique_id")
#' }
#'
nixtla_client_plot <- function(df, fcst=NULL, h=NULL, id_col="unique_id", time_col="ds", target_col="y", unique_ids = NULL, max_insample_length=NULL, plot_anomalies=FALSE){

  # Validate input ----
  if(!is.data.frame(df) & !inherits(df, "tbl_df") & !inherits(df, "tsibble")){
    stop("Only data frames, tibbles, and tsibbles are allowed.")
  }

  # Select facets ----
  nrow <- 4
  ncol <- 2

  # Rename columns ----
  names(df)[which(names(df) == time_col)] <- "ds"
  names(df)[which(names(df) == target_col)] <- "y"

  if(is.null(id_col)){
    # create unique_id for single series
    df <- df |>
      dplyr::mutate(unique_id = "ts_0") |>
      dplyr::select(c("unique_id", tidyselect::everything()))
    ids <- "ts_0"
    nrow <- 1
    ncol <- 1
  }else{
    names(df)[which(names(df) == id_col)] <- "unique_id"
    ids <- unique(df$unique_id)
    if(length(ids) == 2){ # reshape for better vizualization
      nrow <- 2
      ncol <- 1
    }
  }

  ## Select time series if there are more than 8 ----
  if(length(ids) > 8){
    if(!is.null(unique_ids)){
      ids <- unique_ids[1:min(length(unique_ids), 8)]
    }else{
      ids <- sample(unique(df$unique_id), size=8, replace=FALSE)
    }

    df <- df |>
      dplyr::filter(.data$unique_id %in% ids)

    if(!is.null(fcst)){
      fcst <- fcst |>
        dplyr::filter(.data$unique_id %in% ids)
    }
  }

  # Convert dates if necessary ----
  cls <- class(df$ds)[1]
  if(cls == "character"){
    nch <- nchar(df$ds[1])
    if(nch <= 10){
      df$ds <- lubridate::ymd(df$ds)
    }else{
      df$ds <- lubridate::ymd_hms(df$ds)
    }
  }

  if(is.null(fcst)){
    # Plot historical values only ----
    if(!is.null(max_insample_length)){
      df <- df |>
        dplyr::group_by(.data$unique_id) |>
        dplyr::slice_tail(n=max_insample_length) |>
        dplyr::ungroup()
    }

    plot <- ggplot2::ggplot(ggplot2::aes(x=.data$ds, y=.data$y), data = df)+
      ggplot2::geom_line(color="steelblue")+
      ggplot2::facet_wrap(~ .data$unique_id, scales = "free", ncol=ncol, nrow=nrow)+
      ggplot2::labs(x = "Date", y = "Target") +
      ggplot2::theme_minimal()

  }else{
    # Plot historical values and forecast ----
    color_vals <- c("#B5838D", "steelblue")

    # Rename forecast columns ----
    names(fcst)[which(names(fcst) == time_col)] <- "ds"
    if(!is.null(id_col)){
      names(fcst)[which(names(fcst) == id_col)] <- "unique_id"
    }else{
      fcst <- fcst |>
        dplyr::mutate(unique_id = "ts_0") |>
        dplyr::select(c("unique_id", tidyselect::everything()))
    }

    # the values of time_col should already be of class POSIXct

    # Check for cross validation output
    cross_validation <- FALSE
    if("cutoff" %in% names(fcst)){
      cross_validation <- TRUE
      if(plot_anomalies){
        message("Can't plot anomalies and cross validation output at the same time. Setting plot_anomalies=FALSE")
        plot_anomalies <- FALSE
      }
    }

    if(!is.null(max_insample_length)){
      df <- df |>
        dplyr::group_by(.data$unique_id) |>
        dplyr::slice_tail(n=max_insample_length) |>
        dplyr::ungroup()

      if(!cross_validation){
        start_dates <- df |>
          dplyr::group_by(.data$unique_id) |>
          dplyr::summarize(start_date = min(.data$ds)) |>
          dplyr::ungroup()

        fcst <- fcst |>
          dplyr::inner_join(start_dates, by = "unique_id") |>
          dplyr::filter(.data$ds >= .data$start_date) |>
          dplyr::select(-.data$start_date)
      }
    }

    df_long <- tidyr::pivot_longer(df[,c("unique_id", "ds", "y")], cols="y", names_to="variable", values_to="value")
    fcst_long <- tidyr::pivot_longer(fcst[,c("unique_id", "ds", "TimeGPT")], cols="TimeGPT", names_to="variable", values_to="value")
    data_long <- rbind(df_long, fcst_long)

    plot <- ggplot2::ggplot(data_long)+
      ggplot2::facet_wrap(~ .data$unique_id, scales = "free", ncol=ncol, nrow=nrow)+
      ggplot2::labs(x = "Date", y = "Target")+
      ggplot2::theme_minimal()

    # Add prediction intervals ----
    levels <- grepl("(lo|hi)", names(fcst))
    if(any(levels)){
      # Build data frame with  prediction intervals
      lower <- fcst[,c(which(names(fcst) %in% c("unique_id", "ds")), grep("lo", names(fcst)))]
      upper <- fcst[,c(which(names(fcst) %in% c("unique_id", "ds")), grep("hi", names(fcst)))]

      lower_long <- tidyr::pivot_longer(lower, cols=grep("lo", names(lower)), values_to="lower", names_to="variable") |>
        dplyr::mutate(level =  gsub("^[^-]*-[^-]*-", "", .data$variable))

      upper_long <- tidyr::pivot_longer(upper, cols=grep("hi", names(upper)), values_to="upper", names_to="variable") |>
        dplyr::mutate(level =  gsub("^[^-]*-[^-]*-", "", .data$variable))

      intervals <- merge(lower_long[,-which(names(lower_long) == "variable")], upper_long[,-which(names(upper_long) == "variable")], by=c("unique_id", "ds", "level"))

      plot <- plot+
        ggplot2::geom_ribbon(ggplot2::aes(x=.data$ds, ymin=.data$lower, ymax=.data$upper, group=.data$level, fill=.data$level), alpha=0.5, data=intervals)
    }

    # Add quantiles
    quantiles <- grepl("-q-", names(fcst))
    if(any(quantiles)){
      # Build data frame with quantiles
      quant <- fcst[,c(which(names(fcst) %in% c("unique_id", "ds")), grep("-q-", names(fcst)))]

      quant_long <- tidyr::pivot_longer(quant, cols=grep("-q-", names(quant)), values_to="value", names_to="variable") |>
        dplyr::mutate(quantiles =  gsub("^[^-]*-[^-]*-", "", .data$variable))

      num_quantiles <- length(unique(quant_long$quantiles))
      # Prepare colors for quantiles - max 10
      qcolors <- c("#755faa", "#3da564", "#dabb35", "#29e2ff","#b5d56d","#9ca5e2", "#d954a0", "#cb4545", "#e45d17","#18392b")

      if(num_quantiles > 10){
        message("Can't plot more than 10 quantiles at the same time. Selecting 10 at random.")
        qselect <- sample(unique(quant_long$quantiles), 10, replace = FALSE)
        quant_long <- quant_long |>
          dplyr::filter(quantiles %in% qselect)
        num_quantiles <- 10
      }

      color_vals <- c(qcolors[1:num_quantiles], color_vals)
      quant_long$quantiles <- paste0("q-", quant_long$quantiles)

      plot <- plot+
        ggplot2::geom_line(ggplot2::aes(x=.data$ds, y=.data$value, group=.data$quantiles, color=.data$quantiles), alpha=0.5, data=quant_long)
    }

    # Add anomalies ----
    if(plot_anomalies){
      if(!"anomaly" %in% names(fcst)){
        message("No anomalies found. Please include them when selecting plot_anomalies=TRUE")
      }else{
        plot <- plot+
          ggplot2::geom_point(ggplot2::aes(x=.data$ds, y=.data$TimeGPT, color="anomaly"), data=subset(fcst, fcst$anomaly==1))
        color_vals <- c("tomato", c("#B5838D", "steelblue"))
      }
    }

    plot <- plot+
      ggplot2::geom_line(ggplot2::aes(x=.data$ds, y=.data$value, group=.data$variable, color=.data$variable), data=data_long)+
      ggplot2::scale_color_manual(values = color_vals)+
      ggplot2::labs(color="variable")

    # Add start of forecast ----
    if(!is.null(h) & cross_validation == FALSE){
      if(!plot_anomalies || !("anomaly" %in% names(fcst))){
        start_fcst <- data_long |>
          dplyr::group_by(.data$unique_id) |>
          dplyr::mutate(start = dplyr::nth(.data$ds, dplyr::n()-h+1)) |>
          dplyr::ungroup()

        plot <- plot+
          ggplot2::geom_vline(ggplot2::aes(xintercept=.data$start), data=start_fcst, linetype="dotted", linewidth=0.5, color="tomato")
      }
    }

    # Add cutoff dates if available ----
    if(cross_validation){
      cutoff <- fcst[,c("unique_id", "ds", "cutoff")] |>
        dplyr::group_by(.data$unique_id, .data$cutoff) |>
        dplyr::slice(1) |>
        dplyr::ungroup()

      plot <- plot+
        ggplot2::geom_vline(ggplot2::aes(xintercept=.data$ds), data=cutoff, linewidth=0.5, linetype="dashed", color="#E68613")
    }
  }

  return(plot)
}
