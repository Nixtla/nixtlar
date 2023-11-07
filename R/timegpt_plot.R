#' Plot forecast and insample values.
#'
#' @param df A tsibble or a data frame with time series data (insample values).
#' @param fcst A tsibble or a data frame with the TimeGPT point forecast and the prediction intervals (if available).
#' @param h Forecast horizon.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param max_insample_length Max number of insample observations to be plotted.
#' @param plot_anomalies Whether or not to plot anomalies.
#'
#' @return Plot with the forecast and insample values
#' @export
#'
timegpt_plot <- function(df, fcst=NULL, h=NULL, id_col=NULL, time_col="ds", target_col="y", max_insample_length=NULL, plot_anomalies=FALSE){

  # Rename columns
  names(df)[which(names(df) == time_col)] <- "ds"
  names(df)[which(names(df) == target_col)] <- "y"
  if(!is.null(id_col)){
    names(df)[which(names(df) == id_col)] <- "unique_id"

    # Select time series if there are more than 8
    if(length(unique(df$unique_id)) > 8){
      ids <- sample(unique(df$unique_id), size=8, replace=FALSE)
      df <- df |>
        dplyr::filter(.data$unique_id %in% ids)

      fcst <- fcst |>
        dplyr::filter(.data$unique_id %in% ids)
    }
  }

  if(is.null(fcst)){
    # Plot historical values
    if(!is.null(max_insample_length)){
      df <- df |>
        dplyr::group_by(.data$unique_id) |>
        dplyr::slice_tail(n=max_insample_length) |>
        dplyr::ungroup()
    }

    plot <- ggplot2::ggplot(ggplot2::aes(x=.data$ds, y=.data$y), data = df)+
      ggplot2::geom_line(color="steelblue")+
      ggplot2::facet_wrap(~ .data$unique_id, scales = "free", ncol=2, nrow=4) +
      ggplot2::labs(title = "Plots for unique_id", x = "Date", y = "Target") +
      ggplot2::theme_minimal()

  }else{
    # Plot historical values and forecast
    color_vals <- c("#B5838D", "steelblue")

    if(!is.null(max_insample_length)){
      df <- df |>
        dplyr::group_by(.data$unique_id) |>
        dplyr::slice_tail(n=max_insample_length) |>
        dplyr::ungroup()

      start_dates <- df |>
        dplyr::group_by(.data$unique_id) |>
        dplyr::summarize(start_date = min(.data$ds)) |>
        dplyr::ungroup()

      fcst <- fcst |>
        dplyr::inner_join(start_dates, by = "unique_id") |>
        dplyr::filter(.data$ds >= .data$start_date) |>
        dplyr::select(-.data$start_date)
    }

    df_long <- tidyr::pivot_longer(df[,c("unique_id", "ds", "y")], cols="y", names_to="variable", values_to="value")
    fcst_long <- tidyr::pivot_longer(fcst[,c("unique_id", "ds", "TimeGPT")], cols="TimeGPT", names_to="variable", values_to="value")
    data_long <- rbind(df_long, fcst_long)

    plot <- ggplot2::ggplot(data_long)+
      ggplot2::facet_wrap(~ .data$unique_id, scales = "free", ncol=2, nrow=4)+
      ggplot2::labs(title = "Plots for unique_id", x = "Date", y = "Target")+
      ggplot2::theme_minimal()

    # Check if there are prediction intervals
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

    # Check if there are anomalies
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
      ggplot2::scale_color_manual(values = color_vals)

    # Add vertical dotted line to signal start of forecast
    if(!is.null(h)){
      if(!plot_anomalies || !("anomaly" %in% names(fcst))){
        start_fcst <- data_long |>
          dplyr::group_by(.data$unique_id) |>
          dplyr::mutate(start = dplyr::nth(.data$ds, dplyr::n()-h+1)) |>
          dplyr::ungroup()

        plot <- plot+
          ggplot2::geom_vline(ggplot2::aes(xintercept=.data$start), data=start_fcst, linetype="dotted", color="tomato")
      }
    }

  }
  return(plot)
}
