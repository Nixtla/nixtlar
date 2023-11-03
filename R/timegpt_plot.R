#' Plot forecast and insample values.
#'
#' @param df A tsibble or a data frame with time series data (insample values).
#' @param fcst A tsibble or a data frame with the TimeGPT point forecast and the prediction intervals (if available)
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param max_insample_length Max number of insample observations to be plotted.
#'
#' @return Plot with the forecast and insample values
#' @export
#'
timegpt_plot <- function(df, fcst=NULL, id_col=NULL, time_col="ds", target_col="y", max_insample_length=NULL){

  # Rename columns
  names(df)[which(names(df) == time_col)] <- "ds"
  names(df)[which(names(df) == target_col)] <- "y"
  if(!is.null(id_col)){
    names(df)[which(names(df) == id_col)] <- "unique_id"
  }

  if(!is.null(max_insample_length)){
    df <- df |>
      dplyr::group_by(unique_id) |>
      dplyr::slice_tail(n=max_insample_length) |>
      dplyr::ungroup()
  }

  if(is.null(fcst)){
    # Plot historical values
    plot <- ggplot2::ggplot(ggplot2::aes(x=ds, y=y), data = df)+
      ggplot2::geom_line(color="steelblue")+
      ggplot2::facet_wrap(~ unique_id, scales = "free", ncol=2, nrow=4) +
      ggplot2::labs(title = "Plots for unique_id", x = "Date", y = "Target") +
      ggplot2::theme_minimal()

  }else{
    # Plot historical values and forecast
    df_long <- tidyr::pivot_longer(df[,c("unique_id", "ds", "y")], cols=y, names_to="variable", values_to="value")
    fcst_long <- tidyr::pivot_longer(fcst[,c("unique_id", "ds", "TimeGPT")], cols=TimeGPT, names_to="variable", values_to="value")
    data_long <- rbind(df_long, fcst_long)

    plot <- ggplot2::ggplot(data_long)+
      ggplot2::facet_wrap(~ unique_id, scales = "free", ncol=2, nrow=4)+
      ggplot2::labs(title = "Plots for unique_id", x = "Date", y = "Target")+
      ggplot2::theme_minimal()

    # Check if there are prediction intervals
    levels <- grepl("(lo|hi)", names(fcst))
    if(any(levels)){

      # Build data frame with  prediction intervals
      lower <- fcst[,c(which(names(fcst) %in% c("unique_id", "ds")), grep("lo", names(fcst)))]
      upper <- fcst[,c(which(names(fcst) %in% c("unique_id", "ds")), grep("hi", names(fcst)))]

      lower_long <- tidyr::pivot_longer(lower, cols=grep("lo", names(lower)), values_to="lower", names_to="variable") |>
        dplyr::mutate(level =  gsub("^[^-]*-[^-]*-", "", variable))

      upper_long <- tidyr::pivot_longer(upper, cols=grep("hi", names(upper)), values_to="upper", names_to="variable") |>
        dplyr::mutate(level =  gsub("^[^-]*-[^-]*-", "", variable))

      intervals <- merge(lower_long[,-which(names(lower_long) == "variable")], upper_long[,-which(names(upper_long) == "variable")], by=c("unique_id", "ds", "level"))

      #intervals$level <- factor(intervals$level, levels=rev(unique(intervals$level))) # use this to change color order

      plot <- plot+
        ggplot2::geom_ribbon(ggplot2::aes(x=ds, ymin=lower, ymax=upper, group=level, fill=level), alpha=0.5, data=intervals)
    }

    plot <- plot+
      ggplot2::geom_line(ggplot2::aes(x=ds, y=value, group=variable, color=variable))+
      ggplot2::scale_color_manual(values = c("#B5838D", "steelblue"))
  }
  return(plot)
}
