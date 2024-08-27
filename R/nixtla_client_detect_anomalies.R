#' Detect anomalies with 'TimeGPT'
#'
#' @param df A tsibble or a data frame with time series data.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param level The confidence level (0-100) for the prediction interval used in anomaly detection. Default is 99.
#' @param clean_ex_first Clean exogenous signal before making the forecasts using 'TimeGPT'.
#' @param model Model to use, either "timegpt-1" or "timegpt-1-long-horizon". Use "timegpt-1-long-horizon" if you want to forecast more than one seasonal period given the frequency of the data.
#' @param num_partitions A positive integer, "auto", or NULL specifying the number of partitions. When set to "auto", the number of partitions is equal to the number of available cores. When NULL, it defaults to a single partition.
#'
#' @return A tsibble or a data frame with the anomalies detected in the historical period.
#' @export
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_set_api_key("YOUR_API_KEY")
#'   df <- nixtlar::electricity
#'   fcst <- nixtlar::nixtla_client_anomaly_detection(df, id_col="unique_id")
#' }
#'
nixtla_client_detect_anomalies <- function(df, freq=NULL, id_col=NULL, time_col="ds", target_col="y", level=c(99), clean_ex_first=TRUE, model="timegpt-1", num_partitions=NULL){

  if(is.null(num_partitions) || num_partitions == 1){
    res <- .nixtla_client_detect_anomalies_seq(
      df=df,
      freq=freq,
      id_col=id_col,
      time_col=time_col,
      target_col=target_col,
      level=level,
      clean_ex_first=clean_ex_first,
      model=model
    )
  }else{
    res <- .nixtla_client_detect_anomalies_distributed(
      df=df,
      freq=freq,
      id_col=id_col,
      time_col=time_col,
      target_col=target_col,
      level=level,
      clean_ex_first=clean_ex_first,
      model=model,
      num_partitions=num_partitions
    )
  }

  return(res)
}
