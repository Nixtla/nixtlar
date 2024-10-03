#' Generate 'TimeGPT' forecast for the in-sample period (historical period).
#'
#' @param df A tsibble or a data frame with time series data.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param level The confidence levels (0-100) for the prediction intervals.
#' @param quantiles Quantiles to forecast. Should be between 0 and 1.
#' @param finetune_steps Number of steps used to finetune 'TimeGPT' in the new data.
#' @param finetune_loss Loss function to use for finetuning. Options are: "default", "mae", "mse", "rmse", "mape", and "smape".
#' @param clean_ex_first Clean exogenous signal before making the forecasts using 'TimeGPT'.
#' @param model Model to use, either "timegpt-1" or "timegpt-1-long-horizon". Use "timegpt-1-long-horizon" if you want to forecast more than one seasonal period given the frequency of the data.
#' @param num_partitions A positive integer, "auto", or NULL specifying the number of partitions. When set to "auto", the number of partitions is equal to the number of available cores. When NULL, it defaults to a single partition.
#'
#' @return 'TimeGPT''s forecast for the in-sample period.
#' @export
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_set_api_key("YOUR_API_KEY")
#'   df <- nixtlar::electricity
#'   fcst <- nixtlar::nixtla_client_historic(df, id_col="unique_id", level=c(80,95))
#' }
#'
nixtla_client_historic <- function(df, freq=NULL, id_col="unique_id", time_col="ds", target_col="y", level=NULL, quantiles=NULL, finetune_steps=0, finetune_loss="default", clean_ex_first=TRUE, model="timegpt-1", num_partitions=NULL){

  if(is.null(num_partitions) || num_partitions == 1){
    fitted <- .nixtla_client_historic_seq(
      df=df,
      freq=freq,
      id_col=id_col,
      time_col=time_col,
      target_col=target_col,
      level=level,
      quantiles=quantiles,
      finetune_steps=finetune_steps,
      finetune_loss=finetune_loss,
      clean_ex_first=clean_ex_first
    )
  }else{
    fitted <- .nixtla_client_historic_distributed(
      df=df,
      freq=freq,
      id_col=id_col,
      time_col=time_col,
      target_col=target_col,
      level=level,
      quantiles=quantiles,
      finetune_steps=finetune_steps,
      finetune_loss=finetune_loss,
      clean_ex_first=clean_ex_first,
      num_partitions=num_partitions
    )
  }

  return(fitted)
}
