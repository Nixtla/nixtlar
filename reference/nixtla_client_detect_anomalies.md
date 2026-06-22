# `TimeGPT` detect anomalies

`TimeGPT` detect anomalies

## Usage

``` r
nixtla_client_detect_anomalies(
  df,
  freq = NULL,
  id_col = "unique_id",
  time_col = "ds",
  target_col = "y",
  level = c(99),
  clean_ex_first = TRUE,
  model = "timegpt-1"
)
```

## Arguments

- df:

  A data frame with time series data.

- freq:

  Frequency of the data.

- id_col:

  Column that identifies each series.

- time_col:

  Column that identifies each timestep.

- target_col:

  Column that contains the target variable.

- level:

  The confidence level (0-100) for the prediction interval used in
  anomaly detection. Default is 99.

- clean_ex_first:

  Clean exogenous signal before making the forecasts using 'TimeGPT'.

- model:

  Model to use, either "timegpt-1" or "timegpt-1-long-horizon". Use
  "timegpt-1-long-horizon" if you want to forecast more than one
  seasonal period given the frequency of the data.

## Value

A data frame with the anomalies detected in the historical period.

## Examples

``` r
if (FALSE) { # \dontrun{
  nixtlar::nixtla_set_api_key("YOUR_API_KEY")
  df <- nixtlar::electricity
  fcst <- nixtlar::nixtla_client_anomaly_detection(df, id_col="unique_id")
} # }
```
