# Plot the output of the following nixtla_client functions: forecast, historic, anomaly_detection, and cross_validation.

Plot the output of the following nixtla_client functions: forecast,
historic, anomaly_detection, and cross_validation.

## Usage

``` r
nixtla_client_plot(
  df,
  fcst = NULL,
  h = NULL,
  id_col = "unique_id",
  time_col = "ds",
  target_col = "y",
  unique_ids = NULL,
  max_insample_length = NULL,
  plot_anomalies = FALSE
)
```

## Arguments

- df:

  A tsibble or a data frame with time series data (insample values).

- fcst:

  A tsibble or a data frame with the 'TimeGPT' point forecast and the
  prediction intervals (if available).

- h:

  Forecast horizon.

- id_col:

  Column that identifies each series.

- time_col:

  Column that identifies each timestep.

- target_col:

  Column that contains the target variable.

- unique_ids:

  Time series to plot. If NULL (default), selection will be random.

- max_insample_length:

  Max number of insample observations to be plotted.

- plot_anomalies:

  Whether or not to plot anomalies.

## Value

Plot with historical data and 'TimeGPT”s output (if available).

## Examples

``` r
if (FALSE) { # \dontrun{
  nixtlar::nixtla_set_api_key("YOUR_API_KEY")
  df <- nixtlar::electricity
  fcst <- nixtlar::nixtla_client_forecast(df, h=8, id_col="unique_id", level=c(80,95))
  nixtlar::timegpt_plot(df, fcst, h=8, id_col="unique_id")
} # }
```
