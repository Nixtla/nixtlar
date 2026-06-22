# Historical Forecast

``` r

library(nixtlar)
```

## 1. TimeGPT Historical Forecast

When generating a forecast, sometimes you might be interested in
forecasting the historical observations. These predictions, known as
**fitted values**, can help you better understand and evaluate a model’s
performance over time.

`TimeGPT` has a method for generating fitted values, and users can call
it from `nixtlar`. This vignette will explain how to do this. It assumes
you have already set up your API key. If you haven’t done this, please
read the [Get
Started](https://nixtla.github.io/nixtlar/articles/get-started.html)
vignette first.

## 2. Load data

For this vignette, we’ll use the electricity consumption dataset that is
included in `nixtlar`, which contains the hourly prices of five
different electricity markets.

``` r

df <- nixtlar::electricity
head(df)
#>   unique_id                  ds     y
#> 1        BE 2016-10-22 00:00:00 70.00
#> 2        BE 2016-10-22 01:00:00 37.10
#> 3        BE 2016-10-22 02:00:00 37.10
#> 4        BE 2016-10-22 03:00:00 44.75
#> 5        BE 2016-10-22 04:00:00 37.10
#> 6        BE 2016-10-22 05:00:00 35.61
```

## 3. Forecast historical data

To generate a forecast for the historical data, use
[`nixtlar::nixtla_client_historic`](https://nixtla.github.io/nixtlar/reference/nixtla_client_historic.md),
which should include the following parameters:

- **df**: The time series data, provided as a data frame, tibble, or
  tsibble. It must include at least two columns: one for the timestamps
  and one for the observations. The default names for these columns are
  `ds` and `y`. If your column names are different, specify them with
  `time_col` and `target_col`, respectively. If you are working with
  multiple series, you must also include a column with unique
  identifiers. The default name for this column is `unique_id`; if
  different, specify it with `id_col`.
- **level**: The prediction intervals for the forecast.

``` r

nixtla_client_fitted_values <- nixtla_client_historic(df, level = c(80,95))
#> Frequency chosen: h
head(nixtla_client_fitted_values)
#>   unique_id                  ds  TimeGPT TimeGPT-lo-80 TimeGPT-lo-95
#> 1        BE 2016-10-24 00:00:00 46.99983      44.54957      44.09014
#> 2        BE 2016-10-24 01:00:00 42.07846      35.73659      34.54749
#> 3        BE 2016-10-24 02:00:00 40.89080      31.17443      29.35261
#> 4        BE 2016-10-24 03:00:00 38.80745      22.75283      19.74259
#> 5        BE 2016-10-24 04:00:00 38.59341      18.62503      14.88096
#> 6        BE 2016-10-24 05:00:00 36.58477      16.03442      12.18123
#>   TimeGPT-hi-80 TimeGPT-hi-95
#> 1      49.45010      49.90952
#> 2      48.42033      49.60943
#> 3      50.60717      52.42899
#> 4      54.86207      57.87231
#> 5      58.56179      62.30587
#> 6      57.13511      60.98830
```

Notice that there are no fitted values for some of the initial
observations. This is because `TimeGPT` requires a minimum number of
values to generate a forecast for the historical data.

All the fitted values are generated using a rolling window, meaning that
the fitted value for observation $`T`$ is generated using the first
$`T-1`$ observations.

### 3.1 Fitted values from `nixtlar::nixtla_client_forecast`

[`nixtlar::nixtla_client_historic`](https://nixtla.github.io/nixtlar/reference/nixtla_client_historic.md)
is the dedicated function that calls `TimeGPT`’s method for generating
fitted values. However, you can also use
[`nixtlar::nixtla_client_forecast`](https://nixtla.github.io/nixtlar/reference/nixtla_client_forecast.md)
with `add_history=TRUE`. This will generate both a forecast for the
historical data and for the next $`h`$ future observations.
