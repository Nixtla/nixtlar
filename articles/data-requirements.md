# Data Requirements

``` r

library(nixtlar)
```

This vignette explains the data requirements for using any of the core
functions of `nixtlar`:

``` r

# Core functions of `nixtlar` 
- nixtlar::nixtla_client_forecast()
- nixtlar::nixtla_client_historic()
- nixtlar::nixtla_client_detect_anomalies()
- nixtlar::nixtla_client_cross_validation()
- nixtlar::nixtla_client_plot()
```

## 1. Input Requirements

`nixtlar` now supports the following data structures: data frames,
tibbles, and tsibbles. The output format will always be a data frame.

Regardless of your data structure, the following two columns must always
be included when using any core functions of `nixtlar`:

- **Date Column**: This column must contain timestamps formatted as
  `YYYY-MM-DD` or `YYYY-MM-DD hh:mm:ss`, either as characters or
  date-time objects. For date-time objects, we recommend using the
  `as.POSIX*` functions from base R, although `as.Date` is also
  supported. The default name for this column is `ds`. If your dataset
  uses a different name, please specify it by setting the parameter
  `time_col="your_time_column_name"`.

- **Target Column**: This column should contain the numeric target
  variable for forecasting. The default name for this column is `y`. If
  your dataset uses a different name, specify it by setting the
  parameter `target_col="your_target_column_name"`.

## 2. Multiple Series

If you are working with multiple series, you must include a column with
a unique identifier for each series. This column can contain characters
or integers, and its default name is `unique_id`. If your dataset uses a
different name for the identifier column, please specify it by setting
the parameter `id_col="your_id_column_name"`. If your dataset contains
only one series and does not need an identifier, set `id_col` to `NULL`.

Please be aware that in earlier versions of `nixtlar`, the default name
for `id_col` was `NULL`, but it is now `unique_id`.

``` r

# sample valid input 
df <- nixtlar::electricity
head(df)
#>   unique_id                  ds     y
#> 1        BE 2016-10-22 00:00:00 70.00
#> 2        BE 2016-10-22 01:00:00 37.10
#> 3        BE 2016-10-22 02:00:00 37.10
#> 4        BE 2016-10-22 03:00:00 44.75
#> 5        BE 2016-10-22 04:00:00 37.10
#> 6        BE 2016-10-22 05:00:00 35.61
str(df)
#> 'data.frame':    8400 obs. of  3 variables:
#>  $ unique_id: chr  "BE" "BE" "BE" "BE" ...
#>  $ ds       : chr  "2016-10-22 00:00:00" "2016-10-22 01:00:00" "2016-10-22 02:00:00" "2016-10-22 03:00:00" ...
#>  $ y        : num  70 37.1 37.1 44.8 37.1 ...
```

## 3. Exogenous Variables

When using exogenous variables, `nixtlar` distinguishes between
historical and future exogenous variables:

- **Historical Exogenous Variables**: These should be included in the
  input data immediately following the `id_col`, `ds`, and `y` columns.
  If your dataset contains additional columns that are not exogenous
  variables, you must remove them before using any core functions of
  `nixtlar`.

- **Future Exogenous Variables**: These correspond to the `X_df`
  parameter and should cover the entire forecast horizon. This dataset
  must include columns with the appropriate timestamps and, if
  applicable, unique identifiers, formatted as described in the previous
  sections.

``` r

# sample valid input with exogenous variables 
df <- nixtlar::electricity_exo_vars
head(df)
#>   unique_id                  ds     y Exogenous1 Exogenous2 day_0 day_1 day_2
#> 1        BE 2016-10-22 00:00:00 70.00      49593      57253     0     0     0
#> 2        BE 2016-10-22 01:00:00 37.10      46073      51887     0     0     0
#> 3        BE 2016-10-22 02:00:00 37.10      44927      51896     0     0     0
#> 4        BE 2016-10-22 03:00:00 44.75      44483      48428     0     0     0
#> 5        BE 2016-10-22 04:00:00 37.10      44338      46721     0     0     0
#> 6        BE 2016-10-22 05:00:00 35.61      44504      46303     0     0     0
#>   day_3 day_4 day_5 day_6
#> 1     0     0     1     0
#> 2     0     0     1     0
#> 3     0     0     1     0
#> 4     0     0     1     0
#> 5     0     0     1     0
#> 6     0     0     1     0

future_exo_vars <- nixtlar::electricity_future_exo_vars
head(future_exo_vars)
#>   unique_id                  ds Exogenous1 Exogenous2 day_0 day_1 day_2 day_3
#> 1        BE 2016-12-31 00:00:00      64108      70318     0     0     0     0
#> 2        BE 2016-12-31 01:00:00      62492      67898     0     0     0     0
#> 3        BE 2016-12-31 02:00:00      61571      68379     0     0     0     0
#> 4        BE 2016-12-31 03:00:00      60381      64972     0     0     0     0
#> 5        BE 2016-12-31 04:00:00      60298      62900     0     0     0     0
#> 6        BE 2016-12-31 05:00:00      60339      62364     0     0     0     0
#>   day_4 day_5 day_6
#> 1     0     1     0
#> 2     0     1     0
#> 3     0     1     0
#> 4     0     1     0
#> 5     0     1     0
#> 6     0     1     0
```

To learn more about how to use exogenous variables, please refer to the
[Exogenous variables
vignette](https://nixtla.github.io/nixtlar/articles/exogenous-variables.html).

## 4. Missing values

When using `TimeGPT` via `nixtlar`, ensure the following:

1.  **No Missing Values in the Target Column**: The target column must
    not contain any missing values (`NA`).

2.  **Continuous Date Sequence**: The dates must be continuous, without
    any gaps, from the start date to the end date, matching the
    frequency of the data.

Currently, **nixtlar** does not provide any functionality to fill
missing values or dates. To learn more about this, please refer to the
vignette on [Special
Topics](https://nixtla.github.io/nixtlar/articles/special-topics.html).

## 5. Minimum data requirements

The minimum size **per series** to obtain results from
[`nixtlar::nixtla_client_forecast`](https://nixtla.github.io/nixtlar/reference/nixtla_client_forecast.md)
is one, regardless of the frequency of the data. Keep in mind, however,
that this will produce results with limited accuracy.

For certain scenarios, more than one observation may be necessary:

- When using the parameters `level`, `quantiles`, or `finetune_steps`.
- When incorporating exogenous variables.
- When including historical forecasts by setting `add_history=TRUE`.

The minimum data requirement varies with the frequency of the data,
detailed in the official [TimeGPT
documentation](https://www.nixtla.io/docs/data_requirements/data_requirements).

When using
[`nixtlar::nixtla_client_cross_validation`](https://nixtla.github.io/nixtlar/reference/nixtla_client_cross_validation.md),
you also need to consider the forecast horizon (`h`), the number of
windows (`n_windows`) and the step size (`step_size`). The formula for
the minimum data points required per series is:

``` math
\begin{equation}
\text{Min per series} = \text{Min per frequency}+h+\text{step_size}*(\text{n_windows}-1)
\end{equation}
```

Here, $`\text{Min per frequency}`$ refers to the values specified in the
table from the official documentation.
