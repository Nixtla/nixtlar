# Future values for the electricity dataset with exogenous variables

Contains the future values of the exogenous variables of the electricity
dataset (24 steps-ahead). To be used with `electricity_exo_vars`.

## Usage

``` r
electricity_future_exo_vars
```

## Format

### `electricity_future_exo_vars`

A data frame with 120 rows and 11 columns:

- unique_id:

  Unique identifiers of the electricity markets.

- ds:

  Date in format YYYY:MM:DD hh:mm:ss.

- Exogenous1:

  An external factor influencing prices. For all markets, some form of
  day-ahead load forecast.

- Exogenous2:

  An external factor influencing prices. For "BE" and "FR" markets, the
  day-ahead generation forecast. For "NP", the day-ahead wind generation
  forecast. For "PJM", the day-ahead load forecast in a specific zone.
  For "DE", the aggregated day-ahead wind and solar generation
  forecasts.

- day_0:

  Binary variable indicating weekday.

- day_1:

  Binary variable indicating weekday.

- day_2:

  Binary variable indicating weekday.

- day_3:

  Binary variable indicating weekday.

- day_4:

  Binary variable indicating weekday.

- day_5:

  Binary variable indicating weekday.

- day_6:

  Binary variable indicating weekday.

## Source

<https://raw.githubusercontent.com/Nixtla/transfer-learning-time-series/main/datasets/electricity-short-future-ex-vars.csv>
