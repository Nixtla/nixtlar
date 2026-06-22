# Electricity dataset with exogenous variables

Contains prices of different electricity markets with exogenous
variables.

## Usage

``` r
electricity_exo_vars
```

## Format

### `electricity_exo_vars`

A data frame with 8400 rows and 12 columns:

- unique_id:

  Unique identifiers of the electricity markets.

- ds:

  Date in format YYYY:MM:DD hh:mm:ss.

- y:

  Price for the given market and date.

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

<https://raw.githubusercontent.com/Nixtla/transfer-learning-time-series/main/datasets/electricity-short.csv>
