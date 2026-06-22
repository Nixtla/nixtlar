# Generate output dates for forecast method. This is a private function of 'nixtlar'

Generate output dates for forecast method. This is a private function of
'nixtlar'

## Usage

``` r
.generate_output_dates(df_info, freq, h)
```

## Arguments

- df_info:

  A data frame that is created by the forecast method with the last
  dates of every unique id.

- freq:

  The frequency of the data, as a period or offset alias.

- h:

  The forecast horizon.

## Value

A data frame with dates for the forecast.

## Examples

``` r
if (FALSE) { # \dontrun{
  dates_df <- .generate_output_dates(df_info, freq, h)
} # }
```
