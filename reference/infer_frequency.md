# Infer frequency of a data frame.

Infer frequency of a data frame.

## Usage

``` r
infer_frequency(df, freq)
```

## Arguments

- df:

  A data frame with time series data.

- freq:

  The frequency of the data as specified by the user; NULL otherwise.

## Value

The inferred frequency.

## Examples

``` r
df <- nixtlar::electricity
freq <- NULL
infer_frequency(df, freq)
#> Frequency chosen: h
#> [1] "h"
```
