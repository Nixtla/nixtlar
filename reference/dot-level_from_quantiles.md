# Obtain level from quantiles This is a private function of 'nixtlar'

Obtain level from quantiles This is a private function of 'nixtlar'

## Usage

``` r
.level_from_quantiles(quantiles)
```

## Arguments

- quantiles:

  A vector with the quantiles.

## Value

A list containing the level vector and a data frame with the quantiles
and their corresponding levels.

## Examples

``` r
.level_from_quantiles(c(0.1, 0.5, 0.9))
#> $level
#> [1] 80
#> 
#> $ql_df
#>   quantiles level name     level_col quantiles_col
#> 1       0.1    80   lo TimeGPT-lo-80  TimeGPT-q-10
#> 2       0.5     0 <NA>          <NA>  TimeGPT-q-50
#> 3       0.9   -80   hi TimeGPT-hi-80  TimeGPT-q-90
#> 
```
