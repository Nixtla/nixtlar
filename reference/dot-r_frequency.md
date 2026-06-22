# Convert period or offset aliases to a character string recognized by R. This is a private function of 'nixtlar'

Convert period or offset aliases to a character string recognized by R.
This is a private function of 'nixtlar'

## Usage

``` r
.r_frequency(freq)
```

## Arguments

- freq:

  The period or offset alias used by 'TimeGPT'.

## Value

A character string recognized by R for generating a regular sequence of
times.

## Examples

``` r
.r_frequency("MS")   # Returns "month"
#> [1] "month"
.r_frequency("10h")  # Returns "10 h"
#> [1] "10 h"
.r_frequency("h")    # Returns "h" (unchanged)
#> [1] "h"
```
