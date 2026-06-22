# Validate future exogenous variables (if applicable) This is a private function of 'nixtlar'

Validate future exogenous variables (if applicable) This is a private
function of 'nixtlar'

## Usage

``` r
.validate_exogenous(df, h, X_df)
```

## Arguments

- df:

  A tsibble or a data frame with time series data.

- h:

  Forecast horizon.

- X_df:

  A tsibble or a data frame with future exogenous variables.

## Value

If the validation is successful, the function executes without errors.
If the validation fails, it stops execution and returns an error message
indicating that the future exogenous variables must cover the forecast
horizon.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- nixtlar::electricity_exo_vars
X_df <- nixtlar::electricity_future_exo_vars
.validate_exogenous(df, h=24, X_df)
} # }
```
