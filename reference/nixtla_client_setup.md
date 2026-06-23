# Set base 'ULR' and 'API' key in global environment

Set base 'ULR' and 'API' key in global environment

## Usage

``` r
nixtla_client_setup(base_url = NULL, api_key = NULL)
```

## Arguments

- base_url:

  Custom base 'URL'. If NULL, defaults to "https://api.nixtla.io/".

- api_key:

  The user's 'API' key. Get yours here: https://www.nixtla.io/dashboard

## Value

A message indicating the configuration status.

## Examples

``` r
if (FALSE) { # \dontrun{
  nixtlar::nixtla_client_setup(
    base_url = "Base URL",
    api_key = "Your API key"
  )
} # }
```
