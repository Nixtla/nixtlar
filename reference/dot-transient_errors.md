# A function used by httr2::req_retry() to determine if the response represents a transient error This is a private function of 'nixtlar'

A function used by httr2::req_retry() to determine if the response
represents a transient error This is a private function of 'nixtlar'

## Usage

``` r
.transient_errors(resp)
```

## Arguments

- resp:

  The response to a HTTP request

## Value

TRUE if the response status is 500 or 502, FALSE otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
.transient_errors(resp)
} # }
```
