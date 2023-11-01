
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nixtlaR

The `nixtlaR` package provides R users with a SDK for [Nixtla’s TimeGPT
API](https://docs.nixtla.io/).

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/nixtlaR)](https://CRAN.R-project.org/package=nixtlaR)
[![R-CMD-check](https://github.com/MMenchero/nixtlaR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MMenchero/nixtlaR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## 🔄 nixtlaR: Your Gateway to TimeGPT from R

With `nixtlaR` you can easily interact with TimeGPT through simple API
calls, making the power of TimeGPT readily accessible in your projects.
Learn more about TimeGPT [here](https://arxiv.org/abs/2310.03589).

## 💻 Installation

You can install the development version of `nixtlaR` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
#devtools::install_github("MMenchero/nixtlaR")
```

## 🎈Quick Start

To use TimeGPT, you first need to request a token from Nixtla. You can
do this at <https://dashboard.nixtla.io/>.

Once you have the token, you’ll need to set it up in your R session. The
`nixtlaR` package provides a function to do this.

``` r
#library(nixtlaR)

# Set up token 
#set_token("YOUR_TOKEN") 
```

You’ll need to set the token every time you re-start your R session.
`nixtlaR` also includes a function to validate the token.

``` r
# Validate token
#validate_token("YOUR_TOKEN") 
```

You don’t need to validate the token every time you set it up, only when
you want to ensure its validity.

`nixtlaR provides` an easy-to-use function to call the TimeGPT API. It
works with both [tsibbles](https://tsibble.tidyverts.org/) and base R
data frames.

``` r
# Call the TimeGPT API 
#df <- tssible::as_tsibble(AirPassengers) # df can also be a data frame  

#timegpt_forecast(df, h=8, freq=12, time_col="index", target_col="value")
```

## 🐍 Python SDK

Are you a Python user? If yes, then check out
[NixtlaTS](https://github.com/Nixtla/nixtla), a Python SDK for TimeGPT.
