
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- logo -->

# nixtlar <a href="https://nixtla.github.io/nixtlar/"><img src="man/figures/logo.png" align="right" height="139" alt="nixtlar website" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/nixtlar)](https://CRAN.R-project.org/package=nixtlar)
[![R-CMD-check](https://github.com/MMenchero/nixtlar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MMenchero/nixtlar/actions/workflows/R-CMD-check.yaml)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue)](https://www.apache.org/licenses/LICENSE-2.0)
<!-- badges: end -->

The `nixtlar` package provides R users with a SDK for [Nixtla’s
TimeGPT](https://docs.nixtla.io/).

## Your gateway to TimeGPT

TimeGPT is a cutting-edge generative pre-trained transformer model that
has been trained on the largest collection of publicly available time
series data. Accessible to R users via the `nixtlar package`, TimeGPT
democratizes forecasting, enabling rapid and accurate predictions, even
for datasets not seen during training. Learn more about TimeGPT
[here](https://arxiv.org/abs/2310.03589).

## Installation

You can install the development version of `nixtlar` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Nixtla/nixtlar")
```

## Example

Get started with TimeGPT now.

``` r
library(nixtlar)

# Load sample dataset 
df <- nixtlar::electricity # can also be a tsibble! 

# Set TIMEGPT Token 
nixtlar::nixtla_set_token("TIMEGPT TOKEN")
#> Token has been set for the current session.

# Forecast the next 8 horizons using TimeGPT
fcst <- nixtlar::timegpt_forecast(df, h = 8, id_col = "unique_id", level = c(80,95))
#> Frequency chosen: H

# Plot TimeGPT forecast 
nixtlar::timegpt_plot(df, fcst, h = 8, id_col = "unique_id", max_insample_length = 100)
#> Frequency chosen: H
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

Using TIMEGPT through `nixtlar` requires a token provided by Nixtla. To
see how to get it and different ways to set it up, please read the [Get
Started](https://nixtla.github.io/nixtlar/articles/get-started.html).
Here you can also find a more detailed explanation on how `nixtlar`’s
forecast and plot function work.

## Main features of `nixtlar`

- Works with both data frames and
  [tsibbles](https://tsibble.tidyverts.org/).

- Allows you to use TimeGPT’s main features, including:

  - Anomaly detection
  - Exogenous variables
  - Prediction intervals
  - Finetuning
  - Multiple time series
  - Historical forecast
  - Cross-validation

- Includes its own plot function.

Please read the **Articles** on this website or check out the
**Reference** to understand how you can use all these features.

## Python SDK

Are you a Python user? If yes, then check out the [Python SDK for
TimeGPT](https://github.com/Nixtla/nixtla).
