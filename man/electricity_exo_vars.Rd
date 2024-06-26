% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{electricity_exo_vars}
\alias{electricity_exo_vars}
\title{Electricity dataset with exogenous variables}
\format{
\subsection{\code{electricity_exo_vars}}{

A data frame with 8400 rows and 12 columns:
\describe{
\item{unique_id}{Unique identifiers of the electricity markets.}
\item{ds}{Date in format YYYY:MM:DD hh:mm:ss.}
\item{y}{Price for the given market and date.}
\item{Exogenous1}{An external factor influencing prices. For all markets, some form of day-ahead load forecast.}
\item{Exogenous2}{An external factor influencing prices.
For "BE" and "FR" markets, the day-ahead generation forecast.
For "NP", the day-ahead wind generation forecast.
For "PJM", the day-ahead load forecast in a specific zone.
For "DE", the aggregated day-ahead wind and solar generation forecasts.
}
\item{day_0}{Binary variable indicating weekday.}
\item{day_1}{Binary variable indicating weekday.}
\item{day_2}{Binary variable indicating weekday.}
\item{day_3}{Binary variable indicating weekday.}
\item{day_4}{Binary variable indicating weekday.}
\item{day_5}{Binary variable indicating weekday.}
\item{day_6}{Binary variable indicating weekday.}
}
}
}
\source{
\url{https://raw.githubusercontent.com/Nixtla/transfer-learning-time-series/main/datasets/electricity-short.csv}
}
\usage{
electricity_exo_vars
}
\description{
Contains prices of different electricity markets with exogenous variables.
}
\keyword{datasets}
