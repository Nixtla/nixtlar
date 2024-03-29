% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/date_conversion.R
\name{date_conversion}
\alias{date_conversion}
\title{Infer frequency of a tsibble and convert its index to date or string.}
\usage{
date_conversion(df)
}
\arguments{
\item{df}{A tsibble.}
}
\value{
A list with the inferred frequency and df with the new index.
}
\description{
Infer frequency of a tsibble and convert its index to date or string.
}
\examples{
df <- AirPassengers
tsbl <- tsibble::as_tsibble(df)
names(tsbl) <- c("ds", "y")
date_conversion(tsbl)

}
