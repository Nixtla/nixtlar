% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transient_errors.R
\name{.transient_errors}
\alias{.transient_errors}
\title{A function used by httr2::req_retry() to determine if the response represents a transient error
This is a private function of 'nixtlar'}
\usage{
.transient_errors(resp)
}
\arguments{
\item{resp}{The response to a HTTP request}
}
\value{
TRUE if the response status is 500 or 502, FALSE otherwise.
}
\description{
A function used by httr2::req_retry() to determine if the response represents a transient error
This is a private function of 'nixtlar'
}
\examples{
\dontrun{
.transient_errors(resp)
}

}
\keyword{internal}
