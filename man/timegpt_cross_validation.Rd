% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timegpt_cross_validation.R
\name{timegpt_cross_validation}
\alias{timegpt_cross_validation}
\title{Perform cross validation with TimeGPT.}
\usage{
timegpt_cross_validation(
  df,
  h = 8,
  freq = NULL,
  id_col = NULL,
  time_col = "ds",
  target_col = "y",
  X_df = NULL,
  level = NULL,
  n_windows = 1,
  step_size = NULL,
  finetune_steps = 0,
  clean_ex_first = TRUE,
  model = "timegpt-1"
)
}
\arguments{
\item{df}{A tsibble or a data frame with time series data.}

\item{h}{Forecast horizon.}

\item{freq}{Frequency of the data.}

\item{id_col}{Column that identifies each series.}

\item{time_col}{Column that identifies each timestep.}

\item{target_col}{Column that contains the target variable.}

\item{X_df}{A tsibble or a data frame with future exogenous variables.}

\item{level}{The confidence levels (0-100) for the prediction intervals.}

\item{n_windows}{Number of windows to evaluate.}

\item{step_size}{Step size between each cross validation window. If NULL, it will equal the forecast horizon (h).}

\item{finetune_steps}{Number of steps used to finetune TimeGPT in the new data.}

\item{clean_ex_first}{Clean exogenous signal before making the forecasts using TimeGPT.}

\item{model}{Model to use, either "timegpt-1" or "timegpt-1-long-horizon". Use "timegpt-1-long-horizon" if you want to forecast more than one seasonal period given the frequency of the data.}
}
\value{
A tsibble or a data frame with TimeGPT's cross validation.
}
\description{
Perform cross validation with TimeGPT.
}
\examples{
\dontrun{
  nixtlar::nixtla_set_token("YOUR_TOKEN")
  df <- nixtlar::electricity
  fcst <- nixtlar::timegpt_cross_validation(df, h = 8, id_col = "unique_id", n_windows = 5)
}

}
