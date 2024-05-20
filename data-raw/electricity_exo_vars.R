
## Electricity dataset with exogenous variables

electricity_exo_vars <- read.csv('https://raw.githubusercontent.com/Nixtla/transfer-learning-time-series/main/datasets/electricity-short-with-ex-vars.csv')

usethis::use_data(electricity_exo_vars, overwrite = TRUE)
