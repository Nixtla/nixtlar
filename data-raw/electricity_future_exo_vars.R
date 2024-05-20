## code to prepare `electricity_future_exo_vars` dataset goes here

electricity_future_exo_vars <- read.csv('https://raw.githubusercontent.com/Nixtla/transfer-learning-time-series/main/datasets/electricity-short-future-ex-vars.csv')

usethis::use_data(electricity_future_exo_vars, overwrite = TRUE)
