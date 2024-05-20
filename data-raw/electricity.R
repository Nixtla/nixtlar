
# Electricity dataset

electricity <- read.csv("https://raw.githubusercontent.com/Nixtla/transfer-learning-time-series/main/datasets/electricity-short.csv")

usethis::use_data(electricity, overwrite = TRUE)
