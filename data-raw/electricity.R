
# Electricity dataset

electricity <- read.csv("https://raw.githubusercontent.com/Nixtla/transfer-learning-time-series/main/datasets/electricity-short.csv")
electricity$y <- round(electricity$y, 5)

usethis::use_data(electricity, overwrite = TRUE)
