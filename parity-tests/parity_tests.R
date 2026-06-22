# Parity tests for nixtlar (R) vs nixtla (Python). Run from the parity-tests folder.
# Writes each TimeGPT operation output to output/<test>_r.csv.

library(nixtlar)
readRenviron("../.env") # load the API key from .env into the session.

h <- 24 # forecast horizon

df_url <- "https://raw.githubusercontent.com/Nixtla/transfer-learning-time-series/main/datasets/electricity-short.csv"
df_exo_url <- "https://raw.githubusercontent.com/Nixtla/transfer-learning-time-series/main/datasets/electricity-short-with-ex-vars.csv"
df_future_exo_url <- "https://raw.githubusercontent.com/Nixtla/transfer-learning-time-series/main/datasets/electricity-short-future-ex-vars.csv"

dir.create("output", showWarnings = FALSE)

write_output <- function(df, name) {
  write.csv(df, file.path("output", paste0(name, "_r.csv")), row.names = FALSE)
}

df <- read.csv(df_url)
df_exo <- read.csv(df_exo_url)
df_future_exo <- read.csv(df_future_exo_url)

# Keep ds as character to avoid timezone issues in the output timestamps.
df$ds <- as.character(df$ds)
df_exo$ds <- as.character(df_exo$ds)
df_future_exo$ds <- as.character(df_future_exo$ds)

hist_exog_list <- setdiff(names(df_exo), c("unique_id", "ds", "y"))

# 1. forecast
write_output(nixtla_client_forecast(df, h = h), "forecast")

# 2. forecast with prediction intervals
write_output(nixtla_client_forecast(df, h = h, level = c(80, 95)), "forecast_intervals")

# 3. forecast with quantiles
write_output(nixtla_client_forecast(df, h = h, quantiles = c(0.25, 0.75)), "forecast_quantiles")

# 4. forecast with fine-tuning
write_output(
  nixtla_client_forecast(df, h = h, finetune_steps = 5, finetune_depth = 2, finetune_loss = "mae"),
  "forecast_finetune"
)

# 5. forecast with add_history=True
write_output(nixtla_client_forecast(df, h = h, add_history = TRUE), "forecast_add_history")

# 6. historic forecast (only returns in-sample values)
# There is no real equivalent method to this in nixtla, as the forecast method with 
# add_history=True returns the in-sample values and the h future values.
write_output(nixtla_client_historic(df), "historic")

# 7. cross-validation
write_output(nixtla_client_cross_validation(df, h = h), "cross_validation")

# 8. anomaly detection
write_output(nixtla_client_detect_anomalies(df), "anomaly_detection")

# 9. forecast with historic exogenous variables
write_output(
  nixtla_client_forecast(df_exo, h = h, hist_exog_list = hist_exog_list),
  "forecast_hist_exog"
)

# 10. forecast with future exogenous variables
write_output(
  nixtla_client_forecast(df_exo, h = h, X_df = df_future_exo),
  "forecast_future_exog"
)

message("R parity tests complete. Outputs in parity-tests/output/")
