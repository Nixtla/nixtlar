"""Parity tests for nixtla (Python) vs nixtlar (R). Run from the parity-tests folder."""

import os
from pathlib import Path

import pandas as pd
from dotenv import load_dotenv
from nixtla import NixtlaClient

load_dotenv(Path(__file__).resolve().parent.parent / ".env")

h = 24

df_url = "https://raw.githubusercontent.com/Nixtla/transfer-learning-time-series/main/datasets/electricity-short.csv"
df_exo_url = "https://raw.githubusercontent.com/Nixtla/transfer-learning-time-series/main/datasets/electricity-short-with-ex-vars.csv"
df_future_exo_url = "https://raw.githubusercontent.com/Nixtla/transfer-learning-time-series/main/datasets/electricity-short-future-ex-vars.csv"

os.makedirs("output", exist_ok=True)

def write_output(df, name):
    df.to_csv(f"output/{name}_py.csv", index=False)

nixtla_client = NixtlaClient(api_key=os.getenv("NIXTLA_API_KEY"))

df = pd.read_csv(df_url, parse_dates=["ds"])
df_exo = pd.read_csv(df_exo_url, parse_dates=["ds"])
df_future_exo = pd.read_csv(df_future_exo_url, parse_dates=["ds"])
hist_exog_list = [c for c in df_exo.columns if c not in ("unique_id", "ds", "y")]

# 1. forecast
write_output(nixtla_client.forecast(df, h=h), "forecast")

# 2. forecast with prediction intervals
write_output(nixtla_client.forecast(df, h=h, level=[80, 95]), "forecast_intervals")

# 3. forecast with quantiles
write_output(nixtla_client.forecast(df, h=h, quantiles=[0.25, 0.75]), "forecast_quantiles")

# 4. forecast with fine-tuning
write_output(
    nixtla_client.forecast(df, h=h, finetune_steps=5, finetune_depth=2, finetune_loss="mae"),
    "forecast_finetune",
)

# 5. forecast with add_history=True
write_output(nixtla_client.forecast(df, h=h, add_history=True), "forecast_add_history")

# 6. historic forecast
# nixtla returns the in-sample values and the h future values, so we need to keep only the in-sample rows.
hist = nixtla_client.forecast(df, h=h, add_history=True)
last_ds = df.groupby("unique_id")["ds"].max().rename("last_ds")
hist = hist.merge(last_ds, on="unique_id")
hist = hist[hist["ds"] <= hist["last_ds"]].drop(columns="last_ds")
write_output(hist, "historic")

# 7. cross-validation
write_output(nixtla_client.cross_validation(df, h=h), "cross_validation")

# 8. anomaly detection
write_output(nixtla_client.detect_anomalies(df), "anomaly_detection")

# 9. forecast with historic exogenous variables
write_output(
    nixtla_client.forecast(df_exo, h=h, hist_exog_list=hist_exog_list),
    "forecast_hist_exog",
)

# 10. forecast with future exogenous variables
write_output(nixtla_client.forecast(df_exo, h=h, X_df=df_future_exo), "forecast_future_exog")

print("Python parity tests complete. Outputs in parity-tests/output/")
