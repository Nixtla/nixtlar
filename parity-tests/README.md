# Parity tests

Compares the outputs of `nixtlar` (R) and `nixtla` (Python) for the same TimeGPT operations:
forecast, prediction intervals, quantiles, fine-tuning, historic, cross-validation,
anomaly detection, and forecasting with historic and future exogenous variables.

## Environment setup

Create the Python environment (uses `nixtla==0.7.5.dev0`) and install the dependencies:

```bash
uv venv .venv
uv pip install --python .venv -r requirements.txt
```

The NIXTLA_API_KEY is read from the `.env` file in the root directory. The R script
loads it automatically, but the Python script reads it from the environment, so export
it first:

```bash
export $(grep -v '^#' ../.env | xargs)
```

## Run

From the `parity-tests` folder:

```bash
Rscript parity_tests.R          
.venv/bin/python parity_tests.py   
.venv/bin/python compare_outputs.py
```

`compare_outputs.py` aligns each pair on its key columns and checks the forecast
columns within a tolerance (`rtol=0.01`), printing a PASS/FAIL summary and writing
`output/comparison_summary.csv`.
