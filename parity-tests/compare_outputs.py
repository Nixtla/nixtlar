"""Compare nixtlar (R) and nixtla (Python) parity-test outputs.

Run from the parity-tests folder. For each test, the R and Python CSVs are
aligned on their key columns and the TimeGPT/interval/quantile columns are
compared within a tolerance. The boolean anomaly flag is compared for exact
equality.
"""

import numpy as np
import pandas as pd

# Tolerance for numeric comparisons (relative + absolute).
rtol = 0.01
atol = 1e-6

tests = [
    "forecast",
    "forecast_intervals",
    "forecast_quantiles",
    "forecast_finetune",
    "forecast_add_history",
    "historic",
    "cross_validation",
    "anomaly_detection",
    "forecast_hist_exog",
    "forecast_future_exog",
]

key_candidates = ["unique_id", "ds", "cutoff"]
# Columns that are inputs (not predictions) and therefore not compared.
ignore_cols = ["y"]


def load(name, suffix):
    # Base R write.csv renders midnight timestamps from nixtlar as date-only ("YYYY-MM-DD"), 
    # so pad those back to full "YYYY-MM-DD HH:MM:SS" form.
    df = pd.read_csv(f"output/{name}_{suffix}.csv", dtype={"ds": str, "cutoff": str})
    for col in ("ds", "cutoff"):
        if col in df.columns:
            df[col] = df[col].where(df[col].str.len() != 10, df[col] + " 00:00:00")
    return df


def compare_test(name):
    r = load(name, "r")
    p = load(name, "py")

    keys = [c for c in key_candidates if c in r.columns and c in p.columns]
    merged = r.merge(p, on=keys, suffixes=("_r", "_py"), how="outer")
    nrows = len(merged)

    compare_cols = [
        c
        for c in r.columns
        if c not in keys and c not in ignore_cols and c in p.columns
    ]

    rows = []
    overall = "PASS"
    for col in compare_cols:
        a = merged[f"{col}_r"]
        b = merged[f"{col}_py"]

        if col == "anomaly":
            n_mismatch = int((a.astype(bool) != b.astype(bool)).sum())
            status = "PASS" if n_mismatch == 0 else "FAIL"
            rows.append((name, status, nrows, col, "", f"{n_mismatch} flag mismatches"))
        else:
            a = a.astype(float).to_numpy()
            b = b.astype(float).to_numpy()
            abs_diff = np.abs(a - b)
            rel_diff = abs_diff / np.maximum(np.abs(b), atol)
            within = np.isclose(a, b, rtol=rtol, atol=atol)
            status = "PASS" if within.all() else "FAIL"
            rows.append(
                (name, status, nrows, col, f"{abs_diff.max():.6g}", f"{rel_diff.max():.6g}")
            )

        if status != "PASS":
            overall = "FAIL"

    return {"rows": rows, "overall": overall}


def main():
    records = []
    print(f"{'test':<22}{'col':<16}{'status':<8}{'max_abs_diff':<16}{'max_rel_diff':<16}rows")
    print("-" * 90)

    all_pass = True
    for name in tests:
        result = compare_test(name)
        for test, status, nrows, col, max_abs, max_rel in result["rows"]:
            print(f"{test:<22}{col:<16}{status:<8}{max_abs:<16}{max_rel:<16}{nrows}")
            records.append(
                {
                    "test": test,
                    "column": col,
                    "status": status,
                    "max_abs_diff": max_abs,
                    "max_rel_diff": max_rel,
                    "rows": nrows,
                }
            )
        if result["overall"] != "PASS":
            all_pass = False

    summary = pd.DataFrame.from_records(records)
    summary.to_csv("output/comparison_summary.csv", index=False)

    print("-" * 90)
    print(f"Overall: {'ALL TESTS PASS' if all_pass else 'SOME TESTS FAILED'} "
          f"(rtol={rtol}, atol={atol})")
    print("Summary written to output/comparison_summary.csv")


if __name__ == "__main__":
    main()
