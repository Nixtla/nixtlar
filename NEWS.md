# nixtlar 2.0.0

`nixtlar` is now a thin wrapper over the official Python `nixtla` client (0.9.0), called through `reticulate`.

## Breaking changes

- The engine is the Python `nixtla` client. It is installed automatically on first use (an internet connection is needed once); no manual Python setup is required.
- The default model is now the Python client's default (`timegpt-2.1`).
- `nixtla_client_detect_anomalies` now performs online detection: `h` and `detection_size` are required.
- `ds` columns are returned as `POSIXct`.
- Removed the internal `.`-prefixed exports (`.generate_output_dates`, `.get_client_steup`, `.get_client_version`, `.get_model_params`, `.level_from_quantiles`, `.r_frequency`, `.transient_errors`, `.validate_exogenous`). `httr2` and `purrr` are no longer dependencies.

## New features

- Fine-tuning: `nixtla_client_finetune`, `nixtla_client_finetuned_models`, `nixtla_client_finetuned_model`, `nixtla_client_delete_finetuned_model`.
- `nixtla_client_simulate`, `nixtla_client_explain`, `nixtla_client_audit_data`, `nixtla_client_clean_data`, `nixtla_client_usage` and `nixtla_client_list_models`.


# nixtlar 1.0.0  

This release consolidates all changes since the last CRAN release (0.6.2), including the development versions 0.6.3 and 0.6.4.  

## Breaking changes  

- `nixtlar` now requires R (>= 4.1.0). 
- Set default value for `id_col` to `unique_id` in `nixtla_client_historic` to match the other core functions. 

## New features  

- Added support for distinguishing between historic and future exogenous variables in the forecasting and cross-validation functions. This feature was available in the development versions 0.6.3 and 0.6.4, but not in the CRAN version (0.6.2).  

## Bug fixes  

- Fixed an HTTP 422 error when using future exogenous variables with `h = 1`.  
- Updated internal functions to use the `GET` method, as the `POST` method is no longer supported. 
- Fixed in-sample timestamp reconstruction in `nixtla_client_historic` for series that share some but not all in-sample sizes.  
- Fixed handling of mixed date-only and datetime character `ds` values that previously produced "failed to parse" warnings and `NA` timestamps.  
- Made output date-format detection robust across the whole `ds` column in `nixtla_client_historic`, `nixtla_client_detect_anomalies`, `nixtla_client_forecast`, and `nixtla_client_cross_validation`, so a leading midnight value no longer truncates the time component of the remaining rows.  

# nixtlar 0.6.4  

- Development version. Release notes available [here](https://github.com/Nixtla/nixtlar/releases/tag/v0.6.4).  
- Added support for distinguishing between historic and future exogenous variables in forecasting and cross-validation functions. Users can now specify which exogenous variables have only historical values and which include both historical and future values.  

# nixtlar 0.6.2  

- Current stable release. Release notes available [here](https://github.com/Nixtla/nixtlar/releases/tag/v0.6.2).  

# nixtlar 0.6.1  

- Updated to support `TimeGPT` API v2.  
- Added support for `tibbles`, in addition to data frames and tsibbles.  
- Standardized date format requirements to `YYYY-MM-DD` or `YYYY-MM-DD hh:mm:ss`, as character strings or date-time objects.  
- Default `id_col` set to `unique_id`, aligning with the Python SDK. If the dataset contains a single series, `id_col=NULL` should be used. Only character or integer values are accepted for `id_col`.  
- Release notes available [here](https://github.com/Nixtla/nixtlar/releases/tag/v0.6.1).  

# nixtlar 0.6.0  

- Updated to use `TimeGPT` API v2. Release notes available [here](https://github.com/Nixtla/nixtlar/releases/tag/v0.6.0).  

# nixtlar 0.5.4  

- Development version. Release notes available [here](https://github.com/Nixtla/nixtlar/releases/tag/v0.5.4).  

# nixtlar 0.5.3  

- Development version. Release notes available [here](https://github.com/Nixtla/nixtlar/releases/tag/v0.5.3).  

# nixtlar 0.5.0  

- Initial CRAN submission.  
