
# nixtlar 0.6.5  

- Updated `.get_model_params` to use the `GET` method for the `/model_params` endpoint, passing `model` and `freq` as query parameters (the `POST` method is no longer supported).  
- Replaced the deprecated `/validate_token` endpoint with `/validate_api_key`, now accessed via the `GET` method, in `nixtla_validate_api_key`.  
- Added the `nixtla-model` request header to the forecast, historic, cross-validation, and anomaly detection requests to support different models.  
- Removed the unused `.make_request` helper and dropped the `future` and `future.apply` dependencies.  
- Set default value for `id_col` to `unique_id` in `nixtla_client_historic` to match the other core functions.  

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
