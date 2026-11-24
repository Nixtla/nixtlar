# audit_data / clean_data are local: they never call the API, so no key is needed.
.py_unnamed <- function() nixtla_py$NixtlaClient(api_key = "unused")

.to_r_df <- function(x) { attr(x, "pandas.index") <- NULL; x }

# pandas needs datetime64, not R Dates (which arrive as python objects).
.dates_to_posix <- function(df, time_col) {
  df <- .prep_df(df)
  if (inherits(df[[time_col]], "Date")) df[[time_col]] <- as.POSIXct(df[[time_col]], tz = "UTC")
  df
}

.audit_result <- function(res) {
  list(pass = res[[1]], fail = res[[2]], case_specific = res[[3]])
}

#' Audit data quality
#'
#' Runs local data quality checks (duplicates, missing dates, negative values, ...). No API key is needed.
#'
#' @param df A data frame with time series data.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param ... Passed to the Python method, e.g. `start` and `end`.
#'
#' @return A list with `pass` (logical), `fail` and `case_specific` (named lists of data frames).
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   audit <- nixtlar::nixtla_client_audit_data(nixtlar::electricity, freq = "h")
#' }
nixtla_client_audit_data <- function(df, freq, id_col = "unique_id", time_col = "ds", target_col = "y", ...) {
  force(freq)
  args <- .coerce_args("audit_data", list(df = .dates_to_posix(df, time_col), freq = freq, id_col = id_col,
                                          time_col = time_col, target_col = target_col, ...))
  res <- do.call(nixtla_py$NixtlaClient$audit_data, args)
  .audit_result(reticulate::py_to_r(res))
}

#' Clean data
#'
#' Cleans the data after `nixtla_client_audit_data()`. No API key is needed.
#'
#' @param df A data frame with time series data.
#' @param fail_dict The `fail` element of the `nixtla_client_audit_data()` output.
#' @param case_specific_dict The `case_specific` element of the `nixtla_client_audit_data()` output.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param ... Passed to the Python method, e.g. `agg_dict` (required to resolve duplicate rows, e.g. `list(y = "mean")`) and `clean_case_specific`.
#'
#' @return A list with `df` (the cleaned data frame), `pass`, `fail` and `case_specific` from the final audit.
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   a <- nixtlar::nixtla_client_audit_data(nixtlar::electricity, freq = "h")
#'   cl <- nixtlar::nixtla_client_clean_data(nixtlar::electricity, a$fail, a$case_specific, freq = "h")
#' }
nixtla_client_clean_data <- function(df, fail_dict, case_specific_dict, freq, id_col = "unique_id", time_col = "ds", target_col = "y", ...) {
  force(freq)
  args <- .coerce_args("clean_data", list(df = .dates_to_posix(df, time_col), fail_dict = reticulate::r_to_py(fail_dict),
                                          case_specific_dict = reticulate::r_to_py(case_specific_dict),
                                          freq = freq, id_col = id_col, time_col = time_col,
                                          target_col = target_col, ...))
  res <- reticulate::py_to_r(do.call(.py_unnamed()$clean_data, args))
  c(list(df = .to_r_df(res[[1]])), .audit_result(res[-1]))
}
