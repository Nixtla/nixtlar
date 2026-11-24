
skip_if_no_token <- function() {
  tryCatch({
    nixtlar::.get_client_steup()
  }, error = function(e) {
    testthat::skip("NIXTLA_API_KEY is not set")
  })
}

skip_if_no_python <- function() {
  testthat::skip_if_not_installed("reticulate")
  if (!reticulate::py_available(initialize = TRUE) ||
      !reticulate::py_module_available("nixtla")) {
    testthat::skip("Python nixtla is not available")
  }
}

skip_if_no_api <- function() {
  skip_if_no_python()
  if (Sys.getenv("NIXTLA_API_KEY") == "") testthat::skip("NIXTLA_API_KEY is not set")
}
