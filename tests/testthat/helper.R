
skip_if_no_token <- function() {
  tryCatch({
    nixtlar::.get_client_steup()
  }, error = function(e) {
    testthat::skip("NIXTLA_API_KEY is not set")
  })
}
