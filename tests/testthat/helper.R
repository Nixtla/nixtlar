
skip_if_no_token <- function() {
  tryCatch({
    nixtlar::.get_token()
  }, error = function(e) {
    testthat::skip("TIMEGPT_TOKEN is not set")
  })
}
