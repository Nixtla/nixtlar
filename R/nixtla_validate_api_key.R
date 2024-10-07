#' Validate 'API' key
#'
#' @return TRUE if the API key is valid, FALSE otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_set_api_key("YOUR_API_KEY")
#'   nixtlar::nixtla_validate_api_key()
#' }
#'
nixtla_validate_api_key <- function(){

  api_key <- .get_api_key()
  url <- "https://api.nixtla.io/validate_token"

  req <- httr2::request(url) |>
    httr2::req_headers(
      "accept" = "application/json",
      "content-type" = "application/json",
      "authorization" = paste("Bearer", api_key)
    ) |>
    httr2::req_user_agent("nixtlar") |>
    httr2::req_body_json(data = NULL)


  resp <- tryCatch({
    req |>
      httr2::req_perform()
  }, error = function(err){
    NULL
  })

  if(is.null(resp)){
    result <- FALSE
  }else{
    status_code <- httr2::resp_status(resp)
    result <- ifelse(status_code == 200, TRUE, FALSE)
  }

  return(result)
}
