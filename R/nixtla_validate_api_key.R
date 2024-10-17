#' Validate 'API' key
#'
#' @return TRUE if the API key is valid, FALSE otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_client_setup(api_key = "Your API key")
#'   nixtlar::nixtla_validate_api_key()
#' }
#'
nixtla_validate_api_key <- function(){

  setup <- .get_client_steup()

  req <- httr2::request(paste0(setup$base_url, "validate_token")) |>
    httr2::req_headers(
      "accept" = "application/json",
      "content-type" = "application/json",
      "authorization" = paste("Bearer", setup$api_key)
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
