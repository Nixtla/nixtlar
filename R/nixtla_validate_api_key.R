#' Validate 'API' key
#'
#' @return A status code and a message indicating whether the 'API' key is valid.
#' @export
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_set_api_key("YOUR_API_KEY")
#'   nixtlar::nixtla_validate_api_key
#' }
#'
nixtla_validate_api_key <- function(){

  api_key <- .get_api_key()

  url <- "https://dashboard.nixtla.io/api/forecast"
  resp <- httr2::request(url) |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key)
      ) |>
    httr2::req_user_agent("nixtla-r") |>
    httr2::req_perform()

  status_code <- httr2::resp_status(resp)
  if(status_code >= 200 & status_code < 300){
    message("API key validation successful. Happy forecasting! :) \nIf you have questions or need support, please email ops@nixtla.io")
  }else{
    message("API key validation failed. Please go https://dashboard.nixtla.io/ to get a valid API key.")
  }
}
