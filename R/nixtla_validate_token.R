#' Validate token
#'
#' @return A status code and a message indicating whether the token is valid.
#' @export
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_set_token("YOUR_TOKEN")
#'   nixtlar::nixtla_validate_token
#' }
#'
nixtla_validate_token <- function(){

  token <- .get_token()

  url <- "https://dashboard.nixtla.io/api/timegpt"
  resp <- httr2::request(url) |>
    httr2::req_headers(
      Authorization = paste("Bearer", token)
      ) |>
    httr2::req_user_agent("nixtla-r") |>
    httr2::req_perform()

  status_code <- httr2::resp_status(resp)
  if(status_code >= 200 & status_code < 300){
    message("Token validation successful. Happy forecasting! :) \nIf you have questions or need support, please email ops@nixtla.io")
  }else{
    message("Token validation failed. Please go https://dashboard.nixtla.io/ to get a valid token.")
  }
}
