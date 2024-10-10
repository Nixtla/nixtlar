#' Make requests to the 'TimeGPT' API
#' This is a private function of 'nixtlar'
#'
#' @param url String specifying the API endpoint to which the request is sent.
#' @param payload_list List containing the information to be sent to the 'TimeGPT' API.
#'
#' @return List representing the JSON response from the API endpoint.
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   response <- .make_request(url, payload_list_element)
#' }
#'
.make_request <- function(url, payload_list) {

  req_resp <- function(payload) {
    req <- httr2::request(url) |>
      httr2::req_headers(
        "accept" = "application/json",
        "content-type" = "application/json",
        "authorization" = paste("Bearer", .get_api_key())
      ) |>
      httr2::req_user_agent("nixtlar") |>
      httr2::req_body_json(data = payload) |>
      httr2::req_retry(
        max_tries = 6,
        is_transient = .transient_errors
      )

    resp <- req |>
      httr2::req_perform() |>
      httr2::resp_body_json()

    return(resp)
  }

  responses <- future.apply::future_lapply(payload_list, req_resp)

  return(responses)
}
