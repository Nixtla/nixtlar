#' Retrieve parameters for 'TimeGPT' model
#' This is a private function of 'nixtlar'
#'
#' @param model Model to use, either "timegpt-1" or "timegpt-1-long-horizon".
#' @param freq Frequency of the data.
#'
#' @return A list with the model's input size and horizon
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   .get_model_params()
#' }
#'
.get_model_params <- function(model, freq){

  payload_params <- list(
    model = model,
    freq = freq
  )

  req <- httr2::request("https://api.nixtla.io/model_params") |>
    httr2::req_headers(
      "accept" = "application/json",
      "content-type" = "application/json",
      "authorization" = paste("Bearer", .get_api_key())
    ) |>
    httr2::req_user_agent("nixtlar") |>
    httr2::req_body_json(data = payload_params) |>
    httr2::req_retry(
      max_tries = 6,
      is_transient = .transient_errors
    )

  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  model_params <- list(
    input_size = resp$detail$input_size,
    horizon = resp$detail$horizon
  )

  return(model_params)
}
