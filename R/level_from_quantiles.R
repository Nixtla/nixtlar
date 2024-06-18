#' Obtain level from  quantiles
#' This is a private function of 'nixtlar'
#'
#' @param quantiles A vector with the quantiles.
#'
#' @return A list containing the level vector and a data frame with the quantiles and their corresponding levels.
#' @export
#' @keywords internal
#'
#' @examples
#' .level_from_quantiles(c(0.1, 0.5, 0.9))
#'
.level_from_quantiles <- function(quantiles) {
  lvl <- 100 - 200 * quantiles

  ql_df <- data.frame(quantiles = quantiles, level = lvl)

  ql_df <- ql_df |>
    dplyr::mutate(name = ifelse(level < 0, "hi", ifelse(level > 0, "lo", NA))) |>
    dplyr::mutate(level_col = ifelse(level < 0, paste0("TimeGPT-", .data$name, level), ifelse(level > 0, paste0("TimeGPT-", .data$name, "-", level), NA))) |>
    dplyr::mutate(quantiles_col = ifelse(level == 0, "TimeGPT-q-50", paste0("TimeGPT-q-", quantiles*100)))

  level <- sort(unique(abs(lvl)))

  if (any(level == 0)) {
    level <- level[level != 0]
  }

  res <- list(
    level = level,
    ql_df = ql_df
  )

  return(res)
}
