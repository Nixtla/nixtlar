#' Partition payload
#' This is a private function of 'nixtlar'
#'
#' @param payload A list containing the complete dataset for 'TimeGPT'.
#' @param num_partitions A positive integer, "auto", or NULL specifying the number of partitions. When set to "auto", the number of partitions is equal to the number of available cores. When NULL, it defaults to a single partition.
#'
#' @return A list of payloads, each representing a partition.
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#'   payload_lists <- .partition_payload(timegpt_data, num_partitions)
#' }
#'
.partition_payload <- function(payload, num_partitions){

  unique_ids <- unique(sapply(payload$y$data, function(x) x$unique_id))

  num_parts <- as.numeric(future::availableCores())
  if(is.null(num_partitions)){
    num_parts <- 1
  }else if(num_partitions == "auto" || num_partitions >= as.numeric(future::availableCores())){
    num_parts <- as.numeric(future::availableCores())
  } # else use the number of partitions given by the user

  ids_per_set <- ceiling(length(unique_ids)/num_parts)
  split_unique_ids <- split(unique_ids, rep(1:num_parts, each = ids_per_set, length.out = length(unique_ids)))
  unique_ids_sets <- lapply(split_unique_ids, function(x) as.character(x))

  filter_by_unique_id <- function(original_list, unique_ids) {
    new_list <- original_list
    new_list$y$data <- Filter(function(x) x$unique_id %in% unique_ids, original_list$y$data)
    new_list$x$data <- Filter(function(x) x$unique_id %in% unique_ids, original_list$x$data)
    return(new_list)
  }

  payload_list <- lapply(unique_ids_sets, function(ids) filter_by_unique_id(payload, ids))

  return(payload_list)
}
