INT_ARGS <- c("h", "n_windows", "step_size", "finetune_steps", "finetune_depth",
              "num_partitions", "detection_size", "n_paths", "seed")
LIST_ARGS <- c("level", "quantiles", "hist_exog_list", "categorical_exog_list",
               "features")

# Env var first, then R option; "" counts as unset.
.setting <- function(name) {
  v <- Sys.getenv(name, unset = "")
  if (nzchar(v)) return(v)
  o <- getOption(name)
  if (is.null(o) || !nzchar(o)) NULL else o
}

.client_config <- function() {
  api_key <- .setting("NIXTLA_API_KEY")
  if (is.null(api_key)) {
    stop("Please set your NIXTLA_API_KEY. Use nixtla_client_setup() or set it as an environment variable in .Renviron",
         call. = FALSE)
  }
  list(api_key = api_key, base_url = .setting("NIXTLA_BASE_URL"))
}

.get_client <- function() {
  cfg <- .client_config()
  do.call(nixtla_py$NixtlaClient, Filter(Negate(is.null), cfg))
}

.prep_df <- function(df) {
  if (is.null(df)) return(NULL)
  if (!is.data.frame(df)) {
    stop("Only data frames, tibbles, and tsibbles are allowed.", call. = FALSE)
  }
  as.data.frame(df)
}

.coerce_args <- function(method, args) {
  args <- Filter(Negate(is.null), args)
  for (n in intersect(names(args), INT_ARGS)) {
    v <- args[[n]]
    if (is.double(v) && isTRUE(all(v == trunc(v)))) args[[n]] <- as.integer(v)
  }
  for (n in intersect(names(args), LIST_ARGS)) {
    if (n == "level" && method == "detect_anomalies_online") next
    args[[n]] <- as.list(args[[n]])
  }
  args
}

.to_r <- function(x) {
  r <- reticulate::py_to_r(x)
  if (is.data.frame(r)) attr(r, "pandas.index") <- NULL
  r
}

.call_client <- function(method, ...) {
  args <- list(...)
  for (n in intersect(names(args), c("df", "X_df"))) args[[n]] <- .prep_df(args[[n]])
  drop_id <- "id_col" %in% names(args) && is.null(args$id_col)
  if (drop_id) {
    for (n in intersect(names(args), c("df", "X_df"))) {
      if (!is.null(args[[n]])) args[[n]] <- cbind(unique_id = "ts_0", args[[n]])
    }
    args$id_col <- "unique_id"
  }
  res <- .to_r(do.call(.get_client()[[method]], .coerce_args(method, args)))
  if (drop_id && is.data.frame(res)) res$unique_id <- NULL
  res
}
