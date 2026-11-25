INT_ARGS <- c("h", "n_windows", "step_size", "finetune_steps", "finetune_depth",
              "num_partitions", "detection_size", "n_paths", "seed", "job_timeout_seconds", "freq")
LIST_ARGS <- c("level", "quantiles", "hist_exog_list", "categorical_exog_list",
               "features", "date_features", "date_features_to_one_hot")

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
  df <- as.data.frame(df)
  zones <- list()
  # pandas needs datetime64; R Dates would arrive as python objects
  for (n in names(df)) {
    x <- df[[n]]
    # tsibble period indexes would reach pandas as float64
    if (inherits(x, c("yearmonth", "yearquarter", "yearweek"))) x <- as.Date(x)
    if (inherits(x, "Date")) x <- as.POSIXct(x, tz = "UTC")
    # Date-like (all midnight) local times keep wall-clock: converting instants to UTC makes
    # local days 23h/25h across DST. Sub-daily times keep the instant: wall-clock would
    # duplicate/skip hours at DST changes. The zone forced is recorded for .call_client.
    else if (inherits(x, "POSIXct") && !identical(attr(x, "tzone"), "UTC")) {
      if (all(format(x, "%H:%M:%S") == "00:00:00", na.rm = TRUE)) {
        z <- attr(x, "tzone")[1]
        zones[[n]] <- if (is.null(z) || is.na(z) || !nzchar(z)) Sys.timezone() else z
        x <- lubridate::force_tz(x, "UTC")
      } else x <- lubridate::with_tz(x, "UTC")
    }
    df[[n]] <- x
  }
  attr(df, "forced_tz") <- zones
  df
}

.whole_to_int <- function(v) {
  if (is.double(v) && isTRUE(all(v == trunc(v) & abs(v) <= .Machine$integer.max))) as.integer(v) else v
}

.coerce_args <- function(method, args) {
  args <- Filter(Negate(is.null), args)
  for (n in intersect(names(args), INT_ARGS)) {
    args[[n]] <- .whole_to_int(args[[n]])
  }
  if ("level" %in% names(args)) args$level <- .whole_to_int(args$level)
  for (n in intersect(names(args), LIST_ARGS)) {
    if (n == "level" && method == "detect_anomalies_online") next
    # date_features may be TRUE/FALSE: only wrap character vectors
    if (n %in% c("date_features", "date_features_to_one_hot") && !is.character(args[[n]])) next
    args[[n]] <- as.list(args[[n]])
  }
  args
}

.to_r <- function(x) {
  r <- reticulate::py_to_r(x)
  if (is.data.frame(r)) {
    attr(r, "pandas.index") <- NULL
    # naive pandas datetimes arrive without a tzone and would print in local time
    for (n in names(r)) {
      if (inherits(r[[n]], "POSIXct") && !nzchar(paste0(attr(r[[n]], "tzone"), ""))) attr(r[[n]], "tzone") <- "UTC"
    }
  }
  r
}

.call_client <- function(.method, ...) {
  args <- list(...)
  for (n in intersect(names(args), c("df", "X_df"))) args[[n]] <- .prep_df(args[[n]])
  # zone of the time column if .prep_df forced it; results come back UTC otherwise
  # (sub-daily instants stay in UTC)
  tc <- if (is.null(args$time_col)) "ds" else args$time_col
  tz <- attr(args$df, "forced_tz")[[tc]]
  for (n in intersect(names(args), c("df", "X_df"))) attr(args[[n]], "forced_tz") <- NULL
  drop_id <- "id_col" %in% names(args) && is.null(args$id_col)
  if (drop_id) {
    for (n in intersect(names(args), c("df", "X_df"))) {
      if (!is.null(args[[n]])) args[[n]] <- cbind(unique_id = "ts_0", args[[n]])
    }
    args$id_col <- "unique_id"
  }
  call <- function() do.call(.get_client()[[.method]], .coerce_args(.method, args))
  if (.method == "detect_anomalies_online") {
    # Python 0.9.0 emits a rename FutureWarning on every call; hide only that one
    w <- reticulate::import("warnings")
    res <- .to_r(with(w$catch_warnings(), {
      w$filterwarnings("ignore", message = "detect_anomalies_online", category = reticulate::import_builtins()$FutureWarning)
      call()
    }))
  } else {
    res <- .to_r(call())
  }
  if (drop_id && is.data.frame(res)) res$unique_id <- NULL
  if (!is.null(tz) && is.data.frame(res) && inherits(res[[tc]], "POSIXct")) {
    res[[tc]] <- lubridate::force_tz(res[[tc]], tz)
  }
  res
}
