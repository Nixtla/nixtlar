nixtla_py <- NULL

.nixtla_missing <- function(e = NULL) {
  stop("nixtlar needs the Python package 'nixtla==0.9.0' in the active Python, ",
       "but it could not be imported. Restart R without forcing another Python, ",
       "or install it with reticulate::py_install(\"nixtla==0.9.0\").",
       if (!is.null(e)) paste0("\nPython error: ", conditionMessage(e)),
       call. = FALSE)
}

.onLoad <- function(libname, pkgname) {
  reticulate::py_require("nixtla==0.9.0")
  # delay_load imports immediately when Python is already initialized and then
  # on_error is not used, so also guard the import to keep the package loadable
  tryCatch(
    nixtla_py <<- reticulate::import("nixtla", delay_load = list(on_error = .nixtla_missing)),
    error = function(e) {
      ns <- topenv()
      rm("nixtla_py", envir = ns)
      makeActiveBinding("nixtla_py", function() .nixtla_missing(e), ns)
    })
}
