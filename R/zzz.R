nixtla_py <- NULL

.onLoad <- function(libname, pkgname) {
  reticulate::py_require("nixtla==0.9.0")
  nixtla_py <<- reticulate::import("nixtla", delay_load = TRUE)
}
