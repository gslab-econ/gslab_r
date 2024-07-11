.onLoad <- function(libname, pkgname) {
  # Import data.table operators
  data.table::.datatable.aware <- TRUE
}
