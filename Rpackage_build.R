Main <- function(){
  
  CRAN_packages   <- c("bit64",       "bootstrap",   "data.table",  "DBI",      "devtools", 
                       "entropy",     "foreign",     "gamlr",       "gamlss",   "ggplot2", 
                       "glmnet",      "lattice",     "Matrix",      "parallel", "pls",         
                       "plyr",        "R.methodsS3", "R.oo",        "R.utils",  "RSQLite",  
                       "readstata13", "reshape",     "reshape2",    "roxygen2", "rpart.plot",
                       "RUnit",       "slam",        "snow",        "snowfall", "stringr")
  
  CRAN_result     <- lapply(CRAN_packages, install_CRAN, repo = "http://cran.cnr.Berkeley.edu/", 
                            dependency = TRUE, quiet = TRUE)
  
  GitHub_packages <- c("gslab-econ/gslab_r/loadpaths")
  
  GitHub_result   <- lapply(GitHub_packages, install_GitHub, quiet = TRUE)
}

install_CRAN <- function(pkg, repo, dependency, quiet){
  if (system.file(package = pkg) == "") {
    install.packages(pkg, repos = repo, dependencies = dependency, quiet = quiet)
  }
}

install_GitHub <- function(pkg, quiet) {
  #package name is the part of the string after the final "/" in pkg
  pkg_loc  <- nchar(pkg) - regexpr("\\/[^\\/]*$", pkg)
  pkg_name <- substr(pkg, (nchar(pkg) + 1) - pkg_loc, nchar(pkg))
  
  if (system.file(package = pkg_name) == "") {
    install.packages(pkg, quiet = quiet)
  }
}

Main()