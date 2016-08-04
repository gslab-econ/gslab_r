Main <- function(){
    
    CRAN_packages   <- c("data.table", "devtools",   "foreign",   "ggplot2", 
                         "Matrix",     "parallel",   "plyr",      "readstata13",
                         "reshape2",   "roxygen2",   "RUnit",     "testthat")  
    
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
    pkg_loc  <- nchar(pkg) - regexpr("\\/[^\\/]*$", pkg)
    pkg_name <- substr(pkg, (nchar(pkg) + 1) - pkg_loc, nchar(pkg))
    
    if (system.file(package = pkg_name) == "") {
        install.packages(pkg, quiet = quiet)
    }
}

Main()
