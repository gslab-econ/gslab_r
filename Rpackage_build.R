Main <- function(){
    
    CRAN_packages   <- c("data.table", "devtools",   "foreign",   "ggplot2", 
                         "Matrix",     "parallel",   "plyr",      "readstata13",
                         "reshape2",   "roxygen2",   "RUnit",     "testthat")  
    
    CRAN_result     <- lapply(CRAN_packages, install_CRAN, repo = "http://cran.cnr.Berkeley.edu/", 
                              dependency = TRUE, quiet = TRUE)
    
    library(devtools)
    
    GitHub_packages <- c("loadpaths")
    
    GitHub_result   <- lapply(GitHub_packages, function(pkg) install_github('gslab_econ/gslab_r', subdir = pkg))
}

install_CRAN <- function(pkg, repo, dependency, quiet){
    if (system.file(package = pkg) == "") {
        install.packages(pkg, repos = repo, dependencies = dependency, quiet = quiet)
    }
}

Main()
