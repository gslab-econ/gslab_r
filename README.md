# Overview

This repository is used to store R packages created by the GSLab for use in two or more projects/repositories. If a stable release of a package is available from another source, then it should not be stored in this repository. Since different useres will draw on these packages across multiple projects, we follow a standard strategy for development, testing, and documentation. 

Useful tools on package development are given in the table below.

| Resource | Description |
| -------- | ----------- |
| [O'Reily R Packages](http://r-pkgs.had.co.nz/) | A detailed guide of the package developmetn process |
| [Hilary Parker](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/) | Barebones guide to package development with `devtools` |
| [`devtools` on GitHub](https://github.com/hadley/devtools) | Easy package creation functions |
| [`roxygen2` vignette](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html) | Methods for package documentation |
| [`testthat` tutorial](https://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf) | Contemporary unit testing in R |
| [`ggplot2` on GitHub](https://github.com/hadley/ggplot2) | Example of a large scale, well-used, and reliable R package |

##  Anatomy of a GSLab R package

Every R package produced by GSLab has the following structure. 

The DESCRIPTION file (no extension) should contain 



## Internal packages

All R packages produced by GSLab should be stored as directories in `gslab_r` named after the title of the package. An exact match between the name and title is preffered. Internal packages should be loaded with `install_github` from the third party devtools package. 

## Making and maintaining packages

Organization

Each package in the `gslab_r` repository should be given a unique directory. The directory name should exactly match the name of the package. We maintain a single format of our R packages to ensure readability and completeness. Each package should always contain the following files and subdirectories. 

*  The DESCRIPTION file gives key information about the package: its name, title, author, version, description, and dependencies. R will only recognize a directory as a package if it contains a DIRECTORY file.

*  The R subdirectory contains the .R files in the package. These files should meet the standards in the [RA manual](https://github.com/gslab-econ/admin/wiki/Code-Style). Each file should conatin exactly one well-documented function. Dependencies are allowed between files within the R subdirectory. For example 

### Semantic versioning

Use [semantic versioning](http://semver.org/)

### Branching

Our package system must permit calls for a stable download while a package is in development. We meet this standard using branches. Each package has two branches: a stable branch for download and a development branch for updates and experimentation. 

The stable branch should be named 
