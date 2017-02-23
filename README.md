# Overview

This repository is used to store R packages created by the GSLab for use in two or more projects/repositories. If a stable release of a package is available from another source, then it should not be stored in this repository. Since different useres will draw on these packages across multiple projects, we follow a standard strategy for development, testing, and documentation. 

Useful tools on package development are given in the table below.

| Resource | Description |
| -------- | ----------- |
| [O'Reily R Packages](http://r-pkgs.had.co.nz/) | A detailed guide of the package development process |
| [Hilary Parker](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/) | Barebones guide to package development with `devtools` |
| [`devtools` on GitHub](https://github.com/hadley/devtools) | Easy package creation functions |
| [`roxygen2` vignette](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html) | Methods for package documentation |
| [`testthat` tutorial](https://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf) | Contemporary unit testing in R |
| [`ggplot2` on GitHub](https://github.com/hadley/ggplot2) | Example of a large scale, well-designed, and reliable R package |

## Development Process

We follow the development process for code libraries outlined in the [RA manual](https://github.com/gslab-econ/admin/wiki/Version-Control) We store each package in its own directory within `gslab_r`. The default name scheme is to give the package directory the same name as the package. 

### Tools

We use the `devtools`, `roxygen2`, and `testthat` packages as our primary tools in the package development process. `devtools` is a convenient wrapper for the functions in the others. The commands below are from`devtools`, but the syntax belongs to  `roxygen2` and `testthat`.

The `create` function initializes a package with the `DESCRIPTION`, `R` and `NAMESPACE` fields. The `LICENSE` file must be created manually, the `DESCRIPTION` file must be edited manually.

The `document` function should be used to produce the help files in the `man` subdirectory and appropriately fill the the `NAMESPACE` file. This function is a wrapper for the `roxygenise` function from the `roxygen2` package. This functions read from `DESCRIPTION` and the documentation for the files in `R`. Formatting must align with `roxygen2` standards.

During development the `devtools::check` function should be used to preform the unit tests in the `tests` subdirectory and check a large number of other package aspects. This function centralies and adds utilities to functions in the `testthat` package. The `tests` directory must be conform to the format used by `testthat`. 

To install a package from GitHub, use the `install_github` function.

### Workflow

The workflow for the creation of a package might be:
  1. Create a new branch of `gslab_r` named `development-packagename` from the`master` branch.
  2. Open R, load `devtools`, and run `create("packagename")` from the root of `gslab_r`.
  3. Write the functions, in-file documentation, and unit tests for the package.
    *  From the head of the directory, run `document` in R to add and update documentation as needed. 
    *  From the head of the directory, run `devtools::check` in R to perform unit tests as needed.
  4. When development is complete follow GSLab procedure to merge the `development-packagename` branch into `master`.

The workflow to add a feature to a package that already exists might be:
  1. Create a new branch of `gslab_r` named `development-packagename-issue` from the `development-packagename` branch.
  3. Write the new functions, in-file documentation, and unit tests for the package.
    *  From the head of the directory in R, run `document` to add and update documentation as needed. 
    *  From the head of the directory in R, run `devtools::check` to perform unit tests as needed.
  4. When development is complete follow GSLab procedure to merge the `development-packagename-issue` branch into `development-packagename`.

##  Anatomy of a GSLab R package

Every package contains the following structure.

### DESCRIPTION

The `DESCRIPTION` file (no extension) is required for R to reckognize a directory as a package. This file should contain complete information on the following fields. 

*  The `Package` section should contain the name of the package. 

*  The `Title` should give a descriptive package title.

*  `Version` gives the current version of the package. We use [semantic versioning](http://semver.org/).

*  `Authors@R` references Matt and Jesse as authors and Matt as the creator.

*  `Description` is a paragraph explanation of the function.

*  `Depends` gives package dependencies. Normally, this will only contain a version of R.

*  `Imports` lists the packages that will be installed along with this pacakge. Imports provides a robust method of tracking dependencies between packages.

*  `Suggests` contains packages that may be useful but are not strictly necessary to use the pacakge. Packages used for development and testing belong in this field.

*  `License` by defauly contains a reference to a separate file LICENSE file.

### LICENSE

By default, the `LICENSE` file (no extension) should contain the standard [MIT Open Source License](https://opensource.org/licenses/MIT). 

### R 

The `R` subdirectory stores R files that contain the functions in the package. Each script should include exactly one function. Arbitrary dependencies are allowed between the function in this package. Documentation should follow documentation guidelines(#function-documentation)

### NAMESPACE

The `NAMESPACE` file (no extension) informs R which functions from the `R` subdirectory to load for use and which other packages to install.

### man

The `man` subdirectory contains help files stored in .Rd format. Each help file should closely match the documentation of the functions in R.

### tests

The `tests` subdirectory contains the unit tests. We follow GSLab guidelines for [unit testing](https://github.com/gslab-econ/admin/wiki/Unit-Testing). 

### README.md

The `README.md` should give a brief description of the user called functions in the package. 

## Third Party Pacakges

We maintain an R scipt in the root of this driectory named `Rpackage_build.R`. This script contains a list of all third party packages used across multiple repositories and code to install them, if necessary. The [shell script](https://github.com/gslab-econ/admin/tree/master/computer-build-sheet) GSLab uses to set up machins calls `Rpackage_build.R`.

