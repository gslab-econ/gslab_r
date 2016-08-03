# Overview

This repository contains the R tools used in multiple projects/repositories by GSLab. The code in this repository is originally drawn from `trunk/lib/third_party/r_tools` of the SVN repository `econ-gentzkow-stanford, revision 34,755`. However, new packages have been added as needed. 

## Third party packages

All packages in `third_party` are available on CRAN. By default, all third party packages should be download from CRAN or another stable source. If no stable source is available, then the contents of the package should be transfered into a downloadable format and stored in gslab_r. For completeness all thrid party packages should be given a directory within `third_party` for easy scanning.

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
