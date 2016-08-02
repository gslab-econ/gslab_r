# Overview

This repository contains the R tools used in multiple projects/repositories by GSLab. The code in this repository is originally drawn from `trunk/lib/third_party/r_tools` of the SVN repository `econ-gentzkow-stanford, revision 34,755`. However, new packages have been added as needed. 

## Third party packages

All packages in `third_party` are available on CRAN. By default, all third party packages should be download from CRAN or another stable source. If no stable source is available, then the contents of the package should be transfered into a downloadable format and stored in gslab_r. For completeness all thrid party packages should be given a directory within `third_party` for easy scanning.

## Internal packages

All R packages produced by GSLab should be stored as directories in `gslab_r` named after the title of the package. An exact match between the name and title is preffered. Internal packages should be loaded with `install_github` from the third party devtools package. 
