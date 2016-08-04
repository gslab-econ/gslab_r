
# Overview

The loadpaths pacakge provides two functions to load poaths as individual character vectors into an R session and one function to summarize pipe delimited .txt files. The functions in this package were originally created for use in the [politext project](https://github.com/TaddyLab/politext).


## Load paths
Use `loadPaths()` to load the path associated with any variabe returned from `Sys.getenv()` that begins with `PATH`. 

Use `loadPathsDirect()` to specify and load paths from specify .txt files. Files must be tab delimited and have two entries per row: 1) a key and 2) a path. Standard paths are preloaded, but they can be overridden and any arbitrary path may be specificed.

## Summarize

Use `checksum` to produce a "checksum" of all .txt files in the path specified by the environment variable `CHECKSUM_DIR`. The files must be pipe delimited and contain a header.