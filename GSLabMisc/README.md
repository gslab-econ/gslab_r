
# Overview

GSLabMisc() package provides some helper functions.


## AutoFill
Use `AutoFill()` to save TeX macros as described [here](https://github.com/gslab-econ/ra-manual/wiki/Autofilling-Values/4a41786eed04d627060f6c5de37f6fc304ef6025#in-text-values).

## LoadScalars
Use `LoadScalars()` to load values from a .txt file into the global environment. Each line of the input file is assumed to have the name of the global variable followed by a space followed by the value of the global variable. Lines beginning with # are treated as comments. Lines in the file that do not have at least two words (defined as blocks of text separated by a space) are ignored. Strings enclosed in double quotes count as one word.
