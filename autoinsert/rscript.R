#!/usr/bin/env Rscriptee
## Rscriptee: Rscript $@ 2>&1 | tee ${OUTPUT_FILE}

################################################################################
###
##
## Created on:
## Author: Kazuki Yoshida
################################################################################

## When running non-interactively
.script_name. <- gsub("^--file=", "", Filter(function(x) {grepl("^--file=", x)}, commandArgs()))
if (length(.script_name.) == 1) {
    cat("### Running:", paste(commandArgs()), "\n")
    options(width = 100)
    options(echo = TRUE)
}

###
### Prepare environment
################################################################################

## Record start time
start_time <- Sys.time()
cat("### Started ", as.character(start_time), "\n")

## Load packages
library(tidyverse)


###
### Load data
################################################################################



################################################################################
###
### Record package versions etc
################################################################################
print(sessionInfo())
## Record execution time and multicore use
end_time <- Sys.time()
diff_time <- difftime(end_time, start_time, units = "auto")
cat("Started  ", as.character(start_time), "\n",
    "Finished ", as.character(end_time), "\n",
    "Time difference of ", diff_time, " ", attr(diff_time, "units"), "\n",
    sep = "")
