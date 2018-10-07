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
}

cat("
###
### Prepare environment
################################################################################\n")

## Record start time
start_time <- Sys.time()
cat("### Started ", as.character(start_time), "\n")

## Configure parallelization
## Parallel backend for foreach (also loads foreach and parallel; includes doMC)
library(doParallel)
## Reproducible parallelization
library(doRNG)
## Detect core count (Do not use on clusters)
n_cores <- parallel::detectCores()
## Used by parallel::mclapply() as default
options(mc.cores = n_cores)
## Used by doParallel as default
options(cores = n_cores)
## Register doParallel as the parallel backend for foreach
## http://stackoverflow.com/questions/28989855/the-difference-between-domc-and-doparallel-in-r
doParallel::registerDoParallel(cores = n_cores)
## Report multicore use
cat("### Using", foreach::getDoParWorkers(), "cores\n")
cat("### Using", foreach::getDoParName(), "as backend\n")

## https://github.com/tidyverse/tibble/issues/395
options(crayon.enabled = FALSE)
## Load packages
library(tidyverse)


cat("
###
### Load data
################################################################################\n")



################################################################################
cat("
###
### Record package versions etc
################################################################################\n")
print(sessionInfo())
## Record execution time
end_time <- Sys.time()
cat("\n### Started  ", as.character(start_time), "\n")
cat("### Finished ", as.character(end_time), "\n")
print(end_time - start_time)
