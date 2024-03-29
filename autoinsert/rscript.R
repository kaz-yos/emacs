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

## Configure parallelization
## Parallel backend for foreach (also loads foreach and parallel; includes doMC)
library(doParallel)
## Reproducible parallelization
library(doRNG)
## Detect core count. This supports LSF and SLURM.
n_cores <- future::availableCores()
## Used by parallel::mclapply() as default
options(mc.cores = n_cores)
## Used by doParallel as default
options(cores = n_cores)
## Register doParallel as the parallel backend for foreach
## http://stackoverflow.com/questions/28989855/the-difference-between-domc-and-doparallel-in-r
doParallel::registerDoParallel(cores = n_cores)
## future: Unified Parallel and Distributed Processing in R for Everyone
## https://cran.r-project.org/web/packages/future/vignettes/future-1-overview.html
library(future)
future::plan(multicore)
## Report multicore use
foreach::getDoParName()
foreach::getDoParWorkers()
future::availableCores()

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
    "Used ", foreach::getDoParWorkers(), " cores\n",
    "Used ", foreach::getDoParName(), " as backend\n",
    sep = "")
