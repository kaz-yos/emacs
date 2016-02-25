#!/usr/local/bin/Rscript

################################################################################
###
##
## Created on:
## Author: Kazuki Yoshida
################################################################################


### Prepare environment
################################################################################

## Configure sink()
if (sink.number() != 0) {sink()}
..scriptFileName.. <- gsub("^--file=", "", Filter(function(x) {grepl("^--file=", x)}, commandArgs()))
if (length(..scriptFileName..) == 1) {
    sink(file = paste0(..scriptFileName.., ".txt"), split = TRUE)
}
options(width = 120)

## Configure parallelization
## Parallel backend for foreach (also loads foreach and parallel; includes doMC)
library(doParallel)
## Reproducible parallelization
library(doRNG)
## Detect core count
nCores <- min(parallel::detectCores(), 10)
## Used by parallel::mclapply() as default
options(mc.cores = nCores)
## Used by doParallel as default
options(cores = nCores)
## Register doParallel as the parallel backend with foreach
## http://stackoverflow.com/questions/28989855/the-difference-between-domc-and-doparallel-in-r
doParallel::registerDoParallel(cores = nCores)
## Report multicore use
cat("### Using", foreach::getDoParWorkers(), "cores\n")
cat("### Using", foreach::getDoParName(), "as backend\n")

## Load packages
library(magrittr)
library(dplyr)
library(tidyr)


cat("
###
### Load data
################################################################################\n")




################################################################################
cat("\n### Record package versions\n")
print(sessionInfo())
## Stop sinking to a file if active
if (sink.number() != 0) {sink()}
