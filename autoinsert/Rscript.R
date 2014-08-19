#!/usr/bin/Rscript

################################################################################
### 
## 
## Created on: 
## Author: Kazuki Yoshida
################################################################################


### Prepare environment
################################################################################

## Configure parallelization
library(doMC)           # Parallel backend to foreach (used in plyr)
registerDoMC()          # Turn on multicore processing
options(cores = 4)
options(mc.cores = 4)

## Load packages
library(reshape2)
library(magrittr)
library(dplyr)
library(ggplot2)
library(survival)

## Configure sink()
if (sink.number() != 0) {
    sink()
}
..scriptFileName.. <- gsub("^--file=", "", Filter(function(x) {grepl("^--file=", x)}, commandArgs()))
if (length(..scriptFileName..) == 1) {
    sink(file = paste0(..scriptFileName.., ".txt"), split = TRUE)
}


### Load data
################################################################################



################################################################################
## Stop sinking to a file if active
sink()
