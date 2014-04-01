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

### Load packages
library(reshape2)
## library(plyr)
library(ggplot2)
library(survival)


### 
