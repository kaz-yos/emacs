################################################################################
### 
## 
## Created on: 
## Author: Kazuki Yoshida
################################################################################


### Prepare environment
################################################################################

### Load packages
## Load doMC
library(doMC)           # Parallel backend to foreach/plyr
registerDoMC()          # Turn on multicore processing
## Configuration for parallel package
options(cores = 4)
options(mc.cores = 4)


