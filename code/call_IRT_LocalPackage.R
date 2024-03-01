## This script navigates to the local location of the
## IRT Sampler and loads the package

## Separate script b/c location might change
## (And eventually just want to load IRTM from CRAN)

setwd("~/Dropbox/Research/Interventions/Theory-IRT/") ## WD for package install

library(devtools)

install.packages('IRTM',
                 repos=NULL,
                 type='source')

library(IRTM)

source("./IRTM-local/R/anchors.R")

## Direct to diagnostic script:
diagPath <- "~/Dropbox/interventions_irt/"
source(paste0(diagPath, "diagnostics.R"))
