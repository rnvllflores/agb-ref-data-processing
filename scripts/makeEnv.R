## Packages installation
if (!require("pacman")) install.packages("pacman")
pacman::p_load(terra,plyr,dplyr,foreach,purrr,BIOMASS,data.table,ranger,randomForest,
               doParallel,plotrix,gfcanalysis,sf,stringr,Metrics, installr, rmarkdown, here, devtools)

# devtools::install_github("aTnT/Plot2Map")