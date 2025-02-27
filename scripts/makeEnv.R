## Packages installation
if (!require("pacman")) install.packages("pacman")
pacman::p_load(terra,plyr,dplyr,foreach,purrr,BIOMASS,data.table,ranger,randomForest,
               doParallel,plotrix,gfcanalysis,sf,stringr,Metrics, installr, rmarkdown, here, devtools, lubridate,lwgeom)

# List of required packages
required_packages <- c(
  "doParallel", "dplyr", "earthdatalogin", "foreach", "gfcanalysis",
  "httr", "httr2", "parallel", "pbapply", "rvest", "sf", "stringr", "terra",
  "BIOMASS", "grDevices", "graphics", "stats", "utils"
)

# Function to check and install missing packages
install_if_missing <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if (length(missing_packages) > 0) {
    install.packages(missing_packages, dependencies = TRUE)
  }
}

# Install missing packages
install_if_missing(required_packages)

devtools::install_github("aTnT/Plot2Map")

# Create data dir