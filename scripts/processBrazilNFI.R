#### Load Libraries ####
library(here)
# library(Plot2Map)

#### Load file paths #####

# Load the 'config.RData' file from the 'src/config' directory
load(here("src", "config", "directoryConfig.RData"))

#### Note: comment out this section once Plot2Map has been installed properly ####
# Define the path to src functions
srcDir <- config$srcDir
dataDir <- config$dataDir

source(file.path(srcDir, "open_plot_data.R"))

plots <- open_plot_data(file.path(dataDir, "subplot_AGBD_Amazonia_2018_5000m2.csv"), dataType = 2)