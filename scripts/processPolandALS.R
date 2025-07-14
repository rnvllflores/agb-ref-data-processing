#### Load Libraries ####
library(here)
library(sf)
library(ranger)
library(dplyr)
# library(lubridate)
# library(Plot2Map)

#### Load file paths #####

# Load the 'config.RData' file from the 'src/config' directory
load(here("src", "config", "directoryConfig.RData"))

#### Note: comment out this section once Plot2Map has been installed properly ####
# Define the path to src functions
srcDir <- config$srcDir
dataDir <- config$dataDir
outDir <- config$outputDir

#### Set variables####
datasetName <- "poland_agb_als"
map_year = 2015
mapRsl <- 100 
dataVersion<- 7
remove <- c('FEZ', 'FAO.ecozone',  "RS_HA","ratio" )
sdMapVal <- "NA"  #######should have a value to be consistent with PVIR1-2 e.g. @1km SD layers extract

#### Source custom Functions ####
# source(file.path(srcDir, "open_plot_data.R"))
source(file.path(srcDir, "RefLidar.R"))
source(file.path(srcDir, "Deforested.R"))
source(file.path(srcDir, "BiomePair.R"))
source(file.path(srcDir, "TempFix.R"))
source(file.path(srcDir, "TempVis.R"))
source(file.path(srcDir, "AddBiorealm.R"))

#-------------------------------- Open Dataset --------------------------------
fname <- file.path(dataDir,paste0("output/",datasetName,"_formatted.csv"))
if (file.exists(fname)) {
  plots <- read.csv(fname)
  print("Previously processed data loaded")
} else{
  slb.cv <- RefLidar(file.path(dataDir,"for_processing/BULGARIA_data_package_v01/Cv")) 
  plots <- RefLidar(file.path(dataDir,"for_processing/BULGARIA_data_package_v01/AGB")) 
  plots$sdTree <- slb.cv$CV * plots$AGB
  
  write.csv(plots, fname, row.names = FALSE)
}

# checkpoint data
checkpoint_path <- file.path(dataDir,paste0("output/checkpoint/",datasetName,"_deforested.Rdata"))

if (file.exists(checkpoint_path)) {
  # If the file exists, load the .RData file
  load(checkpoint_path)  # This will load all objects saved in the .RData file
  print("Checkpoint file loaded.")
} else {
  # If the file does not exist, run the functions
  plots1 <- Deforested(plots, map_year = map_year, gfc_folder = dataDir)  #use 2015 as midpoint, later on agbref script has temporal script
  plots2 <- BiomePair(plots)
  
  # Save the result as a checkpoint .RData file
  save(plots1, plots2, file = checkpoint_path)  # Save multiple objects if needed
  print("New plots generated and saved.")
}

## ------------------ Measurement error (ONLY for plot data cases #1-3) --------------------------


## Using a pre-trained RF model for plot-level data 
load(file.path(dataDir, "rf1.RData")) #pre-trained RF model from 10000+ plots across biomes 
plotsPred <- plots2[,c('AGB','SIZE_HA', 'GEZ')]
names(plotsPred) <- c('agb', 'size', 'gez')
plotsPred$size <- as.numeric(plotsPred$size) * 10000 #convert size to m2
plotsPred$gez = factor(plotsPred$gez,levels = c("Boreal","Subtropical","Temperate","Tropical"))
plots2$sdTree <- predict(rf1, plotsPred)[[1]]



## ------------------ Temporal adjustment ------------------------------------------------------

## Apply growth data to whole plot data by identifying AGB map year with associated SD based on
## growth data from IPCC 2019

gez <- sort(as.vector((raster::unique(plots2$GEZ)))) 
plots2 <- plots2 %>%
  rename('AGB_T_HA' = 'AGB')
plots3 <- TempApply(plots2,map_year =  map_year)
plots.tf <- TempVar(plots3, map_year = map_year)
plots.tf$sdGrowth <- ifelse(is.nan(plots.tf$sdGrowth), mean(plots.tf$sdGrowth,na.rm=T),plots.tf$sdGrowth)

## Histogram and summary table: before and after temporal adjustment

HistoTemp(plots.tf, map_year, outDir=outDir)
HistoShift(plots.tf, map_year, outDir = outDir)


## ------------------ Sampling error -----------------------------------------------------------

## Estimates SD of pixel and plot sizes differences from geo-simulations of Rejou-Mechain et al. 2014 study

plots.tf$RS_HA <- mapRsl^2 / 10000 
plots.tf$ratio <-  as.numeric(plots.tf$SIZE_HA) / plots.tf$RS_HA
se <- read.csv(file.path(dataDir, 'se.csv'))
rfSE <- ranger(se$cv ~ ., data=se[,c('SIZE_HA','RS_HA','ratio')])

plots.tf <- plots.tf %>%
  rename('AGB' = 'AGB_T_HA')

plots.tf$sdSE <-  (predict(rfSE, plots.tf[,c('SIZE_HA', 'RS_HA', 'ratio')])[[1]] / 100) * mean(plots.tf$AGB, na.rm=T)



## ------------------ Plot uncertainty total ---------------------------------------------------

plots.tf$varPlot <- plots.tf$sdTree^2 + plots.tf$sdSE^2 +plots.tf$sdGrowth^2 



## ------------------ Export validation (and calibration)-ready data ---------------------------

# write.csv(plots.tf, file.path(outDir,paste0(datasetName,"_",map_year,"_validation_data.csv")), row.names=FALSE)

##------------------- Add version, sdMap and drop columns --------------------------------------
plots.tf$VER <- dataVersion
plots.tf <- plots.tf[ , !(names(plots.tf) %in% remove)]
plots.tf$sdMap <- sdMapVal

#-------------------- Add biorealm -------------------------------------------------------------
plots_bio <- add_bio_realms(plots.tf)

#--------------------- Export output -----------------------------------------------------------
write.csv(plots_bio, file.path(outDir,paste0(datasetName,"_",map_year,"_validation_data.csv")), row.names=FALSE)