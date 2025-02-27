#### Load Libraries ####
library(here)
library(purrr)
# library(Plot2Map)

#### Load file paths #####

# Load the 'config.RData' file from the 'src/config' directory
load(here("src", "config", "directoryConfig.RData"))

#### Note: comment out this section once Plot2Map has been installed properly ####
# Define the path to src functions
srcDir <- config$srcDir

# Assuming 'srcDir' is already defined and contains the path to your src folder
# Define the list of function scripts
function_scripts <- c(
  "RawPlots.R",
  "Polygonize.R",
  "MeasurementErr.R",
  "Nested.R",
  "RefLidar.R"
)

# Source all the function files by appending the srcDir path to each script name
invisible(lapply(function_scripts, function(script) {
  # Combine the srcDir with the script name to get the full path
  source(file.path(srcDir, script))
}))


#### Open Plot Data Function ####

open_plot_data <- function(plotsFile, dataType, dataDir = NULL, plotsAGBFile = NULL, treeDataFile = NULL, treeXYFile = NULL, centroidFile = NULL, nestedTreeDataFile = NULL, slbAGBDir = NULL, SRS = NULL) {
  #' Open and process different types of plot data
  #'
  #' This function handles various types of plot data based on the `dataType` provided.
  #' It reads and processes data depending on the format, such as point data, polygon data, 
  #' tree-level data, LiDAR data, and shapefiles. Each data type requires different user inputs.
  #'
  #' @param plotsFile (Character) Path to the input file containing plot data (CSV or shapefile).
  #'        The format of this file depends on `dataType` (e.g., CSV for point data, polygon data, etc.).
  #' @param dataType (Numeric) An integer specifying the type of plot data:
  #'        1: Point data (CSV format)
  #'        2: Unformatted data (user needs to manually specify columns)
  #'        3: Polygon data (CSV with plot corner coordinates)
  #'        4: Tree-level measurement data (CSV)
  #'        5: Nested plots (CSV with sub-plot data)
  #'        6: LiDAR-based map data (requires LiDAR data directory)
  #'        7: Shapefile data (requires spatial data with coordinates)
  #' @param dataDir (Character, Optional) Directory path for additional data files (used for LiDAR data, etc.).
  #' @param plotsAGBFile (Character, Optional) Path to the file containing AGB (Above Ground Biomass) data, used with dataType 3.
  #' @param treeDataFile (Character, Optional) Path to the file containing tree-level measurement data, used with dataType 4.
  #' @param treeXYFile (Character, Optional) Path to the file containing XY coordinates of the tree measurements, used with dataType 4.
  #' @param centroidFile (Character, Optional) File name for centroid data, used with dataType 5 (nested plots).
  #' @param nestedTreeDataFile (Character, Optional) File name for tree data in nested plots, used with dataType 5.
  #' @param slbAGBDir (Character, Optional) Directory path for LiDAR-based AGB maps, used with dataType 6.
  #' @param SRS (Numeric, Optional) The Spatial Reference System code (e.g., 4326, 32622). 
  #'        If NULL, the user will be prompted for input during execution.
  #' 
  #' @return (Data frame or sf object) Processed plot data, returned in a format depending on the type of data.
  #'
  #' @details
  #' The function will handle the following data types:
  #' 1. **Point data**: Assumes CSV format with standard columns.
  #' 2. **Unformatted data**: Assumes CSV format but requires the user to specify which columns represent 
  #'    plot ID, coordinates, AGB, and size.
  #' 3. **Polygon data**: Assumes a CSV file containing plot corner coordinates, and optionally another CSV 
  #'    with AGB values. Transforms the data into a spatial object.
  #' 4. **Tree-level measurement data**: Assumes CSV data, and processes it with error corrections using a specific 
  #'    function based on the region.
  #' 5. **Nested plots**: Assumes data for sub-plots in CSV format. Combines the centroid data and tree data.
  #' 6. **LiDAR-based maps**: Processes LiDAR-derived AGB data for further analysis, based on a directory of LiDAR 
  #'    data.
  #' 7. **Shapefile data**: Reads a shapefile, processes spatial information, and returns it as a spatial data object.
  
  # If SRS is not provided, prompt the user to input it
  if (is.null(SRS)) {
    cat("Please enter the Spatial Reference System (SRS) code: ")
    SRS <- as.numeric(readLines(con = stdin(), n = 1))
  }
  
  # Check the type of the data and open it accordingly
  if (dataType == 1) {
    # Case 1: PLOT DATA IS POINT DATA AND FORMATTED (CSV)
    # Assumes a standard CSV file
    plots <- read.csv(plotsFile)
    
  } else if (dataType == 2) {
    # Case 2: PLOT DATA IS UNFORMATTED (OWN FORMAT)
    # User must specify column indices manually
    plots <- RawPlots(read.csv(plotsFile))
    
  } else if (dataType == 3) {
    # Case 3: PLOT DATA IS A POLYGON WITH PLOT CORNER COORDINATES (CSV)
    plotsPoly <- read.csv(plotsFile) 
    # If there's a separate file for AGB, use the variable plotsAGBFile
    plotsPolyAGB <- read.csv(plotsAGBFile)
    plots <- Polygonize(plotsPoly, SRS)
    plots$AGB_T_HA <- plotsPolyAGB$AGB_T_HA
    plots$AVG_YEAR <- plotsPolyAGB$AVG_YEAR
    
  } else if (dataType == 4) {
    # Case 4: PLOT DATA WITH TREE-LEVEL MEASUREMENT (CSV)
    # Use the treeDataFile and treeXYFile provided as arguments
    plotTree <- read.csv(treeDataFile)
    xyTree <- read.csv(treeXYFile)
    plots <- MeasurementErr(plotTree, xyTree, 'Asia')
    
  } else if (dataType == 5) {
    # Case 5: PLOT DATA WITH TREE-LEVEL MEASUREMENT AND NESTED PLOTS (Sub-plots)
    # Use centroidFile and nestedTreeDataFile as inputs
    cent <- readOGR(dsn = dataDir, layer = centroidFile)
    tree <- read.csv(nestedTreeDataFile)
    TreeData <- Nested(cent, tree)
    plotTree <- TreeData[[1]]
    xyTree <- TreeData[[2]]
    plots <- MeasurementErr(plotTree, xyTree, 'Europe')
    
  } else if (dataType == 6) {
    # Case 6: REFERENCE DATA IS A LIDAR-BASED MAP
    # Use slbAGBDir variable for the path to LiDAR-based maps directory
    slb.cv.dir <- paste0(slbAGBDir, "/SLB_CVmaps")
    slb.cv <- RefLidar(slbAGBDir) 
    plots <- RefLidar(slbAGBDir)
    plots$sdTree <- slb.cv$CV * plots$AGB_T_HA
    
  } else if (dataType == 7) {
    # Case 7: PLOT DATA IS A SHAPEFILE (Spatial Data)
    # Use the plotsFile path and handle it using the st_read function
    plots_sf <- st_read(plotsFile)
    plots_sf <- st_transform(plots_sf, crs = 4326)
    plots_sf$longitude <- st_coordinates(st_centroid(plots_sf))[, 1]
    plots_sf$latitude <- st_coordinates(st_centroid(plots_sf))[, 2]
    plots <- RawPlots(plots_sf)  # Users should know year, plot size, inventory year beforehand
    
  } else {
    stop("Invalid dataType specified. Please provide a valid data type between 1 and 7.")
  }
  
  return(plots)
}
