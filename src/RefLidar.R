## Updates made to the new framework:
# 21/01/25:
# Replacing raster with terra package
# Improved method to calculate cells size in ha (reduces up to 10% bias in tropics)
# Removed unused year argument



#' Process LiDAR-based Reference Data
#'
#' This function processes LiDAR-based reference data from raster files, converting them to a standardized point data format.
#'
#' @param lidar.dir Character string specifying the directory containing LiDAR raster files.
#'
#' @return A data frame containing processed point data with columns varying based on the raster type:
#'   \item{PLOT_ID}{Unique identifier for each plot}
#'   \item{POINT_X}{Longitude coordinate}
#'   \item{POINT_Y}{Latitude coordinate}
#'   \item{AGB/CV/SD}{Above Ground Biomass, Coefficient of Variation, or Standard Deviation (depending on raster type)}
#'   \item{AVG_YEAR}{Year of data collection}
#'
#' @details
#' The function performs the following steps:
#' 1. Loads raster files from the specified directory
#' 2. Reprojects rasters to WGS84 if necessary
#' 3. Converts raster data to points
#' 4. Prompts user for information to extract PLOT_ID and AVG_YEAR from file names
#' 5. Formats output based on the specified raster type (AGB, CV, or SD)
#'
#' @examples
#' \dontrun{
#' lidar_data <- RefLidar(lidar.dir = "data/SustainableLandscapeBrazil_v04/SLB_AGBmaps")
#' }
#' @import terra
#' @importFrom utils menu
#'
#' @export
RefLidar <- function(lidar.dir) {

  #newproj <- "+proj=longlat +datum=WGS84"
  newproj <- "EPSG:4326"  # WGS84

  raw <- list.files(lidar.dir, full.names = TRUE)

  # Prompt for raster type
  raster_type <- readline(prompt = "Enter raster type (AGB, CV, or SD): ")
  raster_type <- toupper(raster_type)  # Ensure case consistency

  if (!raster_type %in% c("AGB", "CV", "SD")) {
    stop("Invalid raster type. Please enter 'AGB', 'CV', or 'SD'.")
  }

  # Load raster files
  r.files <- lapply(raw, function(x) {
    # r <- raster(x)
    # names(r) <- "value"  # Standardize the column name for raster data
    # r
    r <- terra::rast(x)
    names(r) <- "value"  # Standardize the column name for raster data
    r
  })

  # Reproject to WGS84 if needed
  # if (any(sapply(r.files, function(x) grepl('utm|meters|metre|UTM|zone|NAD', crs(x), ignore.case = TRUE)))) {
  #   r.files <- lapply(r.files, function(x) projectRaster(x, crs = newproj, method = 'bilinear'))
  # }
  if (any(sapply(r.files, function(x) grepl('utm|meters|metre|UTM|zone|NAD', terra::crs(x), ignore.case = TRUE)))) {
    r.files <- lapply(r.files, function(x) terra::project(x, newproj, method = 'bilinear'))
  }

  #ha <- xres(r.files[[1]]) * 1000
  #ha <- terra::xres(r.files[[1]]) * 1000
  ha <- mean(terra::values(terra::cellSize(r.files[[1]]) / 10000), na.rm = TRUE)


  # Convert pixels to points
  pts.list <- lapply(r.files, function(x) {
    #df <- as.data.frame(rasterToPoints(x))  # Convert raster to data frame
    df <- terra::as.data.frame(x, xy = TRUE)  # Convert raster to data frame with coordinates
    colnames(df) <- c("POINT_X", "POINT_Y", raster_type)  # Rename columns
    df
  })

  # Add ID for each raster file
  pts.list <- lapply(seq_along(pts.list), function(i) {
    pts <- pts.list[[i]]
    pts$ID <- basename(raw[i])  # Add file name as ID
    pts
  })

  # Combine all points into a single data frame
  pts <- do.call(rbind, pts.list)

  # Manual entry for PLOT_ID and AVG_YEAR
  message("User input needed to extract PLOT_ID from filename(s)...")
  message(paste0("File(s) used to extract data: ",  unique(pts$ID)))
  plot_start <- as.integer(readline(prompt = "Enter numeric index of the first letter of PLOT_ID: "))
  plot_end <- as.integer(readline(prompt = "Enter numeric index of the last letter of PLOT_ID: "))
  pts$PLOT_ID <- substr(pts$ID, plot_start, plot_end)

  message("User input needed to extract YEAR from filename(s)...")
  message(paste0("File(s) used to extract data: ",  unique(pts$ID)))
  year_start <- as.integer(readline(prompt = "Enter numeric index of the first letter of YEAR: "))
  year_end <- as.integer(readline(prompt = "Enter numeric index of the last letter of YEAR: "))
  pts$AVG_YEAR <- substr(pts$ID, year_start, year_end)

  # Handle short year formats
  if (any(nchar(pts$AVG_YEAR) == 2)) {
    pts$AVG_YEAR <- ifelse(nchar(pts$AVG_YEAR) == 2, paste0("20", pts$AVG_YEAR), pts$AVG_YEAR)
  }

  # Add additional columns
  pts$SIZE_HA <- ha

  # Format output based on raster type
  if (raster_type == "AGB") {
    pts <- pts[, c("PLOT_ID", "POINT_X", "POINT_Y", "AGB", "AVG_YEAR")]
  } else if (raster_type == "CV") {
    pts <- pts[, c("PLOT_ID", "POINT_X", "POINT_Y", "CV", "AVG_YEAR")]
  } else if (raster_type == "SD") {
    pts <- pts[, c("PLOT_ID", "POINT_X", "POINT_Y", "SD", "AVG_YEAR")]
  }

  return(pts)
}
