## Updates made to the new framework:
# 04/12/2024:
# - Replaced sp functions with sf equivalents.
# - Replaced raster functions with terra equivalents.
# - Updated the function to work with sf objects instead of SpatialPolygonsDataFrame.
# - Removed the global variable SRS and assumed WGS84 as the default coordinate reference system.
# - Possibility to set the Hansen data GFC tiles version, from "GFC-2023-v1.11" (default) to "GFC-2018-v1.6".
# 27/01/2025:
# - Making less assumptions about the structure of plot data, for ex not assuming a Xnew column if POINT_X column not existing.
# - Corrected bug which removed plots with >50% deforestation vs >5% deforestation


## Notes:
# 04/12/2024:
# - Could not test old_Deforested() because of deprecated packages and the related unreproducible environment.
# Instead we use a refactored_old_Deforested() with the minimum changes that make it work and compare old vs new function outputs to
# validate the new function.



#' Remove deforested plots by overlaying plots with Global Forest Change data
#'
#' This function identifies and removes plots that have been deforested based on
#' the Global Forest Change (GFC) dataset (Hansen et al., 2013). It processes each plot,
#' downloads the necessary forest loss tiles, and determines if the plot has been
#' deforested beyond a set deforestation threshold or if the deforestation occurred before or during the specified map year.
#'
#' @param plt A data frame or sf object containing plot data. For data frame input format, longitude and latitude coordinates should be placed under "POINT_X" and "POINT_Y" columns respectively in WGS 84 CRS.
#' @param map_year Numeric value indicating the threshold year for deforestation. Plots with deforestation started at or before the `map_year` will be removed from the `$non_deforested_plots` list element output. Any year in the 2001-2023 range.
#' @param gfc_folder Character string specifying the directory to download GFC data.
#' @param gfc_dataset_year Numeric value describing which version of the Hansen data to use: any year in the 2018-2023 range or "latest" (default).
#' @param defo_threshold Numeric value indicating the deforestation threshold. Plots with a deforestation area proportion larger than the set `defo_threshold` will be removed from the `$non_deforested_plots` list element output. Default is 5%.
#'
#' @return A list containing two elements:
#'   \item{non_deforested_plots}{A sf object with non-deforested plots}
#'   \item{all_plots}{The original input sf object with added deforestated proportion (0-1) w.r.t. to plot area and deforestation start year}
#'
#' @importFrom sf st_as_sf st_buffer st_bbox st_coordinates
#' @importFrom terra rast extract
#' @importFrom dplyr select setdiff
#' @importFrom gfcanalysis calc_gfc_tiles download_tiles
#' @import gfcanalysis
#'
#' @export
#'
#' @references M. C. Hansen et al., High-Resolution Global Maps of 21st-Century Forest Cover Change. Science342,850-853(2013). [DOI:10.1126/science.1244693](https://doi.org/10.1126/science.1244693)
#'
#' @examples
#' # 4 plots without and 4 plots with deforestation:
#' plots_sample <- c(1, 2, 3, 4, 182, 200, 323, 6765)
#' sampled_plots <- plots[plots_sample,]
#' Deforested(sampled_plots, 2010)
#'
Deforested <- function(plt, map_year, gfc_folder = "data/GFC", gfc_dataset_year = "latest", defo_threshold = 0.05) {

  library(gfcanalysis)

  # Refactored version, more radical changes (changing to terra, sf ecosystem, etc)

  # Verify gfc_folder exists, create if it doesn't
  if (!dir.exists(gfc_folder)) {
    dir.create(gfc_folder, recursive = TRUE)
    message(paste("Created directory:", gfc_folder))
  }

  if (!map_year %in% seq(2001, 2023)) {
    stop("Invalid map_year Please use a year between 2001 and 2023.")
  }

  if (gfc_dataset_year == "latest") {
    gfc_dataset_year <- 2023
  }

  if (!gfc_dataset_year %in% seq(2023, 2015)) {
    stop("Invalid gfc_dataset_year. Please use a year between 2015 and 2023.")
  }

  if (gfc_dataset_year < map_year) {
    stop("The gfc_dataset_year must be equal or later than the map_year.")
  }

  dataset_str <- paste0("GFC-", gfc_dataset_year, "-v1.", (gfc_dataset_year-2018) + 6)

  if ((!"POINT_X" %in% colnames(plt)) & ("Xnew" %in% colnames(plt))) {
    plt$POINT_X <- plt$Xnew
    plt$POINT_Y <- plt$Ynew
    plt$PLOT_ID <- 1:nrow(plt)
  } else if ((!"POINT_X" %in% colnames(plt)) & (!"Xnew" %in% colnames(plt))) {
    stop("Invalid plot data. Please check that both ´POINT_X' and ´POINT_Y´ columns (corresponding to lon, lat points in WGS 84) are provided.")
  }

  if (!inherits(plt, "sf")) {
    plt <- sf::st_as_sf(plt, coords = c("POINT_X", "POINT_Y"), crs = 4326)
  }

  defo <- numeric(nrow(plt))
  defo_start_year <- rep(NA, nrow(plt))

  for (p in 1:nrow(plt)) {

    # Make a square polygon with a ww buffer
    ww <- ifelse(!(is.na(plt[p,]$SIZE_HA)),
                 (sqrt(plt[p,]$SIZE_HA*10000) *0.00001)/2, 0.0002) # mean of plots for NAs, ww in arc-deg
    ww <- ifelse(ww < 0, abs(ww), ww)
    sf_use_s2(FALSE)  # ww in sf:: with sf_use_s2(TRUE) is in meters
    pol <- suppressMessages(suppressWarnings(sf::st_buffer(plt[p,], dist = ww, endCapStyle = "SQUARE")))
    # diff(st_bbox(pol)[c(2, 4)])
    # diff(st_bbox(pol)[c(1, 3)])

    message(paste("Processing row", p, "with PLOT_ID", plt[p,]$PLOT_ID, "and buffered area", round(as.numeric(sf::st_area(pol) / 10000), 2), "ha ... \n"))
    #cat(paste('processing:', round((res / 0.00001 * res / 0.00001) / 10000, 2), 'ha')) # checker

    # Downloads respective forest loss tile/s from squared plots
    #dir.create(file.path(gfc_folder), showWarnings = FALSE)
    #setwd(file.path(gfc_folder))
    gfcTile <- suppressMessages(suppressWarnings(gfcanalysis::calc_gfc_tiles(pol)))
    gfcanalysis::download_tiles(gfcTile, gfc_folder, images = "lossyear", dataset = dataset_str)

    # Get overlapping tile/s (up to 4 possible tiles)
    bb <- sf::st_bbox(pol)
    crds <- expand.grid(x = bb[c(1, 3)], y = bb[c(2, 4)])
    fnms <- character(4)

    for (i in 1:nrow(crds)) {
      lon <- 10 * (crds[i, 1] %/% 10)
      lat <- 10 * (crds[i, 2] %/% 10) + 10
      LtX <- ifelse(lon < 0, "W", "E")
      LtY <- ifelse(lat < 0, "S", "N")
      WE <- paste0(sprintf('%03d', abs(lon)), LtX)
      NS <- paste0(sprintf('%02d', abs(lat)), LtY)

      fnms[i] <- file.path(gfc_folder, paste0("Hansen_", dataset_str, "_lossyear_", NS, "_", WE, ".tif"))
    }

    vls <- numeric()
    for (f in fnms) {
      if (file.exists(f))
        vls <- c(vls, terra::extract(terra::rast(f), pol)[[2]])
    }

    # vls <- if (length(vls[vls == 0]) > length(vls[vls > 0])) vls * 0 else (c(vls)) # implies a 50% threshold

    if (any(vls > 0)) {
      defo_start <- as.numeric(paste0("20", sprintf("%02d", round(min(vls), 0))))
      nr_no_defo_cells <- length(vls[vls == 0])
      nr_defo_cells <- length(vls[vls > 0])
      ratio_defo_cells <- nr_defo_cells / (nr_defo_cells + nr_no_defo_cells)

      message(paste0("Deforestation detected in ", 100 * ratio_defo_cells, "% of the buffered area in plot row ", p, " with PLOT_ID ", plt[p,]$PLOT_ID, ". The earliest measured year of deforestation is ", defo_start, ".\n"))
    } else if (all(vls == 0)) {
      ratio_defo_cells <- 0
      defo_start <- NA
    }

    defo[p] <- ratio_defo_cells
    defo_start_year[p] <- defo_start
  }

  plt$defo <- defo
  plt$defo_start_year <- defo_start_year

  # Plots deforested before map year:
  deforestedPlots <- plt |>
    dplyr::filter(defo > defo_threshold) |>
    dplyr::filter(!is.na(defo_start_year)) |>
    dplyr::filter(defo_start_year <= map_year)

  netPlt <- dplyr::select(dplyr::setdiff(plt, deforestedPlots), -c(defo, defo_start_year))

  message(paste('Removed', nrow(deforestedPlots), 'plot(s) that have >', 100 * defo_threshold, '% deforested area before/during the', map_year, 'map year.'))

  return(list(non_deforested_plots = netPlt, all_plots = plt))
}



# old_refactored_Deforested <- function(plt, fdir, map_year=10){
#
#   library(sp)
#
#   # Test params
#   # dataDir <- file.path(getwd(), "data")
#   # plotsFile <- file.path(dataDir, 'SamplePlots.csv')
#   # plots <- read.csv(plotsFile)
#   # plt = plots[1:25,]
#   # fdir = "data/testdata"
#   # map_year=10
#
#   # Refactored version, but conservative (least changes possible to make it work)
#
#   if(!"POINT_X" %in% colnames(plt)){
#     plt$POINT_X <- plt$Xnew
#     plt$POINT_Y <- plt$Ynew
#     plt$PLOT_ID <- 1:nrow(plt)
#   }
#
#   if (class(plt)[1] == 'SpatialPolygonsDataFrame'){
#     plt <- as.data.frame(pt)
#   }
#
#   defo <- c()
#   for (p in 1:nrow(plt)){
#
#     #make a square polygon from plot size
#     xy <- SpatialPoints(plt[p,c('POINT_X', 'POINT_Y')])
#     ch <- chull(plt[p,c('POINT_X', 'POINT_Y')]) #main plot ID needed
#     ww <- ifelse(!(is.na(plt[p,]$SIZE_HA)),
#                  (sqrt(plt[p,]$SIZE_HA*10000) *0.00001)/2, 0.0002) #mean of plots for NAs
#     ww <- ifelse(ww < 0, abs(ww), ww)
#     # > ww
#     # [1] 5e-04
#     pol <- gBuffer(xy, width=ww, quadsegs=1, capStyle="SQUARE") # deprecated
#     # > pol
#     # class       : SpatialPolygons
#     # features    : 1
#     # extent      : 24.53856, 24.53956, 0.9920556, 0.9930556  (xmin, xmax, ymin, ymax)
#     # crs         : NA
#     proj4string(pol) <- CRS('+init=epsg:4326')
#     res <- pol@bbox[4] - pol@bbox[2]
#     # > res
#     # [1] 0.001
#     print(paste('processing:',round((res / 0.00001 * res / 0.00001 ) /10000,2), 'ha'))#checker
#
#     #downloads respective forest loss tile/s from squared plots
#     bb <- unname(bbox(pol))
#     crds <- expand.grid(x=bb[1,],y=bb[2,])
#     fnms <- character(4)
#
#     # > pol
#     # class       : SpatialPolygons
#     # features    : 1
#     # extent      : 24.53856, 24.53956, 0.9920556, 0.9930556  (xmin, xmax, ymin, ymax)
#     # crs         : +proj=longlat +datum=WGS84 +no_defs
#
#     # > bb[1,]
#     # [1] 24.53856 24.53956
#     # > bb[2,]
#     # [1] 0.9920556 0.9930556
#
#     # [,1]       [,2]
#     # [1,] 24.5385556 24.5395556
#     # [2,]  0.9920556  0.9930556
#     # > crds
#     # x         y
#     # 1 24.53856 0.9920556
#     # 2 24.53956 0.9920556
#     # 3 24.53856 0.9930556
#     # 4 24.53956 0.9930556
#     # > fnms
#     # [1] "" "" "" ""
#
#     dir.create(file.path(fdir))
#     #setwd(file.path(fdir))
#     gfcTile <- calc_gfc_tiles(sf::st_as_sf(pol))
#     download_tiles(gfcTile, fdir, images = "lossyear", dataset='GFC-2018-v1.6')
#
#     #get overlapping tile/s (up to 4 possible tiles)
#     for(i in 1:nrow(crds)){
#
#       lon <- 10*(crds[i,1]%/%10)
#       lat <- 10*(crds[i,2]%/%10) + 10
#       LtX <- ifelse(lon < 0, "W", "E")
#       LtY <- ifelse(lat < 0, "S", "N")
#       WE <- paste0(sprintf('%03d',abs(lon)), LtX)
#       NS <- paste0(sprintf('%02d',abs(lat)), LtY)
#
#       fnms[i] <- file.path(fdir, paste0('Hansen_GFC-2018-v1.6_lossyear_',NS, "_", WE,".tif"))
#
#       vls <- numeric()
#       for(f in fnms){
#         if(file.exists(f))
#           vls <- c(vls, extract(raster(f), pol)[[1]])
#         # > vls
#         # [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#       }
#     }
#     vls <- if(length(vls[vls==0]) > length(vls[vls>0])) vls*0 else(c(vls))
#     #if there is more non-deforested (zeros)
#     print(vls)
#     vls[vls>0] <- 1
#
#     defo[p] <- sum(vls[vls>0], na.rm=T) # exact 0 means no deforestation, will return NaN
#     cat(defo[p])
#   }
#
#   plt$defo <- defo
#   thresh <- plt$SIZE_HA * 0.05
#   print(paste('Removed', print(nrow(subset(plt,plt$defo > thresh ))), 'plots that have >5% change'))
#   defPlt <- subset(plt, plt$defo > 0 ) #if there is deforestation
#   defPlt <- subset(defPlt, defPlt$defo <= map_year)#if plot is older or equal to map year
#   netPlt <- dplyr::select(dplyr::setdiff(plt, defPlt),-defo) #removes defo checker column
#   return(list(netPlt, plt)) #returns non-deforested and original plots
# }




# ### SCRIPT TO REMOVE DEFORESTED PLOTS BY OVERLAYING PLOTS TO FOREST LOSS DATA FROM GLOBAL FOREST CHANGE
# ### Hansen et al., 2013
#
# old_Deforested <- function(plt=plt, fdir = flFolder, map_year=10){
#
#   if(!"POINT_X" %in% colnames(plt)){
#     plt$POINT_X <- plt$Xnew
#     plt$POINT_Y <- plt$Ynew
#     plt$PLOT_ID <- 1:nrow(plt)
#   }
#
#   if (class(plt)[1] == 'SpatialPolygonsDataFrame'){
#     plt <- as.data.frame(pt)  new_result <- Deforested(sampled_plots, "data/testdata", 2018, 10)

#   }
#
#   defo <- c()
#   for (p in 1:nrow(plt)){
#
#     #make a square polygon from plot size
#     xy <- SpatialPoints(plt[p,c('POINT_X', 'POINT_Y')])
#     ch <- chull(plt[p,c('POINT_X', 'POINT_Y')]) #main plot ID needed
#     ww <- ifelse(!(is.na(plt[p,]$SIZE_HA)),
#                  (sqrt(plt[p,]$SIZE_HA*10000) *0.00001)/2, 0.0002) #mean of plots for NAs
#     ww <- ifelse(ww < 0, abs(ww), ww)
#     pol <- gBuffer(xy, width=ww, quadsegs=1, capStyle="SQUARE")
#     proj4string(pol) <- SRS
#     res <- pol@bbox[4] - pol@bbox[2]
#     print(paste('processing:',round((res / 0.00001 * res / 0.00001 ) /10000,2), 'ha'))#checker
#
#     #downloads respective forest loss tile/s from squared plots
#     bb <- unname(bbox(pol))
#     crds <- expand.grid(x=bb[1,],y=bb[2,])
#     fnms <- character(4)
#
#     dir.create(file.path(fdir))
#     setwd(file.path(fdir))
#     gfcTile <- calc_gfc_tiles(pol)
#     download_tiles(gfcTile, fdir, images = "lossyear", dataset='GFC-2018-v1.6')
#
#     #get overlapping tile/s (up to 4 possible tiles)
#     for(i in 1:nrow(crds)){
#
#       lon <- 10*(crds[i,1]%/%10)
#       lat <- 10*(crds[i,2]%/%10) + 10
#     1  LtX <- ifelse(lon < 0, "W", "E")
#       LtY <- ifelse(lat < 0, "S", "N")
#       WE <- paste0(sprintf('%03d',abs(lon)), LtX)
#       NS <- paste0(sprintf('%02d',abs(lat)), LtY)
#
#       fnms[i] <- file.path(fdir, paste0('Hansen_GFC-2018-v1.6_lossyear_',NS, "_", WE,".tif"))
#
#       vls <- numeric()
#       for(f in fnms){
#         if(file.exists(f))
#           vls <- c(vls, extract(raster(f), pol)[[1]])
#       }
#     }
#     vls <- if(length(vls[vls==0]) > length(vls[vls>0])) vls*0 else(c(vls))
#     #if there is more non-deforested (zeros)
#     print(vls)
#     vls[vls>0] <- 1
#
#     defo[p] <- sum(vls[vls>0], na.rm=T) # exact 0 means no deforestation, will return NaN
#     print(defo[p])
#   }
#
#   plt$defo <- defo
#   thresh <- plt$SIZE_HA * 0.05
#   print(paste('Removed', print(nrow(subset(plt,plt$defo > thresh ))), 'plots that have >5% change'))
#   defPlt <- subset(plt, plt$defo > 0 ) #if there is deforestation
#   defPlt <- subset(defPlt, defPlt$defo <= map_year)#if plot is older or equal to map year
#   netPlt <- dplyr::select(dplyr::setdiff(plt, defPlt),-defo) #removes defo checker column
#   return(list(netPlt, plt)) #returns non-deforested and original plots
# }




# Testing

# # rgeos is deprecated but we need it for testing the old function:
# if(!require(rgeos)){
#   remotes::install_version("rgeos", version = "0.6-4")
#   library(rgeos)
# }
#
# library(testthat)
# library(sf)
# library(terra)
# library(sp)
# library(raster)
# library(gfcanalysis)
#
#
# test_that("Old and new Deforested functions produce consistent results - full sample", {
#
#   # The full sample test (8321 plots) passed, but it took ~3 hours to complete, so we skip it in future testing pipelines
#   # The new function seems about 3.25x more efficient than the older one.
#   #skip_on_ci()
#   skip(message = "Skipping because it takes too long, next test is similar but takes a smaller sample")
#
#   # Test params
#   #plots <- read.csv(file.path("data", "SamplePlots.csv"))
#
#   # Run both versions
#   start.time <- Sys.time()
#   old_result <- old_refactored_Deforested(plots, "data/testdata", 10)
#   end.time <- Sys.time()
#   old.time.taken <- end.time - start.time
#   old.time.taken
#   # Time difference of 2.244613 hours
#
#   start.time <- Sys.time()
#   new_result <- Deforested(plots, "data/testdata", 2018, 10)
#   end.time <- Sys.time()
#   new.time.taken <- end.time - start.time
#   new.time.taken
#   # Time difference of 41.50442 mins
#
#   # Compare results
#   expect_equal(dim(old_result[[1]])[1], dim(new_result[[1]])[1])
#   expect_equal(dim(old_result[[2]])[1], dim(new_result[[2]])[1])
#   expect_equal(old_result[[2]]$defo, new_result[[2]]$defo, tolerance = 1e-6)
#
# })
#
#
# test_that("Old and new Deforested functions produce consistent results - small sample", {
#
#   skip(message = "Skipping because Deforested has now a different API from old version")
#
#   # Test params
#   #plots <- read.csv(file.path("data", "SamplePlots.csv"))
#   set.seed(42)
#   plots_sample <- c(sample(1:dim(plots)[1], 10), 182, 200, 323, 6765) # handpicking some plots with deforestation
#   sampled_plots <- plots[plots_sample,]
#
#   # Run both versions
#   # Broken old_Deforested() because of deprecated packages and unreproducible environment, we use old_refactored_Deforested() instead
#   # SRS <- CRS('+init=epsg:4326')
#   # old_result <- old_Deforested(plot_data, mock_fdir, 10)
#   old_result <- old_refactored_Deforested(sampled_plots, "data/testdata", 10)
#
#   new_result <- Deforested(sampled_plots, gfc_dataset_year = 2018, map_year = 2010)
#
#   # Compare results
#   expect_equal(dim(old_result[[1]])[1], dim(new_result[[1]])[1])
#   expect_equal(dim(old_result[[2]])[1], dim(new_result[[2]])[1])
#   expect_equal(old_result[[2]]$defo, new_result[[2]]$defo, tolerance = 1e-6)
#
# })
#
#
# test_that("Deforested function behaves consistently across map_years", {
#
#   # Test params
#   #plots <- read.csv(file.path("data", "SamplePlots.csv"))
#   set.seed(42)
#   plots_sample <- c(sample(1:dim(plots)[1], 10), 182, 200, 323, 6765) # handpicking some plots with deforestation
#   sampled_plots <- plots[plots_sample,]
#
#   result_2010 <- Deforested(sampled_plots, 2010)
#   result_2020 <- Deforested(sampled_plots, 2020)
#   result_2023 <- Deforested(sampled_plots, 2023)
#
#   # Compare results
#   expect_equal(
#     unique(result_2010$all_plots |>
#              dplyr::filter(defo > 0) |>
#              dplyr::select(defo_start_year)),
#     unique(result_2020$all_plots |>
#              dplyr::filter(defo > 0) |>
#              dplyr::select(defo_start_year))
#   )
#   expect_equal(
#     unique(result_2020$all_plots |>
#              dplyr::filter(defo > 0) |>
#              dplyr::select(defo_start_year)),
#     unique(result_2023$all_plots |>
#              dplyr::filter(defo > 0) |>
#              dplyr::select(defo_start_year))
#   )
# })
#
# test_that("Deforested function behaves consistently across gfc_dataset_year", {
#
#   # Test params
#   #plots <- read.csv(file.path("data", "SamplePlots.csv"))
#   set.seed(42)
#   plots_sample <- c(sample(1:dim(plots)[1], 10), 182, 200, 323, 6765) # handpicking some plots with deforestation
#   sampled_plots <- plots[plots_sample,]
#
#   result_gfc_2015 <- Deforested(sampled_plots, map_year = 2010, gfc_dataset_year = 2015)
#   result_gfc_2019 <- Deforested(sampled_plots, map_year = 2010, gfc_dataset_year = 2019)
#   result_gfc_2022 <- Deforested(sampled_plots, map_year = 2010, gfc_dataset_year = 2022)
#   result_gfc_2023 <- Deforested(sampled_plots, map_year = 2010, gfc_dataset_year = 2023)
#   result_gfc_latest <- Deforested(sampled_plots, map_year = 2010)
#
#   # Compare results
#   expect_equal(result_gfc_2015$non_deforested_plots, result_gfc_2019$non_deforested_plots)
#   expect_equal(result_gfc_2022$non_deforested_plots, result_gfc_2019$non_deforested_plots)
#   expect_equal(result_gfc_2015$non_deforested_plots, result_gfc_2023$non_deforested_plots)
#   expect_equal(result_gfc_latest$non_deforested_plots, result_gfc_2023$non_deforested_plots)
#
#   expect_equal(result_gfc_latest$all_plots, result_gfc_2023$all_plots)
#
#   expect_equal(sum(result_gfc_2015$all_plots$defo), 2)
#   expect_equal(sum(result_gfc_2019$all_plots$defo), 4)
#   expect_equal(sum(result_gfc_2022$all_plots$defo), 5)
#   expect_equal(sum(result_gfc_2023$all_plots$defo), sum(result_gfc_latest$all_plots$defo))
#
# })
#
# test_that("Deforested function behaves consistently", {
#
#   # Test params
#   #plots <- read.csv(file.path("data", "SamplePlots.csv"))
#   set.seed(42)
#   plots_sample <- sample(1:dim(plots)[1], 2)
#   sampled_plots <- plots[plots_sample,]
#
#   result <- Deforested(sampled_plots, map_year = 2010)
#
#   # Check output structure
#   expect_type(result, "list")
#   expect_length(result, 2)
#   expect_s3_class(result[[1]], "data.frame")
#   expect_s3_class(result[[2]], "data.frame")
#
#   # Check column names
#   expected_cols <- c("PLOT_ID", "AGB_T_HA", "AVG_YEAR", "geometry", "defo")
#   expect_true(all(expected_cols %in% colnames(result[[2]])))
#
#   # Check deforestation filtering
#   expect_true(all(result[[2]]$defo <= 0.05))
# })

