
#' Compute tree cover forest mask for a specific year
#'
#' This function computes a tree cover forest mask layer for a given year based on a 2000 tree cover baseline - the Hansen Global Forest Change (GFC) dataset
#' or a 2010 tree cover baseline - the GLAD 2010 Tree Canopy Cover dataset.
#'
#' @param year Integer. The year for which to compute the tree cover (2001-2023).
#' @param gfc_folder Character. Path to the folder containing GFC dataset files. Default is "data/GFC".
#' @param treecover_threshold Numeric. Optional threshold for tree cover (0-100) at baseline year. If provided, only areas with
#'        tree cover values >= this threshold will be considered. Default is NULL (no threshold).
#' @param coords Character. Coordinates of the tile to process, e.g., "10N_020E". If NULL, processes all available tiles.
#' @param output_folder Character. Path to the folder where output rasters should be saved.
#' @param num_cores Integer. Number of cores to use for parallel processing.
#' @param baseline Integer. Baseline year for tree cover dataset. Can be 2010 (default) or 2000.
#'
#' @return A list. If coords is specified, only one tile is processed and returns a file path to a single raster.
#'         Otherwise, returns a set of file paths, each representing the tree cover at baseline for the specified year for different tiles.
#'         The rasters are computed taking the tree cover values from the baseline rasters, removing pixels where forest loss was recorded at or before the
#'         provided year. If a tree cover threshold is provided, all tree cover pixels below the threshold are set to NA. Rasters are saved as a GeoTIFF file
#'         saved in the output_folder.
#'
#' @import terra stringr parallel pbapply
#'
#' @details
#' This function computes a tree cover forest mask by leveraging a baseline tree cover and forest loss data.
#' It uses two primary types of input files:
#' \enumerate{
#'   \item Tree cover data, which serves as the baseline:
#'     \itemize{
#'       \item For the 2000 baseline, the function uses files matching the pattern "Hansen_GFC-2023-v1.11_treecover2000_*.tif".
#'       \item For the 2010 baseline, the function uses files matching the pattern "treecover2010_*.tif".
#'       \item Tree cover values represent the percentage of tree cover (0-100%).
#'     }
#'   \item Forest loss year data:
#'     \itemize{
#'       \item The function uses files matching the pattern "Hansen_GFC-2023-v1.11_lossyear_*.tif".
#'       \item Values in these files indicate the year of forest loss (1-23, corresponding to years 2001-2023), or 0 if no loss occurred.
#'     }
#' }
#' The function applies the following steps:
#' \enumerate{
#'   \item It reads the tree cover and forest loss rasters for the specified year and location.
#'   \item If a `treecover_threshold` is provided, all tree cover pixels with values below the threshold are set to NA.
#'   \item It removes pixels where forest loss was recorded at or before the specified year.
#'   \item The resulting raster represents a tree cover forest mask for the specified year, with areas of low tree cover or forest loss masked out.
#'   \item The function saves the resulting raster as a GeoTIFF file in the `output_folder`.
#' }
#'
#' @examples
#' \dontrun{
#' # Compute all tiles tree cover for 2015 using 2010 baseline
#' treecover_2015 <- compute_treecover(2015)
#'
#' # Compute 30% thresholded tree cover for the 10N, 20E tile for 2005 using
#' # 2000 tree cover baseline
#' treecover_2020 <- compute_treecover(2020,
#'                                     treecover_threshold = 30,
#'                                     coords = "10N_020E",
#'                                     output_folder = "data/treecover_calc",
#'                                     baseline = 2000)
#' }
#' @export
compute_treecover <- function(year, gfc_folder = "data/GFC", treecover_threshold = NULL,
                              coords = NULL, output_folder = "data/treecover_calc", num_cores = 1, baseline = 2010) {
  if (year < 2001 || year > 2023) {
    stop("Year must be between 2001 and 2023.")
  }

  if (baseline != 2000 && baseline != 2010) {
    stop("Baseline must be either 2000 or 2010.")
  }

  # Check if output_folder exists and create it if it doesn't
  if (!is.null(output_folder) && !dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
    message(paste("Created output folder:", output_folder))
  }

  treecover_pattern <- if(baseline == 2000) "treecover2000.*\\.tif$" else "treecover2010.*\\.tif$"
  treecover_files <- list.files(gfc_folder, pattern = treecover_pattern, full.names = TRUE)
  lossyear_files <- list.files(gfc_folder, pattern = "lossyear.*\\.tif$", full.names = TRUE)

  if (!is.null(coords)) {
    treecover_files <- treecover_files[grep(coords, treecover_files)]
    lossyear_files <- lossyear_files[grep(coords, lossyear_files)]
    if (length(treecover_files) == 0 || length(lossyear_files) == 0) {
      stop("No matching files found for the specified coordinates.")
    }
  }

  extract_info <- function(filename) {
    if (baseline == 2000) {
      year_version <- str_extract(filename, "GFC-\\d{4}-v\\d+\\.\\d+")
    } else {
      year_version <- "TCC-2010"
    }
    coords <- str_extract(filename, "\\d{2}[NS]_\\d{3}[EW]")
    list(year_version = year_version, coords = coords)
  }

  treecover_info <- lapply(treecover_files, extract_info)
  lossyear_info <- lapply(lossyear_files, extract_info)

  create_identifier <- function(info) {
    paste(info$year_version, info$coords, sep = "_")
  }

  treecover_ids <- sapply(treecover_info, create_identifier)
  lossyear_ids <- sapply(lossyear_info, create_identifier)

  common_ids <- intersect(treecover_ids, lossyear_ids)

  cl <- makeCluster(num_cores)
  on.exit(stopCluster(cl))

  clusterExport(cl, c("process_pair", "year", "treecover_threshold",
                      "output_folder", "treecover_files", "lossyear_files",
                      "treecover_ids", "lossyear_ids", "baseline"),
                envir = environment())

  clusterEvalQ(cl, {
    library(terra)
    library(stringr)
  })

  result_rasters <- pbapply::pblapply(common_ids, function(id) {
    process_pair(id, year, treecover_threshold, output_folder,
                 treecover_files, lossyear_files, treecover_ids, lossyear_ids, baseline)
  }, cl = cl)

  names(result_rasters) <- common_ids
  return(result_rasters)
}



#' Process a single pair of tree cover and forest loss files
#'
#' @param id The identifier for the file pair
#' @param year The year for which to compute tree cover
#' @param treecover_threshold The threshold for tree cover
#' @param output_folder The folder to save output rasters
#' @param treecover_files List of tree cover files
#' @param lossyear_files List of forest loss files
#' @param treecover_ids IDs for tree cover files
#' @param lossyear_ids IDs for forest loss files
#' @param baseline Baseline year for tree cover dataset (2000 or 2010)
#'
#' @return A file path to a geotiff raster representing the computed tree cover
process_pair <- function(id, year, treecover_threshold, output_folder,
                         treecover_files, lossyear_files, treecover_ids, lossyear_ids, baseline) {

  # Load baseline tree cover and forest loss rasters
  treecover_file <- treecover_files[treecover_ids == id]
  lossyear_file <- lossyear_files[lossyear_ids == id]

  treecover_baseline <- rast(treecover_file)
  forest_loss <- rast(lossyear_file)

  # Ensure rasters have the same extent and resolution
  if (!compareGeom(treecover_baseline, forest_loss, stopOnError = FALSE)) {
    forest_loss <- resample(forest_loss, treecover_baseline)
  }

  # Convert year to two-digit format for comparison with forest loss raster
  year_2digit <- year %% 100

  # Apply threshold to baseline tree cover (if specified)
  if (!is.null(treecover_threshold)) {
    # Explicitly set values below the threshold to NA
    treecover_baseline[treecover_baseline < treecover_threshold] <- NA
  }

  # Create a binary mask for forest loss up to the specified year
  forest_loss_binary <- forest_loss > 0 & forest_loss <= year_2digit

  # Subtract forest loss from baseline tree cover
  treecover_y <- mask(treecover_baseline, !forest_loss_binary)

  # Save output raster to disk
  if (!is.null(output_folder)) {
    if (!dir.exists(output_folder)) {
      dir.create(output_folder, recursive = TRUE)
    }
    original_filename <- basename(treecover_file)
    filename_parts <- strsplit(original_filename, "_")[[1]]
    new_filename <- paste0(
      ifelse(baseline == 2000, "Hansen_", "GLAD_"),
      ifelse(baseline == 2000, filename_parts[2], "TCC-2010"),
      "_treecover_calc_", year, "_",
      filename_parts[length(filename_parts)-1], "_", filename_parts[length(filename_parts)]
    )
    output_path <- file.path(output_folder, new_filename)
    writeRaster(treecover_y, output_path, overwrite = TRUE)

    return(output_path)
  }
}


#' #' Compute tree cover for a specific year
#' #'
#' #' This function computes a tree cover layer for a given year based on the Hansen Global Forest Change (GFC) dataset.
#' #' It uses the tree cover from 2000 as a baseline and removes cells with forest loss up to the specified year.
#' #'
#' #' @param year Integer. The year for which to compute the tree cover (2001-2023).
#' #' @param gfc_folder Character. Path to the folder containing GFC dataset files. Default is "data/GFC".
#' #' @param treecover_threshold Numeric. Optional threshold for tree cover (0-100) at baseline year (2000). If provided, only areas with
#' #'        tree cover values >= this threshold will be considered. Default is NULL (no threshold).
#' #' @param coords Character. Coordinates of the tile to process, e.g., "10N_020E". If NULL, processes all available tiles.
#' #' @param output_folder Character. Path to the folder where output rasters should be saved. If NULL, rasters are not saved to disk. Default is NULL.
#' #' @param num_cores Integer. Number of cores to use for parallel processing.
#' #'
#' #' @return If coords is specified or only one tile is processed, returns a single SpatRaster object.
#' #'         Otherwise, returns a list of SpatRaster objects, each representing tree cover for the specified year for different tiles.
#' #'
#' #' @import terra stringr parallel pbapply
#' #'
#' #' @details
#' #' The function uses two pattern matched (matched by GFC dataset year, version and lat, log coordinates)
#' #'  types of input files stored in the specified gfc_folder, for example:
#' #' 1. Tree cover 2000: "Hansen_GFC-2023-v1.11_treecover2000_*.tif"
#' #'    - Values represent percentage of tree cover (0-100%)
#' #' 2. Forest loss year: "Hansen_GFC-2023-v1.11_lossyear_*.tif"
#' #'    - Values: 0 (no loss) or 1-23 (loss year 2001-2023)
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # Compute tree cover for 2015 for all available tiles
#' #' treecover_2015 <- compute_treecover(2015)
#' #'
#' #' # Compute tree cover for 2020 for a specific tile with a baseline tree cover threshold of 30% and save output
#' #' treecover_2020 <- compute_treecover(2020,
#' #'                                     treecover_threshold = 30,
#' #'                                     coords = "10N_020E",
#' #'                                     output_folder = "data/treecover_calc")
#' #' }
#' #' @export
#' compute_treecover <- function(year, gfc_folder = "data/GFC", treecover_threshold = NULL,
#'                               coords = NULL, output_folder = NULL, num_cores = 1) {
#'   if (year < 2001 || year > 2023) {
#'     stop("Year must be between 2001 and 2023.")
#'   }
#'
#'   treecover_files <- list.files(gfc_folder, pattern = "treecover2000.*\\.tif$", full.names = TRUE)
#'   lossyear_files <- list.files(gfc_folder, pattern = "lossyear.*\\.tif$", full.names = TRUE)
#'
#'   if (!is.null(coords)) {
#'     treecover_files <- treecover_files[grep(coords, treecover_files)]
#'     lossyear_files <- lossyear_files[grep(coords, lossyear_files)]
#'     if (length(treecover_files) == 0 || length(lossyear_files) == 0) {
#'       stop("No matching files found for the specified coordinates.")
#'     }
#'   }
#'
#'   extract_info <- function(filename) {
#'     year_version <- str_extract(filename, "GFC-\\d{4}-v\\d+\\.\\d+")
#'     coords <- str_extract(filename, "\\d{2}[NS]_\\d{3}[EW]")
#'     list(year_version = year_version, coords = coords)
#'   }
#'
#'   treecover_info <- lapply(treecover_files, extract_info)
#'   lossyear_info <- lapply(lossyear_files, extract_info)
#'
#'   create_identifier <- function(info) {
#'     paste(info$year_version, info$coords, sep = "_")
#'   }
#'
#'   treecove
#'   r_ids <- sapply(treecover_info, create_identifier)
#'   lossyear_ids <- sapply(lossyear_info, create_identifier)
#'
#'   common_ids <- intersect(treecover_ids, lossyear_ids)
#'
#'   cl <- makeCluster(num_cores)
#'   on.exit(stopCluster(cl))
#'
#'   clusterExport(cl, c("process_pair", "year", "treecover_threshold",
#'                       "output_folder", "treecover_files", "lossyear_files",
#'                       "treecover_ids", "lossyear_ids"),
#'                 envir = environment())
#'
#'   clusterEvalQ(cl, {
#'     library(terra)
#'     library(stringr)
#'   })
#'
#'
#'   result_rasters <- pblapply(common_ids, function(id) {
#'     process_pair(id, year, treecover_threshold, output_folder,
#'                  treecover_files, lossyear_files, treecover_ids, lossyear_ids)
#'   }, cl = cl)
#'
#'   names(result_rasters) <- common_ids
#'   return(result_rasters)
#' }
#'
#'
#' #' Process a single pair of tree cover and forest loss files
#' #'
#' #' @param id The identifier for the file pair
#' #' @param year The year for which to compute tree cover
#' #' @param treecover_threshold The threshold for forest loss
#' #' @param output_folder The folder to save output rasters
#' #' @param treecover_files List of tree cover files
#' #' @param lossyear_files List of forest loss files
#' #' @param treecover_ids IDs for tree cover files
#' #' @param lossyear_ids IDs for forest loss files
#' #'
#' #' @return A SpatRaster object representing the computed tree cover
#' process_pair <- function(id, year, treecover_threshold, output_folder,
#'                          treecover_files, lossyear_files, treecover_ids, lossyear_ids) {
#'
#'   treecover_file <- treecover_files[treecover_ids == id]
#'   lossyear_file <- lossyear_files[lossyear_ids == id]
#'
#'   treecover_2000 <- rast(treecover_file)
#'   forest_loss <- rast(lossyear_file)
#'
#'   if (!compareGeom(treecover_2000, forest_loss, stopOnError = FALSE)) {
#'     forest_loss <- resample(forest_loss, treecover_2000)
#'   }
#'
#'   year_2digit <- year %% 100
#'
#'   if (is.null(treecover_threshold)) {
#'     forest_loss_binary <- forest_loss > 0 & forest_loss <= year_2digit
#'   } else {
#'     forest_loss_binary <- (forest_loss > 0 & forest_loss <= year_2digit) & (treecover_2000 >= treecover_threshold)
#'   }
#'
#'   treecover_y <- treecover_2000 * (1 - forest_loss_binary)
#'
#'   if (!is.null(output_folder)) {
#'     if (!dir.exists(output_folder)) {
#'
#'       dir.create(output_folder, recursive = TRUE)
#'     }
#'     original_filename <- basename(treecover_file)
#'     filename_parts <- strsplit(original_filename, "_")[[1]]
#'     new_filename <- paste0(
#'       "Hansen_", filename_parts[2], "_treecover_calc_", year, "_",
#'       filename_parts[length(filename_parts)-1], "_", filename_parts[length(filename_parts)]
#'     )
#'     output_path <- file.path(output_folder, new_filename)
#'     writeRaster(treecover_y, output_path, overwrite=TRUE)
#'   }
#'
#'   return(treecover_y)
#' }

### Tests:
# treecover_2020 <- compute_treecover(2020)
# treecover_2020 <- compute_treecover(2020, treecover_threshold = 30, coords = "50N_010W", output_folder = "data/treecover_calc")
# treecover_2015 <- compute_treecover(year = 2015, num_cores = 20)



# library(testthat)
# library(sf)
#
# # Create a temporary directory for test files
# temp_dir <- tempdir()
#
# # Use tiles for tests by using function that download the required data:
# sample_plot <- plots[1, ]
#
# # Define a region of interest (ROI) for AFR7
# roi_afr7 <- st_polygon(list(rbind(c(24.53406, 0.9875556), c(24.54406, 0.9875556),
#                                   c(24.54406, 0.9975556), c(24.53406, 0.9975556),
#                                   c(24.53406, 0.9875556))))
# roi_sf_afr7 <- st_sfc(roi_afr7, crs = 4326)
#
# # Get data by running:
# Deforested(sample_plot,  map_year = 2015)
# sampleTreeCover(roi_sf_afr7, thresholds=10)
# # We should now have the following rasters in data/GFC:
# treecover_file <- "data/GFC/Hansen_GFC-2023-v1.11_treecover2000_10N_020E.tif"
# lossyear_file <-  "data/GFC/Hansen_GFC-2023-v1.11_lossyear_10N_020E.tif"
#
#
#
# test_that("process_pair function works correctly", {
#   id <- "GFC-2023-v1.11_10N_020E"
#   year <- 2015
#   treecover_threshold <- NULL
#   output_folder <- file.path(temp_dir, "output")
#   treecover_files <- c(treecover_file)
#   lossyear_files <- c(lossyear_file)
#   treecover_ids <- c("GFC-2023-v1.11_10N_020E")
#   lossyear_ids <- c("GFC-2023-v1.11_10N_020E")
#   baseline <- 2000
#
#   # Test basic functionality
#   result <- process_pair(id, year, treecover_threshold, output_folder,
#                          treecover_files, lossyear_files, treecover_ids, lossyear_ids, baseline)
#
#   expect_true(grepl("treecover_calc_2015_10N_020E.tif", result))
#   expect_true(file.exists(file.path(output_folder, "Hansen_GFC-2023-v1.11_treecover_calc_2015_10N_020E.tif")))
#   r_ras <- terra::rast(result)
#   expect_s4_class(r_ras, "SpatRaster")
#
#   # Test with treecover_threshold
#   result_threshold <- process_pair(id, year, treecover_threshold = 50, output_folder,
#                                    treecover_files, lossyear_files, treecover_ids, lossyear_ids, baseline)
#   rt_ras <- terra::rast(result_threshold)
#   expect_s4_class(rt_ras, "SpatRaster")
#   expect_true(grepl("STATISTICS_MINIMUM=50", describe(rt_ras)[53]))
#
#   # Test with baseline = 2010
#   result_baseline_2010 <- process_pair(id, year, treecover_threshold = NULL, output_folder,
#                                        treecover_files, lossyear_files, treecover_ids, lossyear_ids, baseline = 2010)
#   expect_true(grepl("GLAD_TCC-2010_treecover_calc_2015_10N_020E.tif", result_baseline_2010))
#   expect_true(file.exists(file.path(output_folder, "GLAD_TCC-2010_treecover_calc_2015_10N_020E.tif")))
#
#   # Test with non-matching extents (should resample lossyear raster)
#   # Create a dummy raster with different extent
#   dummy_raster <- rast(nrows = 10, ncols = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1, vals = 1)
#   dummy_file <- file.path(temp_dir, "dummy_lossyear.tif")
#   writeRaster(dummy_raster, dummy_file, overwrite = TRUE)
#   lossyear_files_dummy <- c(dummy_file)
#
#   result_resample <- process_pair(id, year, treecover_threshold = NULL, output_folder,
#                                   treecover_files, lossyear_files_dummy, treecover_ids, lossyear_ids, baseline)
#   expect_true(file.exists(result_resample))
#
#   # Test with missing treecover or lossyear files (should throw an error)
#   expect_error(
#     process_pair(id, year, treecover_threshold = NULL, output_folder,
#                  treecover_files = c(), lossyear_files, treecover_ids, lossyear_ids, baseline)
#   )
#   expect_error(
#     process_pair(id, year, treecover_threshold = NULL, output_folder,
#                  treecover_files, lossyear_files = c(), treecover_ids, lossyear_ids, baseline)
#   )
# })
#
#
# # Test for compute_treecover function
# test_that("compute_treecover function works correctly - 1 core", {
#   year <- 2015
#   gfc_folder <- "data/GFC"
#
#   # Test with default parameters
#   result <- compute_treecover(year, gfc_folder, coords = "10N_020E")
#   expect_type(result, "list")
#   expect_length(result, 1)
#   r_ras <- terra::rast(result[[1]])
#   expect_s4_class(r_ras, "SpatRaster")
#
#   # Test with treecover_threshold
#   result_threshold <- compute_treecover(year, gfc_folder, coords = "10N_020E", treecover_threshold = 50)
#   expect_type(result_threshold, "list")
#   expect_length(result_threshold, 1)
#   rt_ras <- terra::rast(result_threshold[[1]])
#   expect_s4_class(rt_ras, "SpatRaster")
#
#   # Test with coords
#   result_coords <- compute_treecover(year, gfc_folder, coords = "10N_020E")
#   expect_type(result_coords, "list")
#   expect_length(result_coords, 1)
#   rc_ras <- terra::rast(result_coords[[1]])
#   expect_s4_class(rc_ras, "SpatRaster")
#
#   # Test with output_folder
#   output_folder <- file.path(temp_dir, "output2")
#   result_output <- compute_treecover(year, gfc_folder, coords = "10N_020E", output_folder = output_folder)
#   expect_type(result_output, "list")
#   expect_length(result_output, 1)
#   rof_ras <- terra::rast(result_output[[1]])
#   expect_s4_class(rof_ras, "SpatRaster")
#   expect_true(file.exists(file.path(output_folder, "GLAD_TCC-2010_treecover_calc_2015_10N_020E.tif")))
#
#   # Test error for invalid year
#   expect_error(compute_treecover(2000, gfc_folder), "Year must be between 2001 and 2023.")
#
#   # Test error for non-existent coordinates
#   expect_error(compute_treecover(year, gfc_folder, coords = "91N_999E"), "No matching files found for the specified coordinates.")
# })
#
# test_that("compute_treecover function works correctly - multicore", {
#
#   skip_on_ci()
#   skip_on_cran()
#
#   year <- 2015
#   gfc_folder <- "data/GFC"
#
#   # Test with multiple cores
#   result_cores <- compute_treecover(year, gfc_folder, coords = "10N_020E", num_cores = 2)
#   expect_type(result_cores, "list")
#   expect_length(result_cores, 1)
#   r_ras <- terra::rast(result_cores[[1]])
#   expect_s4_class(r_ras, "SpatRaster")
#
# })
#
# # Clean up temporary directory
# unlink(temp_dir, recursive = TRUE)



### Benchmarks:
# The main gains with parallelism are when n_cores = nr of tiles to compute as seen below:

# library(microbenchmark)
#
## Two tiles
# compute_1core <- function() {
#   compute_treecover(year = 2015, num_cores = 1)
# }
#
# compute_2cores <- function() {
#   compute_treecover(year = 2015, num_cores = 2)
# }
#
# microbenchmark(
#   "1 core" = compute_1core(),
#   "2 cores" = compute_2cores(),
#   times = 1
# )
# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# 1 core 1406.839 1406.839 1406.839 1406.839 1406.839 1406.839     1
# 2 cores 1026.846 1026.846 1026.846 1026.846 1026.846 1026.846     1

## Single tile
# compute_1core <- function() {
#   compute_treecover(2020, treecover_threshold = 30, coords = "50N_010W", output_folder = "data/treecover_calc", num_cores = 1)
# }
#
# compute_20cores <- function() {
#   compute_treecover(2020, treecover_threshold = 30, coords = "50N_010W", output_folder = "data/treecover_calc", num_cores = 20)
# }
#
# microbenchmark(
#   "1 core" = compute_1core(),
#   "20 cores" = compute_20cores(),
#   times = 1
# )
# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# 1 core 1314.738 1314.738 1314.738 1314.738 1314.738 1314.738     1
# 20 cores 1083.504 1083.504 1083.504 1083.504 1083.504 1083.504     1
