## Updates made to the new framework:
# 07/01/2025:
# Used sf and terra packages for compatibility with both sf and SpatVector objects.
# Simplified the year and version selection logic in AGBtileNames and SDtileNames.
# Removed redundant code and improved overall readability.

## Notes:


#' Generate tree cover tile names
#'
#' This function generates file names for tree cover tiles based on a given polygon and year.
#'
#' @param pol An sf or SpatVector object representing the polygon of interest.
#' @param year Numeric, the year for which to generate tile names (2010, 2015, or 2020).
#'
#' @return A character vector of unique file names for tree cover tiles.
#'
#' @importFrom sf st_bbox
#' @importFrom terra ext xmin xmax ymin ymax
#'
#' @export
TCtileNames <- function(pol, year = 2010, treeCoverDir = "data/treecover2010_v3_100m") {
  if (inherits(pol, "SpatVector")) {
    bb <- terra::ext(pol)
    bb_vec <- c(terra::xmin(bb), terra::ymin(bb), terra::xmax(bb), terra::ymax(bb))
  } else if (inherits(pol, "sfc_POLYGON")) {
    bb_vec <- sf::st_bbox(pol)
  } else {
    stop("The object representing the polygon of interest must be of class SpatVector or sfc_POLYGON from terra and sf packages respectivelly.")
  }

  crds <- expand.grid(x = c(bb_vec[1], bb_vec[3]), y = c(bb_vec[2], bb_vec[4]))
  fnms <- character(nrow(crds))

  for (i in 1:nrow(crds)) {
    lon <- 10 * (crds$x[i] %/% 10)
    lat <- 10 * (crds$y[i] %/% 10) + 10
    LtX <- ifelse(lon < 0, "W", "E")
    LtY <- ifelse(lat < 0, "S", "N")
    WE <- paste0(sprintf('%03d', abs(lon)), LtX)
    NS <- paste0(sprintf('%02d', abs(lat)), LtY)

    # Adjust filename based on the year
    if(year == 2015) {
      fnms[i] <- file.path(treeCoverDir, paste0("treecover2015_", NS, "_", WE, ".tif"))
    } else if(year == 2020) {
      fnms[i] <- file.path(treeCoverDir, paste0("treecover2020_", NS, "_", WE, ".tif"))
    } else {
      # Default to 2010 if another year is provided
      fnms[i] <- file.path(treeCoverDir, paste0("treecover2010_", NS, "_", WE, ".tif"))
    }

  }

  unique(fnms)
}

#' Generate AGB tile names
#'
#' This function generates file names for AGB tiles based on a given polygon.
#'
#' @param pol An sf or SpatVector object representing the polygon of interest.
#'
#' @return A character vector of unique file names for AGB tiles.
#'
#' @import stringr
#' @importFrom sf st_bbox
#' @importFrom terra ext xmin xmax ymin ymax
#'
#' @export
AGBtileNames <- function(pol, agbTilesDir="data/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2020-fv5.0") {
  if (inherits(pol, "SpatVector")) {
    bb <- terra::ext(pol)
    bb_vec <- c(terra::xmin(bb), terra::ymin(bb), terra::xmax(bb), terra::ymax(bb))
  } else if (inherits(pol, "sfc_POLYGON")) {
    bb_vec <- sf::st_bbox(pol)
  } else {
    stop("The object representing the polygon of interest must be of class SpatVector or sfc_POLYGON from terra and sf packages respectivelly.")
  }

  crds <- expand.grid(x = c(bb_vec[1], bb_vec[3]), y = c(bb_vec[2], bb_vec[4]))
  fnms <- character(nrow(crds))

  for (i in 1:nrow(crds)) {
    lon <- 10 * (crds$x[i] %/% 10)
    lat <- 10 * (crds$y[i] %/% 10) + 10
    LtX <- ifelse(lon < 0, "W", "E")
    LtY <- ifelse(lat < 0, "S", "N")
    WE <- paste0(LtX, sprintf('%03d', abs(lon)))
    NS <- paste0(LtY, sprintf('%02d', abs(lat)))

    # year <- if (grepl('2017', agbTilesDir)) "2017" else if (grepl('2010', agbTilesDir)) "2010" else if (grepl('2018', agbTilesDir)) "2018" else "2020"
    # version <- if (year == "2020") "fv3.0" else "fv2.0"

    # Extract the fixed component of the filename
    agbfiles <- list.files(agbTilesDir)
    fixed_component <- str_extract(agbfiles[1], "_.*(?=$)")
    # Verify that this component is present in all files
    all_match <- all(str_detect(agbfiles, fixed_component))
    if (!all_match) {
      stop("The agbTilesDir folder contains data with several versions, levels and/or years. Keep each of those in separate folders and pass the required one to  agbTilesDir.")
    }

    #fnms[i] <- file.path(agbTilesDir, paste0(NS, WE, "_ESACCI-BIOMASS-L4-AGB-MERGED-100m-", year, "-", version, ".tif"))
    fnms[i] <- file.path(agbTilesDir, paste0(NS, WE, fixed_component))
  }

  unique(setdiff(fnms, grep("1000m|AGB_SD|aux", fnms, value = TRUE)))
}

#' Generate AGB SD tile names
#'
#' This function generates file names for AGB SD tiles based on a given polygon.
#'
#' @param pol An sf or SpatVector object representing the polygon of interest.
#'
#' @return A character vector of unique file names for AGB SD tiles.
#'
#' @importFrom sf st_bbox
#' @importFrom terra ext xmin xmax ymin ymax
#'
#' @export
SDtileNames <- function(pol, agbTilesDir="data/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2020-fv5.0") {
  if (inherits(pol, "SpatVector")) {
    bb <- terra::ext(pol)
    bb_vec <- c(terra::xmin(bb), terra::ymin(bb), terra::xmax(bb), terra::ymax(bb))
  } else {
    bb_vec <- sf::st_bbox(pol)
  }

  crds <- expand.grid(x = c(bb_vec[1], bb_vec[3]), y = c(bb_vec[2], bb_vec[4]))
  fnms <- character(nrow(crds))

  for (i in 1:nrow(crds)) {
    lon <- 10 * (crds$x[i] %/% 10)
    lat <- 10 * (crds$y[i] %/% 10) + 10
    LtX <- ifelse(lon < 0, "W", "E")
    LtY <- ifelse(lat < 0, "S", "N")
    WE <- paste0(LtX, sprintf('%03d', abs(lon)))
    NS <- paste0(LtY, sprintf('%02d', abs(lat)))

    year <- if (grepl('2017', agbTilesDir)) "2017" else if (grepl('2010', agbTilesDir)) "2010" else if (grepl('2018', agbTilesDir)) "2018" else "2020"
    version <- if (year == "2020") "fv3.0" else "fv2.0"

    fnms[i] <- file.path(agbTilesDir, paste0(NS, WE, "_ESACCI-BIOMASS-L4-AGB_SD-MERGED-100m-", year, "-", version, ".tif"))
  }

  unique(fnms)
}



#' Generate ESA-CCI AGB tile names
#'
#' This function generates file names for ESA-CCI AGB tiles based on a given polygon.
#'
#' @param pol An sf or SpatVector object representing the polygon of interest.
#' @param esacci_biomass_year The ESACCI BIOMASS AGB tiles year to use. Use either 2010, 2015, 2016, 2017, 2018, 2019,
#' 2020, 2021 or "latest" (default).
#' @param esacci_biomass_version The ESACCI BIOMASS AGB tiles version to use. Use either "v2.0", "v3.0", "v4.0",
#' "v5.0", "v5.01" or "latest" (default).
#'
#' @return A character vector of unique file names for ESA-CCI AGB tiles.
#'
#' @import stringr
#' @importFrom sf st_bbox
#' @importFrom terra ext xmin xmax ymin ymax
#'
#' @export
ESACCIAGBtileNames <- function(pol,
                               esacci_biomass_year = "latest",
                               esacci_biomass_version = "latest") {

  esacci_args <- validate_esacci_biomass_args(esacci_biomass_year, esacci_biomass_version)
  esacci_biomass_year <- esacci_args$esacci_biomass_year
  esacci_biomass_version <- esacci_args$esacci_biomass_version

  if (inherits(pol, "SpatVector")) {
    bb <- terra::ext(pol)
    bb_vec <- c(terra::xmin(bb), terra::ymin(bb), terra::xmax(bb), terra::ymax(bb))
  } else if (inherits(pol, "sfc_POLYGON")) {
    bb_vec <- sf::st_bbox(pol)
  } else {
    stop("The object representing the polygon of interest must be of class SpatVector or sfc_POLYGON from terra and sf packages respectivelly.")
  }

  crds <- expand.grid(x = c(bb_vec[1], bb_vec[3]), y = c(bb_vec[2], bb_vec[4]))
  fnms <- character(nrow(crds))

  for (i in 1:nrow(crds)) {
    lon <- 10 * (crds$x[i] %/% 10)
    lat <- 10 * (crds$y[i] %/% 10) + 10
    LtX <- ifelse(lon < 0, "W", "E")
    LtY <- ifelse(lat < 0, "S", "N")
    WE <- paste0(LtX, sprintf('%03d', abs(lon)))
    NS <- paste0(LtY, sprintf('%02d', abs(lat)))

    if (esacci_biomass_version == "v5.01") {
      esacci_biomass_version <- "v5.0"
    }

    fnms[i] <- paste0(NS, WE, "_ESACCI-BIOMASS-L4-AGB-MERGED-100m-", esacci_biomass_year, "-f", esacci_biomass_version, ".tif")
  }
  unique(setdiff(fnms, grep("1000m|AGB_SD|aux", fnms, value = TRUE)))
}




# Up to four treecover (TC) tile names covered by pol
#
# old_TCtileNames <- function(pol, year) {
#
#   treeCoverDir <- file.path("data", "treecover2010_v3_100m")
#
#   # Assuming 'bb' extraction has been corrected for 'terra' objects
#   bb <- ext(pol)  # Use 'terra::ext' for SpatVector objects
#   bb_vec <- c(xmin(bb), ymin(bb), xmax(bb), ymax(bb))
#
#   # Generate combinations of the bounding box corners
#   crds <- expand.grid(x = c(bb_vec[1], bb_vec[3]), y = c(bb_vec[2], bb_vec[4]))
#
#   fnms <- character(length(crds$x))  # Initialize with the correct length based on 'crds'
#   result
#   for(i in 1:nrow(crds)) {
#     lon <- 10 * (crds$x[i] %/% 10)
#     lat <- 10 * (crds$y[i] %/% 10) + 10
#     LtX <- ifelse(lon < 0, "W", "E")
#     LtY <- ifelse(lat < 0, "S", "N")
#     WE <- paste0(sprintf('%03d', abs(lon)), LtX)
#     NS <- paste0(sprintf('%02d', abs(lat)), LtY)
#
#     # Adjust filename based on the year
#     if(year == 2015) {
#       fnms[i] <- file.path(treeCoverDir, paste0("treecover2015_", NS, "_", WE, ".tif"))
#     } else if(year == 2020) {
#       fnms[i] <- file.path(treeCoverDir, paste0("treecover2020_", NS, "_", WE, ".tif"))
#     } else {
#       # Default to 2010 if another year is provided
#       fnms[i] <- file.path(treeCoverDir, paste0("treecover2010_", NS, "_", WE, ".tif"))
#     }
#   }
#
#   return(unique(fnms))
# }
#
# old_AGBtileNames <- function(pol){
#
#   agbTilesDir <- file.path("data", "ESACCI-BIOMASS-L4-AGB-MERGED-100m-2020-fv5.0")
#
#   bb <- unname(bbox(pol))
#   crds <- expand.grid(x=bb[1,],y=bb[2,])
#   fnms <- character(6)
#   match <- c('1000m', 'AGB_SD', 'aux')
#
#   for(i in 1:nrow(crds)){
#
#     if (grepl('2017', agbTilesDir) == TRUE){
#       lon <- 10*(crds[i,1]%/%10)
#       lat <- 10*(crds[i,2]%/%10) + 10
#       LtX <- ifelse(lon < 0, "W", "E")
#       LtY <- ifelse(lat < 0, "S", "N")
#       WE <- paste0(LtX, sprintf('%03d',abs(lon)))
#       NS <- paste0(LtY,sprintf('%02d',abs(lat)))
#       fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2017-fv2.0.tif"))
#
#     }else if (grepl('2010', agbTilesDir) == TRUE){
#       lon <- 10*(crds[i,1]%/%10)
#       lat <- 10*(crds[i,2]%/%10) + 10
#       LtX <- ifelse(lon < 0, "W", "E")
#       LtY <- ifelse(lat < 0, "S", "N")
#       WE <- paste0(LtX, sprintf('%03d',abs(lon)))
#       NS <- paste0(LtY,sprintf('%02d',abs(lat)))
#       fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2010-fv2.0.tif"))
#     }else if (grepl('2018', agbTilesDir) == TRUE){
#       lon <- 10*(crds[i,1]%/%10)
#       lat <- 10*(crds[i,2]%/%10) + 10
#       LtX <- ifelse(lon < 0, "W", "E")
#       LtY <- ifelse(lat < 0, "S", "N")
#       WE <- paste0(LtX, sprintf('%03d',abs(lon)))
#       NS <- paste0(LtY,sprintf('%02d',abs(lat)))
#       fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2018-fv2.0.tif"))
#     }else{
#       lon <- 10*(crds[i,1]%/%10)
#       lat <- 10*(crds[i,2]%/%10) + 10
#       LtX <- ifelse(lon < 0, "W", "E")
#       LtY <- ifelse(lat < 0, "S", "N")
#       WE <- paste0(LtX, sprintf('%03d',abs(lon)))
#       NS <- paste0(LtY,sprintf('%02d',abs(lat)))
#       fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2020-fv3.0.tif"))
#     }
#   }
#   fnms0 <- unique(grep(paste(match,collapse="|"), fnms, value=T))
#   fnms <- setdiff(fnms,fnms0)
#   unique(fnms)
# }
#
# old_SDtileNames <- function(pol){
#
#   agbTilesDir <- file.path("data", "ESACCI-BIOMASS-L4-AGB-MERGED-100m-2020-fv5.0")
#
#   bb <- unname(bbox(pol))
#   crds <- expand.grid(x=bb[1,],y=bb[2,])
#   fnms <- character(6)
#   match <- '_SD'
#
#   for(i in 1:nrow(crds)){
#
#     if (grepl('2017', agbTilesDir) == TRUE){
#       lon <- 10*(crds[i,1]%/%10)
#       lat <- 10*(crds[i,2]%/%10) + 10
#       LtX <- ifelse(lon < 0, "W", "E")
#       LtY <- ifelse(lat < 0, "S", "N")
#       WE <- paste0(LtX, sprintf('%03d',abs(lon)))
#       NS <- paste0(LtY,sprintf('%02d',abs(lat)))
#       fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB_SD-MERGED-100m-2017-fv2.0.tif"))
#
#     }else if (grepl('2010', agbTilesDir) == TRUE){
#       lon <- 10*(crds[i,1]%/%10)
#       lat <- 10*(crds[i,2]%/%10) + 10
#       LtX <- ifelse(lon < 0, "W", "E")
#       LtY <- ifelse(lat < 0, "S", "N")
#       WE <- paste0(LtX, sprintf('%03d',abs(lon)))
#       NS <- paste0(LtY,sprintf('%02d',abs(lat)))
#       fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB_SD-MERGED-100m-2010-fv2.0.tif"))
#     }else if (grepl('2018', agbTilesDir) == TRUE){
#       lon <- 10*(crds[i,1]%/%10)
#       lat <- 10*(crds[i,2]%/%10) + 10
#       LtX <- ifelse(lon < 0, "W", "E")
#       LtY <- ifelse(lat < 0, "S", "N")
#       WE <- paste0(LtX, sprintf('%03d',abs(lon)))
#       NS <- paste0(LtY,sprintf('%02d',abs(lat)))
#       fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB_SD-MERGED-100m-2018-fv2.0.tif"))
#     }else{
#       lon <- 10*(crds[i,1]%/%10)
#       lat <- 10*(crds[i,2]%/%10) + 10
#       LtX <- ifelse(lon < 0, "W", "E")
#       LtY <- ifelse(lat < 0, "S", "N")
#       WE <- paste0(LtX, sprintf('%03d',abs(lon)))
#       NS <- paste0(LtY,sprintf('%02d',abs(lat)))
#       fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB_SD-MERGED-100m-2020-fv3.0.tif"))
#     }
#   }
#   fnms0 <- unique(grep(paste(match,collapse="|"), fnms, value=T))
#   unique(fnms)
# }


# Tests
#
# library(testthat)
# library(sf)
# library(terra)
#
# # Test comparison between old and new versions
# test_that("Old and new TCtileNames functions produce consistent results", {
#   # Create a sample polygon
#   pol <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
#   pol_sf <- st_sfc(pol, crs = 4326)
#   pol_sv <- vect(pol_sf)
#
#   year <- c(2010, 2015, 2017, 2020)
#
#   for (year in year) {
#     old_result <- old_TCtileNames(pol_sv, year)
#     new_result <- TCtileNames(pol_sf, year)
#     expect_equal(old_result, new_result)
#
#     new_result_sv <- TCtileNames(pol_sv, year)
#     expect_equal(old_result, new_result_sv)
#   }
#
#
# })
#
# test_that("Old and new AGBtileNames functions produce consistent results", {
#
#   skip("Not run because old_AGBtileNames has broken sp dependecies.")
#
#   pol <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
#   pol_sf <- st_sfc(pol, crs = 4326)
#   pol_sv <- vect(pol_sf)
#
#   old_result <- old_AGBtileNames(pol_sv)
#   new_result <- AGBtileNames(pol_sf)
#   expect_equal(old_result, new_result)
#
#   new_result_sv <- AGBtileNames(pol_sv)
#   expect_equal(old_result, new_result_sv)
# })
#
# test_that("Old and new SDtileNames functions produce consistent results", {
#
#   skip("Not run because old_SDtileNames has broken sp dependecies.")
#
#   pol <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
#   pol_sf <- st_sfc(pol, crs = 4326)
#   pol_sv <- vect(pol_sf)
#
#   old_result <- old_SDtileNames(pol_sf)
#   new_result <- SDtileNames(pol_sf)
#   expect_equal(old_result, new_result)
#
#   new_result_sv <- SDtileNames(pol_sv)
#   expect_equal(old_result, new_result_sv)
# })
#
# # Test internal consistency
# test_that("TCtileNames function behaves consistently", {
#   pol <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
#   pol_sf <- st_sfc(pol, crs = 4326)
#
#   result_2010 <- TCtileNames(pol_sf, 2010)
#   result_2015 <- TCtileNames(pol_sf, 2015)
#   result_2020 <- TCtileNames(pol_sf, 2020)
#   result_2014 <- TCtileNames(pol_sf, 2014)
#
#   expect_true(all(grepl("treecover2010", result_2010)))
#   expect_true(all(grepl("treecover2015", result_2015)))
#   expect_true(all(grepl("treecover2020", result_2020)))
#   expect_true(all(grepl("treecover2010", result_2014)))
# })
#
# test_that("AGBtileNames function behaves consistently", {
#   pol <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
#   pol_sf <- st_sfc(pol, crs = 4326)
#
#   result <- AGBtileNames(pol_sf)
#
#   expect_true(all(grepl("ESACCI-BIOMASS-L4-AGB-MERGED-100m-2020", result)))
#   expect_false(any(grepl("1000m|AGB_SD|aux", result)))
# })
#
# test_that("SDtileNames function behaves consistently", {
#   pol <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
#   pol_sf <- st_sfc(pol, crs = 4326)
#
#   result <- SDtileNames(pol_sf)
#
#   expect_true(all(grepl("ESACCI-BIOMASS-L4-AGB_SD-MERGED-100m-2020", result)))
# })
#
#
# test_that("ESACCIAGBtileNames function behaves consistently", {
#
#   # Amazon, 1 tile
#   amazon_pol <- st_polygon(list(rbind(c(-62.2159, -3.4653), c(-62.2059, -3.4653),
#                                       c(-62.2059, -3.4553), c(-62.2159, -3.4553),
#                                       c(-62.2159, -3.4653))))
#   amazon_pol_sf <- st_sfc(amazon_pol, crs = 4326)
#
#   # Mexico, 2 tiles
#   mexico_pol <- st_polygon(list(rbind(c(-99.5,18), c(-101,19), c(-101,19), c(-99.5,19), c(-99.5,18))))
#   mexico_pol_sf <- st_sfc(mexico_pol, crs = 4326)
#
#   result_amazon1 <- ESACCIAGBtileNames(amazon_pol_sf, esacci_biomass_year=2021, esacci_biomass_version="v5.01")
#   result_mexico1 <- ESACCIAGBtileNames(mexico_pol_sf, esacci_biomass_year=2021, esacci_biomass_version="v5.01")
#   expect_equal(result_amazon1, "N00W070_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2021-fv5.0.tif")
#   expect_equal(result_mexico1, c(
#     "N20W110_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2021-fv5.0.tif",
#     "N20W100_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2021-fv5.0.tif"
#   ))
#
#   result_amazon2 <- ESACCIAGBtileNames(amazon_pol_sf, esacci_biomass_year=2021, esacci_biomass_version="v5.0")
#   expect_equal(result_amazon1, result_amazon2)
#
#   expect_error(ESACCIAGBtileNames(amazon_pol_sf, esacci_biomass_year=2021, esacci_biomass_version="v2.0"))
#
# })


