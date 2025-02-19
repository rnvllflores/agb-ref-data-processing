## Updates made to the new framework:
# 06/01/2025:
# Replaced filter() and left_join() with dplyr::filter() and dplyr::left_join() to avoid potential namespace conflicts.
# Improved error handling and messages.
# 28/01/2025:
# Replaced and renamed arguments and introduced arguments checks.
# Added gez = "all" input option and checks.
# using dplyr::bind_rows instead of rbind to make sure that we don't get an error in case static case has rows.



#' Apply temporal adjustment to plot biomass
#'
#' This function adjusts plot biomass values to align with the map year by adding or subtracting
#' annual growth increment to older or newer plots. It uses growth data from a model-based
#' estimate of growth-age relationships.
#'
#' @inheritParams Deforested
#' @param map_year Numeric value indicating the AGB map year.
#' @param gez Character string specifying the Global Ecological Zones (GEZ) of interest to apply. If "all" (default),
#'  all GEZ in plt will be calculated.
#'
#' @return A data frame with adjusted AGB values.
#'
#' @importFrom dplyr filter left_join
#' @importFrom utils read.csv
#'
#' @export
#' @examples
#' set.seed(42)
#' sample_plots <- plots[sample(nrow(plots), 10), ]
#' sample_plots <- BiomePair(sample_plots)
#' resultApply <- TempApply(sample_plots, 2004)
#' head(resultApply)
#' resultVar <- TempVar(sample_plots, 2004)
#' head(resultVar)
TempApply <- function(plt, map_year, gez = "all") {

  plt <- check_and_convert_plt(plt, ez=TRUE)

  # Read growth rate data and enforce data structure:
  gr <- read.csv(system.file(file.path("data", "GR_Uniques.csv"), package = "Plot2Map"))
  gr$GEZ <- as.character(gr$GEZ)
  gr$ZONE <- as.character(gr$ZONE)
  gr$FAO.ecozone <- as.character(gr$FAO.ecozone)

  plt$AVG_YEAR <- as.numeric(plt$AVG_YEAR)

  # Check that there is GR data for the 'GEZ', 'ZONE', 'FAO.ecozone' combinations in plt:
  if (gez == "all") {
    # Perform a left join to check for missing combinations
    unmatched <- dplyr::anti_join(plt, gr, by = c("GEZ", "ZONE", "FAO.ecozone"))

    if (nrow(unmatched) > 0) {
      stop("The function misses Growth Rate data for the following GEZ, ZONE, and FAO.ecozone combinations (try to remove the indicated combination(s) from the input plt object or don't use the gez = \"all\" option):\n",
           paste0(unique(unmatched$GEZ), ", ", unique(unmatched$ZONE), ", ", unique(unmatched$FAO.ecozone), collapse = "\n"))
    }
  }

  # Filter eco-region first
  if (gez %in% unique(plt$GEZ) | gez == "all") {

    if (gez == "all") {
      plt0 <- plt
    } else {
      plt0 <- dplyr::filter(plt, GEZ == gez)
    }

    # String check
    plt0$ZONE <- ifelse(plt0$ZONE == 'Northern America', 'N.America', plt0$ZONE)
    plt0$ZONE <- ifelse(plt0$ZONE == 'Southern America', 'S.America', plt0$ZONE)
    plt0$ZONE <- ifelse(plt0$ZONE == 'Central America', 'C.America', plt0$ZONE)

    # Join growth rate table using 3 variables to assure uniqueness
    plt.old <- dplyr::left_join(plt0, gr, by = c('GEZ', 'ZONE', 'FAO.ecozone'))

    # Filter above and below map year (i.e. 2010 for GlobBiomass), keep no changes to map year
    below <- subset(plt.old, AVG_YEAR < map_year) #non-NAs
    above <- subset(plt.old, AVG_YEAR > map_year) #non-NAs
    static <- subset(plt.old, is.na(AVG_YEAR) | AVG_YEAR == map_year) #NAs AVG_YEAR OR 2010 subsets

    # Apply growth rates (GR1 = primary, GR2 = old secondary, GR3 = young secondary)
    below$AGB_ORIG <- below$AGB_T_HA
    above$AGB_ORIG <- above$AGB_T_HA
    below$AGB_T_HA <- below$AGB_T_HA + (ifelse(below$AGB_T_HA < 100, below$GR3, below$GR2) * (map_year - below$AVG_YEAR))
    above$AGB_T_HA <- above$AGB_T_HA - (ifelse(above$AGB_T_HA < 100, above$GR3, above$GR2) * (above$AVG_YEAR - map_year))

    below$AGB_T_HA <- ifelse(below$AGB_T_HA > 152,
                             below$AGB_ORIG + (below$GR1 * (map_year - below$AVG_YEAR)), below$AGB_T_HA)

    above$AGB_T_HA <- ifelse(above$AGB_T_HA > 152,
                             above$AGB_ORIG - (above$GR1 * (above$AVG_YEAR - map_year)), above$AGB_T_HA) #retain if not in primary/GR3 class

    above$AGB_T_HA <- ifelse(above$AGB_T_HA < 0,
                             above$AGB_ORIG, above$AGB_T_HA) #retain original if it gets negative

    # Combine all: static and recomputed
    static$AGB_ORIG <- static$AGB_T_HA

    plt.new <- dplyr::bind_rows(below, above, static)
    plt.new$AGB_T_HA_ORIG <- plt.new$AGB_ORIG

    # Checker of rows
    if (sum(nrow(plt.old)) == sum(nrow(plt.new))) {
      message('Growth rates applied correspondingly per eco-region.')
    } else {
      stop('Something is wrong, nr of dataframe rows different before/after applying growth rates.')
    }

    #remove last joined growth rates columns for further row binding
    remove <- c('GR1', 'GR2', 'GR3', 'AGB_ORIG')
    plt.new <- plt.new[, !(names(plt.new) %in% remove)]

    plt.new$AGB_T_HA <- ifelse(is.na(plt.new$AGB_T_HA), plt$AGB_T_HA, plt.new$AGB_T_HA)
    return(plt.new)
  } #this part doesn't give NAs already
}


#' Calculate temporal variance in plot biomass
#'
#' This function calculates the temporal variance in plot biomass based on growth rate
#' standard deviations. It computes the standard deviation
#' of growth for each plot.
#'
#' @inheritParams TempApply
#'
#' @return A data frame with calculated growth standard deviations.
#'
#' @importFrom dplyr filter left_join
#' @importFrom utils read.csv
#'
#' @export
#' @examples
#' set.seed(42)
#' sample_plots <- plots[sample(nrow(plots), 10), ]
#' sample_plots <- BiomePair(sample_plots)
#' resultApply <- TempApply(sample_plots, 2004)
#' head(resultApply)
#' resultVar <- TempVar(sample_plots, 2004)
#' head(resultVar)
TempVar <- function(plt, map_year, gez = "all") {

  plt <- check_and_convert_plt(plt, ez=TRUE)

  # Read growth rate data and enforce data structure:
  gr <- read.csv(system.file(file.path("data", "GR_SD.csv"), package = "Plot2Map"))
  gr$GEZ <- as.character(gr$GEZ)
  gr$ZONE <- as.character(gr$ZONE)
  gr$FAO.ecozone <- as.character(gr$FAO.ecozone)

  plt$AVG_YEAR <- as.numeric(plt$AVG_YEAR)

  # Check that there is GR data for the 'GEZ', 'ZONE', 'FAO.ecozone' combinations in plt:
  if (gez == "all") {
    # Perform a left join to check for missing combinations
    unmatched <- dplyr::anti_join(plt, gr, by = c("GEZ", "ZONE", "FAO.ecozone"))

    if (nrow(unmatched) > 0) {
      stop("The function misses Growth Rate data for the following GEZ, ZONE, and FAO.ecozone combinations (try to remove the indicated combination(s) from the input plt object or don't use the gez = \"all\" option):\n",
           paste0(unique(unmatched$GEZ), ", ", unique(unmatched$ZONE), ", ", unique(unmatched$FAO.ecozone), collapse = "\n"))
    }
  }

  # Filter eco-region first
  if (gez %in% unique(plt$GEZ) | gez == "all") {

    if (gez == "all") {
      plt0 <- plt
    } else {
      plt0 <- dplyr::filter(plt, GEZ == gez)
    }

    plt0$ZONE <- ifelse(plt0$ZONE == 'Northern America', 'N.America', plt0$ZONE)
    plt0$ZONE <- ifelse(plt0$ZONE == 'Southern America', 'S.America', plt0$ZONE)
    plt0$ZONE <- ifelse(plt0$ZONE == 'Central America', 'C.America', plt0$ZONE)

    # Join growth rate table using 3 variables to assure uniqueness
    plt.old <- dplyr::left_join(plt0, gr, by = c('GEZ', 'ZONE', 'FAO.ecozone'))

    # Filter above and below map year (i.e. 2010 for GlobBiomass), keep no changes to map year
    below <- subset(plt.old, AVG_YEAR < map_year) #non-NAs
    above <- subset(plt.old, AVG_YEAR > map_year) #non-NAs
    static <- subset(plt.old, is.na(AVG_YEAR) | AVG_YEAR == map_year) #NAs AVG_YEAR OR 2010 subsets

    # Apply growth rates (SD1 = primary, SD2 = old secondary, SD3 = young secondary)
    below$AGB_T_HA_ORIG <- below$AGB_T_HA
    above$AGB_T_HA_ORIG <- above$AGB_T_HA
    below$AGB_T_HA <- below$AGB_T_HA_ORIG + (ifelse(below$AGB_T_HA_ORIG < 100, below$SD3, below$SD2) * (map_year - below$AVG_YEAR))
    above$AGB_T_HA <- above$AGB_T_HA_ORIG - (ifelse(above$AGB_T_HA_ORIG < 100, above$SD3, above$SD2) * (above$AVG_YEAR -  map_year))

    below$AGB_T_HA <- ifelse(below$AGB_T_HA > 152,
                             below$AGB_T_HA_ORIG + (below$SD1 * (map_year - below$AVG_YEAR)), below$AGB_T_HA)

    above$AGB_T_HA <- ifelse(above$AGB_T_HA > 152,
                             above$AGB_T_HA_ORIG - (above$SD1 * (above$AVG_YEAR - map_year)), above$AGB_T_HA) #retain if not in primary/SD3 class

    above$AGB_T_HA <- ifelse(above$AGB_T_HA < 0,
                             above$AGB_T_HA_ORIG, above$AGB_T_HA) #retain original if it gets negative

    #combine all: static and recomputed
    plt.new <- dplyr::bind_rows(below, above, static)

    # Checker of rows
    if (sum(nrow(plt.old)) == sum(nrow(plt.new))) {
      message('Growth rates applied correspondingly per eco-region.')
    } else {
      stop('Something is wrong, nr of dataframe rows different before/after applying growth rates.')
    }

    # Retain last joined growth rates columns for further row binding
    plt.new$sdGrowth <- abs(plt.new$AGB_T_HA - plt.new$AGB_T_HA_ORIG)
    retain <- c(names(plt0), 'sdGrowth')
    plt.new <- plt.new[, (names(plt.new) %in% retain)]
    plt.new$AGB_T_HA <- plt.old$AGB_T_HA
    plt.new$sdGrowth <- ifelse(is.na(plt.new$sdGrowth), mean(plt.new$sdGrowth, na.rm = TRUE), plt.new$sdGrowth)
    plt.new$sdGrowth <- ifelse(is.nan(plt.new$sdGrowth), mean(plt.new$sdGrowth, na.rm = TRUE), plt.new$sdGrowth)
    return(plt.new)
  }
}



#
# old_TempApply <- function(df, domain, year){
#   dataDir <- "data"
#   gr <- read.csv(paste0(dataDir,'/GR_Uniques.csv'))
#   gr$GEZ <- as.character(gr$GEZ)
#   gr$ZONE <- as.character(gr$ZONE)
#   gr$FAO.ecozone <- as.character(gr$FAO.ecozone)
#
#   df$AVG_YEAR <- as.numeric(df$AVG_YEAR)
#
#   #filter +- 10 years older or newer plots to the map year
#   #df <- subset(df, df$AVG_YEAR < year+11
#   #        & df$AVG_YEAR > year-11)
#
#
#   #filter eco-region first
#   if (domain %in% unique(df$GEZ)==T){
#     df0 <- dplyr::filter (df, GEZ == domain)
#
#     #string check
#     df0$ZONE <- ifelse(df0$ZONE == 'Northern America', 'N.America', df0$ZONE)
#     df0$ZONE <- ifelse(df0$ZONE == 'Southern America', 'S.America', df0$ZONE)
#     df0$ZONE <- ifelse(df0$ZONE == 'Central America', 'C.America', df0$ZONE)
#
#     #join growth rate table using 3 variables to assure uniqueness
#     df.old <- dplyr::left_join(df0, gr, by = c('GEZ'='GEZ', 'ZONE'='ZONE', 'FAO.ecozone'='FAO.ecozone'))
#
#     #filter above and below map year (i.e. 2010 for GlobBiomass), keep no changes to map year
#     below <- subset(df.old, AVG_YEAR < year) #non-NAs
#     above <- subset(df.old, AVG_YEAR > year) #non-NAs
#     static <- subset(df.old, is.na(AVG_YEAR) | AVG_YEAR == year) #NAs AVG_YEAR OR 2010 subsets
#
#
#     #apply growth rates (GR1 = primary, GR2 = old secondary, GR3 = young secondary)
#     below$AGB_ORIG <- below$AGB_T_HA
#     above$AGB_ORIG <- above$AGB_T_HA
#     below$AGB_T_HA <- below$AGB_T_HA + (ifelse(below$AGB_T_HA < 100, below$GR3, below$GR2) * (year - below$AVG_YEAR))
#     above$AGB_T_HA <- above$AGB_T_HA - (ifelse(above$AGB_T_HA < 100, above$GR3, above$GR2) * (above$AVG_YEAR -  year))
#
#     below$AGB_T_HA <- ifelse(below$AGB_T_HA > 152,
#                              below$AGB_ORIG + (below$GR1 * (year - below$AVG_YEAR)), below$AGB_T_HA)
#
#     above$AGB_T_HA <- ifelse(above$AGB_T_HA > 152,
#                              above$AGB_ORIG - (above$GR1 * (above$AVG_YEAR - year)), above$AGB_T_HA) #retain if not in primary/GR3 class
#
#     above$AGB_T_HA <- ifelse(above$AGB_T_HA < 0,
#                              above$AGB_ORIG, above$AGB_T_HA) #retain original if it gets negative
#
#
#     #combine all: static and recomputed
#     static$AGB_ORIG <- static$AGB_T_HA
#
#     df.new <- rbind(below,above,static)
#     df.new$AGB_T_HA_ORIG <- df.new$AGB_ORIG
#
#     #checker of rows
#     if (sum(nrow(df.old)) == sum(nrow(df.new))) {
#       print('growth rates applied correspondingly per eco-region!')
#     } else {
#       print('something is wrong..row sums not equal..')
#     }
#
#
#     #remove last joined growth rates columns for further row binding
#     remove <- c('GR1', 'GR2', 'GR3', 'AGB_ORIG')
#     df.new <- df.new[ , !(names(df.new) %in% remove)]
#
#     df.new$AGB_T_HA <- ifelse(is.na(df.new$AGB_T_HA),df$AGB_T_HA, df.new$AGB_T_HA)
#     return(df.new)
#   } #this part doesn't give NAs already
#
# }
#
#
#
#
# old_TempVar <- function(df, domain, year){
#   dataDir <- "data"
#   gr <- read.csv(paste0(dataDir,'/GR_SD.csv'))
#   gr$GEZ <- as.character(gr$GEZ)
#   gr$ZONE <- as.character(gr$ZONE)
#   gr$FAO.ecozone <- as.character(gr$FAO.ecozone)
#
#   #filter eco-region first
#   if (domain %in% unique(df$GEZ)==T){
#     df0 <- dplyr::filter(df, GEZ == domain) #this part doesn't give NAs already
#     df0$ZONE <- ifelse(df0$ZONE == 'Northern America', 'N.America', df0$ZONE)
#     df0$ZONE <- ifelse(df0$ZONE == 'Southern America', 'S.America', df0$ZONE)
#     df0$ZONE <- ifelse(df0$ZONE == 'Central America', 'C.America', df0$ZONE)
#
#     #join growth rate table using 3 variables to assure uniqueness
#     df.old <- dplyr::left_join(df0, gr, by = c('GEZ'='GEZ', 'ZONE'='ZONE', 'FAO.ecozone'='FAO.ecozone'))
#
#     #filter above and below map year (i.e. 2010 for GlobBiomass), keep no changes to map year
#     below <- subset(df.old, AVG_YEAR < year) #non-NAs
#     above <- subset(df.old, AVG_YEAR > year) #non-NAs
#     static <- subset(df.old, is.na(AVG_YEAR) | AVG_YEAR == year) #NAs AVG_YEAR OR 2010 subsets
#
#
#     #apply growth rates (SD1 = primary, SD2 = old secondary, SD3 = young secondary)
#
#     below$AGB_T_HA <- below$AGB_T_HA_ORIG + (ifelse(below$AGB_T_HA_ORIG < 100, below$SD3, below$SD2) * (year - below$AVG_YEAR))
#     above$AGB_T_HA <- above$AGB_T_HA_ORIG - (ifelse(above$AGB_T_HA_ORIG < 100, above$SD3, above$SD2) * (above$AVG_YEAR -  year))
#
#     below$AGB_T_HA <- ifelse(below$AGB_T_HA > 152,
#                              below$AGB_T_HA_ORIG + (below$SD1 * (year - below$AVG_YEAR)), below$AGB_T_HA)
#
#     above$AGB_T_HA <- ifelse(above$AGB_T_HA > 152,
#                              above$AGB_T_HA_ORIG - (above$SD1 * (above$AVG_YEAR - year)), above$AGB_T_HA) #retain if not in primary/SD3 class
#
#     above$AGB_T_HA <- ifelse(above$AGB_T_HA < 0,
#                              above$AGB_T_HA_ORIG, above$AGB_T_HA) #retain original if it gets negative
#
#
#     #combine all: static and recomputed
#     df.new <- rbind(below,above,static)
#
#
#     #checker of rows
#     if (sum(nrow(df.old)) == sum(nrow(df.new))) {
#       print('growth rates applied correspondingly per eco-region!')
#     } else {
#       print('something is wrong..row sums not equal..')
#     }
#
#     #retain last joined growth rates columns for further row binding
#     df.new$sdGrowth <-  abs(df.new$AGB_T_HA - df.new$AGB_T_HA_ORIG)
#     retain <- c(names(df0), 'sdGrowth')
#     df.new <- df.new[ , (names(df.new) %in% retain)]
#     df.new$AGB_T_HA <-  df.old$AGB_T_HA
#     df.new$sdGrowth <- ifelse(is.na(df.new$sdGrowth), mean(df.new$sdGrowth,na.rm=T),df.new$sdGrowth)
#     df.new$sdGrowth <- ifelse(is.nan(df.new$sdGrowth), mean(df.new$sdGrowth,na.rm=T),df.new$sdGrowth)
#     df.new}
# }


# # Tests:
# library(testthat)
#
# # Test comparison between old and new versions
# test_that("Old and new TempApply functions produce consistent results", {
#   # Create sample data
#   test_data <- data.frame(
#     GEZ = c("Tropical", "Tropical", "Temperate"),
#     ZONE = c("Africa", "S.America", "Europe"),
#     FAO.ecozone = c("Tropical rainforest", "Tropical moist forest", "Temperate oceanic forest"),
#     AVG_YEAR = c(2005, 2015, 2010),
#     AGB_T_HA = c(200, 150, 100)
#   )
#
#   # Run both versions
#   old_result <- old_TempApply(test_data, "Tropical", 2010)
#   new_result <- TempApply(test_data, "Tropical", 2010)
#
#   # Compare results
#   expect_equal(old_result$AGB_T_HA, new_result$AGB_T_HA, tolerance = 1e-6)
#   expect_equal(old_result$AGB_T_HA_ORIG, new_result$AGB_T_HA_ORIG, tolerance = 1e-6)
# })
#
# test_that("Old and new TempVar functions produce consistent results", {
#   # Create sample data
#   test_data <- data.frame(
#     GEZ = c("Tropical", "Tropical", "Temperate"),
#     ZONE = c("Africa", "S.America", "Europe"),
#     FAO.ecozone = c("Tropical rainforest", "Tropical moist forest", "Temperate oceanic forest"),
#     AVG_YEAR = c(2005, 2015, 2010),
#     AGB_T_HA = c(200, 150, 100),
#     AGB_T_HA_ORIG = c(190, 160, 100)
#   )
#
#   # Run both versions
#   old_result <- old_TempVar(test_data, "Tropical", 2010)
#   new_result <- TempVar(test_data, "Tropical", 2010)
#
#   # Compare results
#   expect_equal(old_result$AGB_T_HA, new_result$AGB_T_HA, tolerance = 1e-6)
#   expect_equal(old_result$sdGrowth, new_result$sdGrowth, tolerance = 1e-6)
# })
#
# # Test internal consistency
# test_that("TempApply function behaves consistently", {
#
#   set.seed(123)
#   test_data <- plots[sample(nrow(plots), 10), ]
#   test_data <- BiomePair(test_data)
#
#   result <- TempApply(test_data, 2004)
#
#   # Check output structure
#   expect_s3_class(result, "data.frame")
#   expect_true(all(c("AGB_T_HA", "AGB_T_HA_ORIG") %in% names(result)))
#
#   # Check for expected adjustments
#   expect_true(all(result$AGB_T_HA[result$AVG_YEAR < 2004] >= result$AGB_T_HA_ORIG[result$AVG_YEAR < 2004]))
#   expect_true(all(result$AGB_T_HA[result$AVG_YEAR > 2004] <= result$AGB_T_HA_ORIG[result$AVG_YEAR > 2004]))
# })
#
# test_that("TempVar function behaves consistently", {
#
#   set.seed(1234)
#   test_data <- plots[sample(nrow(plots), 10), ]
#   test_data <- BiomePair(test_data)
#
#   result <- TempVar(test_data, 2005)
#
#   # Check output structure
#   expect_s3_class(result, "data.frame")
#   expect_true(all(c("AGB_T_HA", "sdGrowth") %in% names(result)))
#
#   # Check for expected calculations
#   expect_true(all(result$sdGrowth >= 0))
#   expect_false(any(is.na(result$sdGrowth)))
# })
#
# test_that("TempApply and TempVar both give consistent results - uncorrected AGB", {
#
#   set.seed(12345)
#   test_data <- plots[sample(nrow(plots), 10), ]
#   test_data <- BiomePair(test_data)
#
#   resultApply <- TempApply(test_data, 2005)
#   resultVar <- TempVar(test_data, 2005)
#
#   # Check output structure
#   expect_equal(sort(resultApply$AGB_T_HA_ORIG), sort(resultVar$AGB_T_HA))
#
# })


