## Updates made to the new framework:
# 13/01/25:
# Replaces raster package functions with terra equivalents.
# Uses sf for spatial operations instead of sp.
# Replaces ddply with base R aggregate function.
# Improves error handling and input validation.
# Uses foreach and doParallel for parallel processing, which are CRAN-compatible.


## Notes:




#' Inverse Dasymetric Mapping
#'
#' This function performs inverse dasymetric mapping on plot data. It selects plots
#' based on given criteria, optionally aggregates them, and calculates forest
#' fraction and AGB (Above Ground Biomass) data for each plot or cell.
#'
#' @param clmn Character, column name for plot selection.
#' @param value Character, value to select in the column.
#' @param aggr Numeric, aggregation factor in degrees (NULL for no aggregation).
#' @param minPlots Integer, minimum number of plots per aggregated cell.
#' @param wghts Logical, whether to use weights in sampling.
#' @param is_poly Logical, whether input plots are polygons.
#' @param own Logical, whether to use own AGB map.
#' @param fmask SpatRaster or NULL, forest mask raster.
#' @param agbTilesDir Character, AGB tiles directory.
#'
#' @return A data frame with plot/cell AGB values and related information.
#'
#' @import sf
#' @import terra
#' @import foreach
#' @import doParallel
#' @importFrom stats aggregate na.omit weighted.mean
#'
#' @export
invDasymetry <- function(clmn = "ZONE", value = "Europe", aggr = NULL,
                         minPlots = 1, wghts = FALSE, is_poly = TRUE, own = TRUE, fmask = NULL,
                         agbTilesDir = "data/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2020-fv5.0") {

  # Check if fmask is a SpatRaster
  if (!is.null(fmask) && !inherits(fmask, "SpatRaster")) {
    fmask <- terra::rast(fmask)
  }

  # Select plots fulfilling selection criterion
  clm <- which(names(plots.tf) == clmn)
  if (length(clm) == 0) stop(paste('Attribute', clmn, 'not found'))

  ndx <- which(plots.tf[, clm] == value)
  if (length(ndx) == 0) stop('There are no records satisfying the selection criterion.')
  plots.tf <- plots.tf[ndx, ]

  # Aggregate if aggr != NULL
  if (!is.null(aggr)) {
    plots.tf$Xnew <- aggr * (0.5 + plots.tf$POINT_X %/% aggr)
    plots.tf$Ynew <- aggr * (0.5 + plots.tf$POINT_Y %/% aggr)

    plots.tf$inv <- 1 / plots.tf$varPlot

    plotsTMP <- aggregate(plots.tf[, c('AGB_T_HA_ORIG', 'AGB_T_HA', 'SIZE_HA')],
                          list(plots.tf$Xnew, plots.tf$Ynew),
                          mean, na.rm = TRUE)

    plotsTMP <- cbind(plotsTMP, aggregate(plots.tf[, "varPlot"],
                                          list(plots.tf$Xnew, plots.tf$Ynew),
                                          function(x) 1 / sum(1 / x))[3])

    plotsTMP <- plotsTMP[with(plotsTMP, order(Group.2, Group.1)), ]

    x <- aggregate(AGB_T_HA ~ Xnew + Ynew, data = plots.tf,
                   FUN = function(x) weighted.mean(x, plots.tf$inv[plots.tf$AGB_T_HA %in% x], na.rm = TRUE))
    x <- x[with(x, order(Ynew, Xnew)), ]

    plotsTMP$AGB_T_HA1 <- x$AGB_T_HA

    names(plotsTMP) <- c("POINT_X", "POINT_Y", 'AGB_T_HA_ORIG', 'AGB_T_HA_UW',
                         'SIZE_HA', 'varPlot', 'AGB_T_HA')

    # Only keep plots satisfying minPlots criterion
    if (minPlots > 1) {
      blockCOUNT <- aggregate(plots.tf$AGB_T_HA, list(plots.tf$Xnew, plots.tf$Ynew),
                              function(x) length(na.omit(x)))
      ndx <- which(blockCOUNT$x >= minPlots)
      plotsTMP1 <- plotsTMP[ndx, ]
      if (nrow(plotsTMP1) < 2) {
        plotsTMP1 <- plotsTMP[1:2, ]
      }
      plotsTMP1$n <- subset(blockCOUNT, blockCOUNT$x >= minPlots)[[3]]
    }
    plots.tf <- plotsTMP1
    rsl <- aggr
  } else {
    # Determine resolution output
    #fname <- list.files(agbTilesDir, "*.tif")[99]
    fname <- list.files(agbTilesDir, "*.tif")[1]
    rsl <- terra::res(terra::rast(file.path(agbTilesDir, fname)))[1]
  }

  if (nrow(plots.tf) <= 1) {
    stop("Very few plots selected, try decreasing minPlots or run at original resolution")
  }

  print(paste0(nrow(plots.tf), ' plots being processed ...'))
  if (own) {
    rsl <- terra::res(AGBown)[1]
  }

  # Parallel processing setup
  nc <- parallel::detectCores()
  cl <- parallel::makeCluster(nc - 1)
  doParallel::registerDoParallel(cl, nc)

  FFAGB <- foreach::foreach(i = 1:nrow(plots.tf), .combine = 'rbind',
                            .packages = c('terra', 'sf'),
                            .export=c('MakeBlockPolygon', 'SRS',
                                      'sampleTreeCover', 'TCtileNames',
                                      'AGBtileNames', 'sampleTreeCover',
                                      'sampleAGBmap', 'plots', #for polygon
                                      'agbTilesDir', 'treeCoverDir', 'AGBown','fmask',
                                      'forestTHs')) %dopar% {
                                        if (is_poly) {
                                          pol <- plots[i, ]
                                        } else {
                                          pol <- MakeBlockPolygon(plots.tf$POINT_X[i], plots.tf$POINT_Y[i], rsl)
                                        }

                                        if (is.null(aggr)) {
                                          if (is.na(plots.tf$SIZE_HA[i])) {
                                            treeCovers <- sampleTreeCover(pol, forestTHs, wghts, fmask)
                                          } else if (plots.tf$SIZE_HA[i] >= 1) {
                                            treeCovers <- rep(1, length(forestTHs))
                                          } else {
                                            treeCovers <- sampleTreeCover(pol, forestTHs, wghts, fmask)
                                          }
                                        } else {
                                          treeCovers <- sampleTreeCover(pol, forestTHs, wghts, fmask)
                                        }

                                        wghts2 <- ifelse(is.null(aggr), FALSE, wghts)

                                        if (!is.null(aggr)) {
                                          c(treeCovers * plots.tf$AGB_T_HA[i],
                                            plots.tf$AGB_T_HA_UW[i], plots.tf$AGB_T_HA_ORIG[i],
                                            sampleAGBmap(pol, wghts2, own),
                                            plots.tf$SIZE_HA[i], plots.tf$n[i],
                                            plots.tf$POINT_X[i], plots.tf$POINT_Y[i])
                                        } else {
                                          c(treeCovers * plots.tf$AGB_T_HA[i], plots.tf$AGB_T_HA[i],
                                            plots.tf$AGB_T_HA_ORIG[i],
                                            sampleAGBmap(pol, wghts2, own),
                                            plots.tf$SIZE_HA[i],
                                            plots.tf$POINT_X[i], plots.tf$POINT_Y[i])
                                        }
                                      }

  parallel::stopCluster(cl)

  FFAGB <- as.data.frame(FFAGB)

  if (!is.null(aggr)) {
    names(FFAGB) <- c(paste0("plotAGB_", forestTHs), "uwPlotAGB", "orgPlotAGB",
                      "mapAGB", 'SIZE_HA', 'n', "x", "y")
  } else {
    names(FFAGB) <- c(paste0("plotAGB_", forestTHs), "tfPlotAGB", "orgPlotAGB",
                      "mapAGB", 'SIZE_HA', "x", "y")
  }

  print(head(FFAGB))
  return(FFAGB)
}




# old_invDasymetry <- function(clmn = "ZONE", value = "Europe", aggr = NULL,
#                              minPlots = 1, wghts = FALSE, is_poly=TRUE, own=TRUE,fmask=NA){
#
#   agbTilesDir <- "data/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2020-fv5.0"
#
#
#   if(class(fmask)=='RasterLayer'){
#     fmask <- fmask
#     plot(fmask)}
#   # returns a data.frame with (mean) AGB from plots satisfying selection
#   # criteria
#
#   if(is.null(aggr)) # overrule minPlots if no aggregagtion
#     minPlots <- 1
#
#   # select plots fulfilling selection criterion set by attr and value
#   clm <- which(names(plots.tf) == clmn)
#   if(length(clm)==0)
#     stop(paste('Attribute', attr, 'not found'))
#
#   ndx <- which(plots.tf[,clm] == value)
#   if(length(ndx)==0)
#     stop('There are no records satisfying the selection criterion.')
#   plots.tf <- plots.tf[ndx,]
#
#   # aggregate if aggr != NULL
#   if(!is.null(aggr)){
#     # aggregate to aggr degree cells
#     plots.tf$Xnew <- aggr * (0.5 + plots.tf$POINT_X %/% aggr)
#     plots.tf$Ynew <- aggr * (0.5 + plots.tf$POINT_Y %/% aggr)
#
#     #aggregatioN!
#     plots.tf$inv <- 1/plots.tf$varPlot
#
#     plotsTMP <- aggregate(plots.tf[,c('AGB_T_HA_ORIG', 'AGB_T_HA', 'SIZE_HA')],
#                           list(plots.tf$Xnew, plots.tf$Ynew),
#                           mean, na.rm=T)
#
#     plotsTMP <- cbind(plotsTMP, aggregate(plots.tf[,"varPlot"],
#                                           list(plots.tf$Xnew, plots.tf$Ynew), function(x) 1/sum(1/x))[3])
#
#     plotsTMP <- plotsTMP[with(plotsTMP, order(Group.2, Group.1)), ] #order to match
#     x <- ddply(plots.tf, .(paste(plots.tf$Ynew, plots.tf$Xnew)),
#                function(x) data.frame(Xnew=mean(x$Xnew),
#                                       Ynew=mean(x$Ynew),
#                                       AGB_T_HA=weighted.mean(x$AGB_T_HA, x$inv ,na.rm=T)))
#     x <- x[with(x, order(Ynew, Xnew)), ]
#
#     plotsTMP$AGB_T_HA1 <- x$AGB_T_HA
#
#     names(plotsTMP) <- c("POINT_X","POINT_Y",'AGB_T_HA_ORIG','AGB_T_HA_UW',
#                          'SIZE_HA', 'varPlot','AGB_T_HA')
#
#     # only keep plots satisfying minPlots criterion
#     if(minPlots > 1){
#       blockCOUNT <- aggregate(plots.tf$AGB_T_HA, list(plots.tf$Xnew, plots.tf$Ynew),
#                               function(x) length(na.omit(x)))
#       ndx <- which(blockCOUNT$x >= minPlots)
#       plotsTMP1 <- plotsTMP[ndx,]
#       if(nrow(plotsTMP1) < 2){plotsTMP1 <- plotsTMP[1:2,]}
#       plotsTMP1$n <- subset(blockCOUNT,blockCOUNT$x >= minPlots)[[3]] #add plots inside
#       print(plotsTMP1)
#     }
#     plots.tf <- plotsTMP1
#     rsl <- aggr
#   } else {
#     # determine resolution output
#     fname <- list.files(agbTilesDir, "*.tif")[99]
#     rsl <- xres(raster(file.path(agbTilesDir, fname)))
#   }
#
#   #error control for few plots after aggregation
#   try(if(nrow(plots.tf) <= 1) stop("very few plots selected, try decreasing minPlots or run at original resolution"))
#
#   print(paste0(nrow(plots.tf), ' number of plots being processed'))
#   if (own==T){rsl <- xres(AGBown)}
#   # sample forest fraction and AGB data per cell/plot
#   nc <- detectCores()
#   cl <- makeCluster(nc-1)
#   registerDoParallel(cl, nc)
#
#
#   FFAGB <- foreach(i=1:nrow(plots.tf), .combine='rbind',# .errorhandling = 'pass',
#                    .packages='raster', .export=c('MakeBlockPolygon', 'SRS',
#                                                  'sampleTreeCover', 'TCtileNames',
#                                                  'AGBtileNames', 'sampleTreeCover',
#                                                  'sampleAGBmap', 'plots', #for polygon
#                                                  'agbTilesDir', 'treeCoverDir', 'AGBown','fmask',
#                                                  'forestTHs')) %dopar% {
#
#                                                    if (is_poly==TRUE){
#                                                      pol <- plots[i,] #own polygon
#                                                    }else{
#                                                      pol <- MakeBlockPolygon(plots.tf$POINT_X[i],
#                                                                              plots.tf$POINT_Y[i], rsl)
#                                                    }
#                                                    if(is.null(aggr)){ #no aggregation!
#                                                      if(is.na(plots.tf$SIZE_HA[i])){
#                                                        treeCovers <- sampleTreeCover(pol, forestTHs, wghts, fmask)
#                                                      } else if(plots.tf$SIZE_HA[i] >= 1){
#                                                        # ***** if plot size equals 1 ha *****
#                                                        treeCovers <- rep(1, length(forestTHs))
#                                                      } else {
#                                                        # ***** if plot size less than 1 ha *****
#                                                        treeCovers <- sampleTreeCover(pol, forestTHs, wghts,fmask)
#                                                      }
#                                                    } else
#                                                      treeCovers <- sampleTreeCover(pol, forestTHs, wghts,fmask)
#                                                    wghts2 <- ifelse(is.null(aggr), FALSE, wghts)
#
#                                                    if(!is.null(aggr)){
#                                                      c(treeCovers * plots.tf$AGB_T_HA[i],
#                                                        plots.tf$AGB_T_HA_UW[i], plots.tf$AGB_T_HA_ORIG[i],
#                                                        sampleAGBmap(pol, wghts2, own),
#                                                        plots.tf$SIZE_HA[i], plots.tf$n[i],
#                                                        plots.tf$POINT_X[i], plots.tf$POINT_Y[i])}
#                                                    else{
#                                                      c(treeCovers * plots.tf$AGB_T_HA[i],plots.tf$AGB_T_HA[i],
#                                                        plots.tf$AGB_T_HA_ORIG[i],
#                                                        sampleAGBmap(pol, wghts2, own),
#                                                        plots.tf$SIZE_HA[i],
#                                                        plots.tf$POINT_X[i], plots.tf$POINT_Y[i])
#                                                    }
#
#                                                  }
#   stopCluster(cl)
#   FFAGB <- data.frame(FFAGB)
#   FFAGB
#   if (!is.null(aggr)){
#     names(FFAGB) <- c(paste0("plotAGB_", forestTHs), "uwPlotAGB", "orgPlotAGB",
#                       "mapAGB",'SIZE_HA', 'n', "x", "y")}
#   else{
#     names(FFAGB) <- c(paste0("plotAGB_", forestTHs), "tfPlotAGB","orgPlotAGB",
#                       "mapAGB",'SIZE_HA', "x", "y")
#   }
#
#
#
#   print(head(FFAGB))
#   return(FFAGB)
# }




# Tests

# library(testthat)
# library(sf)
# library(terra)
#
# # Test comparison between old and new versions
# test_that("Old and new invDasymetry functions produce consistent results", {
#   # Create sample data
#   plots.tf <- data.frame(
#     ZONE = c("Europe", "Europe", "Asia"),
#     POINT_X = c(10, 11, 20),
#     POINT_Y = c(50, 51, 30),
#     AGB_T_HA = c(100, 150, 200),
#     AGB_T_HA_ORIG = c(90, 140, 190),
#     SIZE_HA = c(1, 0.5, 2),
#     varPlot = c(10, 15, 20)
#   )
#
#   # Create sample forest mask
#   fmask <- rast(nrows=10, ncols=10, xmin=0, xmax=20, ymin=20, ymax=60, vals=sample(0:1, 100, replace=TRUE))
#
#   # Run both versions
#   old_result <- old_invDasymetry("ZONE", "Europe", aggr=NULL, minPlots=1, wghts=FALSE, is_poly=FALSE, own=FALSE, fmask=fmask)
#   new_result <- invDasymetry("ZONE", "Europe", aggr=NULL, minPlots=1, wghts=FALSE, is_poly=FALSE, own=FALSE, fmask=fmask)
#
#   # Compare results
#   expect_equal(old_result, new_result, tolerance=1e-6)
# })
#
# # Test internal consistency
# test_that("invDasymetry function behaves consistently", {
#   plots.tf <- data.frame(
#     ZONE = c("Europe", "Europe", "Asia"),
#     POINT_X = c(10, 11, 20),
#     POINT_Y = c(50, 51, 30),
#     AGB_T_HA = c(100, 150, 200),
#     AGB_T_HA_ORIG = c(90, 140, 190),
#     SIZE_HA = c(1, 0.5, 2),
#     varPlot = c(10, 15, 20)
#   )
#
#   fmask <- rast(nrows=10, ncols=10, xmin=0, xmax=20, ymin=20, ymax=60, vals=sample(0:1, 100, replace=TRUE))
#
#   result <- new_invDasymetry("ZONE", "Europe", aggr=NULL, minPlots=1, wghts=FALSE, is_poly=FALSE, own=FALSE, fmask=fmask)
#
#   # Check output structure
#   expect_s3_class(result, "data.frame")
#   expect_true(all(c("plotAGB_10", "tfPlotAGB", "orgPlotAGB", "mapAGB", "SIZE_HA", "x", "y") %in% names(result)))
#
#   # Check if the function correctly filters by ZONE
#   expect_equal(nrow(result), 2)
#
#   # Check if AGB values are within expected range
#   expect_true(all(result$plotAGB_10 >= 0 & result$plotAGB_10 <= max(plots.tf$AGB_T_HA)))
#
#   # Test with aggregation
#   result_agg <- new_invDasymetry("ZONE", "Europe", aggr=1, minPlots=1, wghts=FALSE, is_poly=FALSE, own=FALSE, fmask=fmask)
#   expect_true("n" %in% names(result_agg))
# })

