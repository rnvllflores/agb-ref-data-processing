## Updates made to the new framework:
# 20/01/25:
# Minor changes to make code more robust to eventual changes in the BIOMASS uptream package



#' Calculate Plot-Level AGB and Standard Deviation
#'
#' This function calculates plot-level Above Ground Biomass (AGB) and Standard Deviation using tree-level data and plot locations.
#' It uses the BIOMASS package for AGB calculation and uncertainty estimation.
#'
#' @param plot A data frame containing tree-level data.
#' @param xy A data frame containing plot locations.
# @param region A character string specifying the region for wood density estimation. Default is 'India'.
#' @inheritParams BIOMASS::getWoodDensity
#'
#' @return A data frame with plot-level AGB estimates and standard deviations, including columns:
#'   PLOT_ID, POINT_X, POINT_Y, SIZE_HA, AVG_YEAR, AGB_T_HA, and sdTree.
#'
#' @details
#' The function performs the following steps:
#' 1. Filters trees with diameter >= 10cm
#' 2. Corrects taxonomy and retrieves wood density data
#' 3. Computes or uses provided tree height data
#' 4. Runs Monte Carlo simulation for AGB estimation
#' 5. Calculates plot-level AGB and standard deviation
#' 6. Scales values per hectare
#'
#' @import dplyr
#' @importFrom BIOMASS correctTaxo getWoodDensity modelHD retrieveH AGBmonteCarlo
#'
#' @examples
#' plotsTree <- utils::read.csv(sample_file("SampleTree.csv"))
#' xyTree <- utils::read.csv(sample_file("SampleTreeXY.csv"))
#' plot_uncertainties <- MeasurementErr(plotsTree, xyTree, region = "Asia")
#'
#' @export
MeasurementErr <- function(plot, xy, region = "World") {

  plot <- subset(plot, diameter>=10) #filter those above 10cm in diameter
  #  blowup <- plot[1,5] / 10000
  # print(paste('plot size is', blowup, 'ha'))

  #taxonomy correction
  #tax <- correctTaxo(genus = plot$genus, species = plot$species)
  #plot$genus <- tax$genusCorrected
  #plot$species <- tax$speciesCorrected

  #get wood density
  wd <- getWoodDensity(genus = plot$genus,
                       species = plot$species,
                       stand = plot$id, region=region)
  plot$wd <- wd$meanWD
  plot$sd.wd<- wd$sdWD

  #compute local HD model / input your own H data if you have
  if("height" %in% colnames(plot)){
    message('Using actual tree height from the provided plot data.')
  }else{
    message('No tree height data found in original plot data. Calculating height using BIOMASS height-diameter model.')
    HDmodel <- modelHD(D = BIOMASS::NouraguesHD$D, H = BIOMASS::NouraguesHD$H,
                       method='weibull',  useWeight = TRUE, drawGraph = FALSE, plot = NULL)
    dataHlocal <- retrieveH(D = plot$diameter, model = HDmodel)
    plot$height <- dataHlocal$H
  }

  #run MC simulation
  if("height" %in% colnames(plot)){
    mc <- by(plot, plot$id,
             function(x) AGBmonteCarlo(D = x$diameter, WD = x$wd, errWD = x$sd.wd,
                                       H = x$height, errH = x$height, Dpropag ='chave2004'),simplify = F)
  }else{
    mc <- by(plot, plot$id,
             function(x) AGBmonteCarlo(D = x$diameter, WD = x$wd, errWD = x$sd.wd,
                                       HDmodel = HDmodel, Dpropag = "chave2004"),simplify = F)}

  #get agb and sd
  agb <- unlist(sapply(mc, "[", "meanAGB"))
  sd <- unlist(sapply(mc, "[", "sdAGB"))

  #add XY
  #plot.fin <- left_join(plot, xy, by = c('id' = 'id')) #needs full to avoid gaps
  plot.fin <- dplyr::left_join(plot, unique(xy), by = dplyr::join_by(id))

  #remove unecessaries
  plot.fin <- plot.fin[,c("id","x","y", 'size', 'year')] # retain columns of interest

  #summarize per plot and add key results
  plot.fin$x <- as.numeric(plot.fin$x)
  plot.fin$y <- as.numeric(plot.fin$y)

  plot.fin <- plot.fin |>
    dplyr::group_by(id) |>
    dplyr::summarise_all(mean)

  #scale values per ha
  agb <- agb / (plot.fin$size/10000)
  sd <- sd / (plot.fin$size/10000)
  plot.fin$agb <- agb
  plot.fin$sd <- sd
  plot.fin <- as.data.frame(plot.fin[,c("id","x","y", 'size', 'year', 'agb', 'sd')]) # retain columns of interest
  plot.fin$size <- plot.fin$size / 10000
  names(plot.fin) <- c('PLOT_ID', 'POINT_X', 'POINT_Y', 'SIZE_HA', 'AVG_YEAR',
                       'AGB_T_HA', 'sdTree')
  return(plot.fin)

}
