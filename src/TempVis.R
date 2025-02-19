## Updates made to the new framework:
# 06/01/2025:
# Exlicit dplyr:: function calls to avoid potential conflicts.
# Removed setwd() calls and used full file paths instead.

## Notes:
# 06/01/2025:
# Modified the original output of old_HistoShift for testing purposes


#' Visualize histogram of temporal fix effect
#'
#' This function creates a histogram to visualize the effect of temporal adjustment
#' or forest fraction correction on AGB values. It generates a plot comparing the
#' distribution of AGB values before and after the adjustment.
#'
#' @param df A data frame containing AGB data.
#' @param year Numeric value indicating the year of analysis.
#' @param outDir Character string specifying the output directory for saving the histogram (default: "results").
#'
#' @return Invisibly returns NULL. The function creates a plot as a side effect.
#'
#' @importFrom graphics hist plot axis legend
#' @importFrom grDevices png dev.off rgb
#'
#' @export
#' @examples
#' set.seed(42)
#' sample_plots <- plots[sample(nrow(plots), 10), ]
#' sample_plots <- BiomePair(sample_plots)
#' sample_plots <- TempApply(sample_plots, 2004)
#' head(sample_plots)
#' HistoTemp(sample_plots, 2004)
HistoTemp <- function(df, year, outDir = "results") {

    # Create output directory if it doesn't exist
  if (!dir.exists(outDir)) {
    dir.create(outDir, recursive = TRUE)
  }

  if ("plotAGB_10" %in% colnames(df)) {
    df$AGB_T_HA <- df$plotAGB_10
    df$AGB_T_HA_ORIG <- df$orgPlotAGB
    main <- 'Before and after FF correction'
  } else {
    df$AGB_T_HA <- df$AGB_T_HA
    df$AGB_T_HA_ORIG <- df$AGB_T_HA_ORIG
    main <- 'Before and after temporal adjustment'
  }

  df <- df[(df$AGB_T_HA < 600 & df$AGB_T_HA_ORIG < 600 & df$AGB_T_HA > 0), ]  #select 600 and below, disregard negative for now

  # create a bar graph with fixed agb bins
  h1 <- hist(df$AGB_T_HA_ORIG, plot = FALSE, breaks = 25)
  h2 <- hist(df$AGB_T_HA, plot = FALSE, breaks = 25)

  png(filename = file.path(outDir, paste0('histogram_tempfixed_', year, '.png')),
      width = 800, height = 600)
  y.ax <- nrow(df) / 2
  plot(h1, xaxt = "n", col = rgb(0, 0, 1, 1/4), main = main, xlab = 'AGB(Mg/ha)',
       ylab = 'n', xlim = c(0, 600), ylim = c(0, y.ax),
       cex.lab = 2, cex.axis = 1.5, cex.main = 2, cex.sub = 2)
  axis(1, at = 0:6 * 100, labels = c(0:6 * 100),
       cex.lab = 2, cex.axis = 1.5, cex.main = 2, cex.sub = 2)
  plot(h2, col = rgb(1, 0, 0, 1/4), add = TRUE)
  legend("topright", c("Before", "After", "Overlap"),
         col = c(rgb(0, 0, 1, 1/4), rgb(1, 0, 0, 1/4), rgb(0.5, 0, 0.5, 1/4)),
         lwd = 10, cex = 2, bty = 'n')
  dev.off()

  invisible(NULL)
}


#' Analyze AGB shift in bins
#'
#' This function analyzes the shift in AGB values across different bins before and after
#' temporal adjustment or forest fraction correction. It generates a summary table
#' of the changes in AGB distribution.
#'
#' @param df A data frame containing AGB data.
#' @param year Numeric value indicating the year of analysis.
#' @param outDir Character string specifying the output directory for saving the CSV file (default: "results").
#'
#' @return A data frame summarizing the changes in AGB distribution across bins.
#'
#' @importFrom dplyr group_by tally full_join
#' @importFrom utils write.csv
#'
#' @export
#' @examples
#' set.seed(42)
#' sample_plots <- plots[sample(nrow(plots), 10), ]
#' sample_plots <- BiomePair(sample_plots)
#' sample_plots <- TempApply(sample_plots, 2004)
#' head(sample_plots)
#' HistoShift(sample_plots, 2004)
HistoShift <- function(df, year, outDir = "results") {

  # Create output directory if it doesn't exist
  if (!dir.exists(outDir)) {
    dir.create(outDir, recursive = TRUE)
  }

  if ("plotAGB_10" %in% colnames(df)) {
    df$AGB_T_HA <- df$plotAGB_10
    df$AGB_T_HA_ORIG <- df$orgPlotAGB
  } else {
    df$AGB_T_HA <- df$AGB_T_HA
    df$AGB_T_HA_ORIG <- df$AGB_T_HA_ORIG
  }

  #calculate change in bins
  df$AGB_T_HA_ORIG <- ifelse(df$AGB_T_HA_ORIG == 0, df$AGB_T_HA_ORIG + 0.0001, df$AGB_T_HA_ORIG)
  df$AGB_T_HA <- ifelse(df$AGB_T_HA == 0, df$AGB_T_HA + 0.0001, df$AGB_T_HA)

  bins <- c(0:9 * 20, 2:5 * 100, Inf)
  old1 <- transform(df, group = cut(AGB_T_HA_ORIG, breaks = bins))
  new1 <- transform(df, group = cut(AGB_T_HA, breaks = bins))

  old2 <- dplyr::group_by(old1, group) %>% dplyr::tally()
  new2 <- dplyr::group_by(new1, group) %>% dplyr::tally()

  #calculate change in AGB
  old3 <- aggregate(old1["AGB_T_HA_ORIG"], by = old1["group"], mean)
  new3 <- aggregate(new1["AGB_T_HA"], by = new1["group"], mean)

  if (nrow(old2) != nrow(new2)) {
    fj1 <- dplyr::full_join(old2, new2, by = 'group')
    fj2 <- dplyr::full_join(old3, new3, by = 'group')
    outs <- cbind(fj1, fj2)
    outs <- outs[, c(1, 2, 3, 5, 6)]
  } else {
    outs <- do.call(cbind, list(old2, new2, old3, new3))
    outs <- outs[, c(1, 2, 4, 6, 8)]
  }

  names(outs) <- c('agb_Mgha_bins', 'n_pre', 'n_post', 'agb_Mgha_pre', 'agb_Mgha_post')
  write.csv(outs, file.path(outDir, paste0('TF_pre_post_change_', year, '.csv')), row.names = FALSE)

  outs
}







## FUNCTIONS TO VISUALIZE HISTOGRAM EFFECT OF TEMPORAL FIX AND ALSO SUMMARIZES IT PER AGB BIN IN A TABLE

# old_HistoTemp <- function(df, year){
#
#   outDir <- "results"
#
#   if("plotAGB_10" %in% colnames(df)){
#     df$AGB_T_HA <- df$plotAGB_10
#     df$AGB_T_HA_ORIG <- df$orgPlotAGB
#     main <- 'Before and after FF correction'
#   }else {
#     df$AGB_T_HA <- df$AGB_T_HA
#     df$AGB_T_HA_ORIG <- df$AGB_T_HA_ORIG
#     main <- 'Before and after temporal adjustment'}
#
#   df <- df[(df$AGB_T_HA < 600 & df$AGB_T_HA_ORIG <600 & df$AGB_T_HA > 0), ] #select 600 and below, disregard negative for now
#
#   # create a bar graph with fixed agb bins
#   h1 <- hist(df$AGB_T_HA_ORIG, plot=F, breaks=25)
#   h2 <- hist(df$AGB_T_HA, plot=F, breaks=25)
#
#   png (filename=paste0(outDir,paste0('/histogram_tempfixed_',year,'.png')),  width = 800, height = 600)
#   y.ax <- nrow(df) / 2
#   plot(h1, xaxt="n", col=rgb(0,0,1,1/4), main=main, xlab='AGB(Mg/ha)',ylab='n',
#        xlim = c(0,600), ylim=c(0,y.ax),cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2)
#   axis(1,at=0:6*100, labels=c(0:6*100),cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2)
#   plot(h2,col=rgb(1,0,0,1/4),add=T)
#   legend("topright", c("Before", "After", "Overlap"),
#          col=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4), rgb(0.5,0,0.5,1/4)), lwd=10, cex=2, bty='n')
#
#   dev.off()
#
#
#   plot(h1, xaxt="n", col=rgb(0,0,1,1/4), main=main, xlab='AGB(Mg/ha)',ylab='n',
#        xlim = c(0,600), ylim=c(0,y.ax))
#   axis(1,at=0:6*100, labels=c(0:6*100))
#   plot(h2,col=rgb(1,0,0,1/4),add=T)
#   legend("topright", c("Before", "After", "Overlap"),
#          col=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4), rgb(0.5,0,0.5,1/4)), lwd=10)
# }
#
# old_HistoShift <- function(df, year){
#
#   outDir <- "results"
#   mainDir <- getwd()
#
#   if("plotAGB_10" %in% colnames(df)){
#     df$AGB_T_HA <- df$plotAGB_10
#     df$AGB_T_HA_ORIG <- df$orgPlotAGB
#
#   }else {
#     df$AGB_T_HA <- df$AGB_T_HA
#     df$AGB_T_HA_ORIG <- df$AGB_T_HA_ORIG
#    }
#
#   #calculate change in bins
#   df$AGB_T_HA_ORIG <- ifelse(df$AGB_T_HA_ORIG == 0, df$AGB_T_HA_ORIG + 0.0001,df$AGB_T_HA_ORIG)
#   df$AGB_T_HA <- ifelse(df$AGB_T_HA == 0, df$AGB_T_HA + 0.0001,df$AGB_T_HA)
#
#   bins <- c(0:9*20, 2:5*100, Inf)
#   old1 <- transform(df, group=cut(AGB_T_HA_ORIG,  breaks=bins))
#   new1 <- transform(df, group=cut(AGB_T_HA,  breaks=bins))
#
#   old2 <- old1 %>%
#     dplyr::group_by(group) %>%
#     dplyr::tally()
#   old2 <- as.data.frame(old2)
#
#   new2 <- new1 %>%
#     dplyr::group_by(group) %>%
#     dplyr::tally()
#   new2 <- as.data.frame(new2)
#
#   #calculate change in AGB
#   old3 <- aggregate(old1["AGB_T_HA_ORIG"], by=old1["group"], mean)
#   new3 <- aggregate(new1["AGB_T_HA"], by=new1["group"], mean)
#
#   if (nrow(old2) != nrow(new2)){
#     fj1 <- dplyr::full_join(old2, new2, by=c('group'='group'))
#     fj2 <-  dplyr::full_join(old3, new3, by=c('group'='group'))
#     outs <- cbind(fj1,fj2)
#     outs <- outs[,c(1,2,3,5,6)]
#
#   }
#   else{
#     outs <- do.call(cbind, list(old2,new2,old3,new3))
#     outs <- outs[,c(1,2,4,6,8)]
#   }
#
#   names(outs) <- c('agb_Mgha_bins', 'n_pre', 'n_post', 'agb_Mgha_pre', 'agb_Mgha_post')
#   #setwd(outDir)
#   #write.csv(outs, paste0('TF_pre_post_change_',year,'.csv'), row.names = F)
#   #write.csv(outs, file.path(outDir, paste0('TF_pre_post_change_', year, '.csv')), row.names = FALSE)
#   #setwd(mainDir)
#   outs
# }


# Tests:

# library(testthat)
#
# # Test comparison between old and new versions
# test_that("Old and new HistoTemp functions produce consistent results", {
#   # Create sample data
#   test_data <- data.frame(
#     AGB_T_HA = runif(100, 0, 600),
#     AGB_T_HA_ORIG = runif(100, 0, 600)
#   )
#
#   # Run both versions
#   new_result <- capture.output(HistoTemp(test_data, 2020))
#   old_result <- capture.output(old_HistoTemp(test_data, 2020))
#
#   # Compare results
#   expect_equal(old_result, new_result)
#
#   # Check if file was created
#   expect_true(file.exists(file.path(outDir, "histogram_tempfixed_2020.png")))
# })
#
# test_that("Old and new HistoShift functions produce consistent results", {
#   # Create sample data
#   test_data <- data.frame(
#     AGB_T_HA = runif(100, 0, 600),
#     AGB_T_HA_ORIG = runif(100, 0, 600)
#   )
#
#   # Run both versions
#   old_result <- old_HistoShift(test_data, 2020)
#   new_result <- HistoShift(test_data, 2020)
#
#   # Compare results
#   expect_equal(old_result, new_result)
#
#   # Check if file was created
#   expect_true(file.exists(file.path(outDir, "TF_pre_post_change_2020.csv")))
# })
#
# # Test internal consistency
# test_that("HistoTemp function behaves consistently", {
#   test_data <- data.frame(
#     AGB_T_HA = runif(100, 0, 600),
#     AGB_T_HA_ORIG = runif(100, 0, 600)
#   )
#
#   # Check if function runs without errors
#   expect_no_error(HistoTemp(test_data, 2020))
#
#   # Check if output file is created
#   expect_true(file.exists(file.path(outDir, "histogram_tempfixed_2020.png")))
# })
#
# test_that("HistoShift function behaves consistently", {
#   test_data <- data.frame(
#     AGB_T_HA = runif(100, 0, 600),
#     AGB_T_HA_ORIG = runif(100, 0, 600)
#   )
#
#   result <- HistoShift(test_data, 2020)
#
#   # Check output structure
#   expect_s3_class(result, "data.frame")
#   expect_true(all(c("agb_Mgha_bins", "n_pre", "n_post", "agb_Mgha_pre", "agb_Mgha_post") %in% names(result)))
#
#   # Check if output file is created
#   expect_true(file.exists(file.path(outDir, "TF_pre_post_change_2020.csv")))
# })

