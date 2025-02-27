#' Get path to example sample file
#'
#' This package comes bundled with a number of sample files in its `inst/extdata`
#' directory. This function make them easy to access
#'
#' @param file Name of file. If `NULL`, all sample files will be listed.
#' @export
#' @examples
#' sample_file()
#' sample_file("SampleUnformattedPlots.csv")
sample_file <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "Plot2Map"))
  } else {
    system.file("extdata", file, package = "Plot2Map", mustWork = TRUE)
  }
}


#' Get path to example sample lidar folder
#'
#' This package comes bundled with a number of sample files in its `inst/extdata`
#' directory. This function make them easy to access
#'
#' @param file Name of folder.
#' @export
#' @examples
#' sample_lidar_folder("SustainableLandscapeBrazil_v04/SLB_AGBmaps")
sample_lidar_folder <- function(folder) {

  dir(system.file(file.path("extdata", folder), package = "Plot2Map"))

  return(file.path("extdata", folder))
}
