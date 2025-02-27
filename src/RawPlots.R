
#' Format Plot Data
#'
#' This function formats raw plot data into a standardized structure for further processing.
#'
#' @param plots A data frame containing plot data with Latitude, Longitude coordinates.
#' @param mapYear Optional. The year of the map being used for comparison (not used in current implementation).
#'
#' @return A data frame with formatted plot data, including columns for PLOT_ID, POINT_X, POINT_Y, AGB_T_HA, SIZE_HA, FEZ, GEZ, and AVG_YEAR.
#'
#' @details
#' The function prompts the user to select or manually enter column indices for key plot attributes.
#' It then formats the data, converting plot sizes from m^2 to hectares if necessary.
#'
#' @examples
#' \dontrun{
#' # Assuming 'raw_plots' is your input data frame
#' formatted_plots <- RawPlots(raw_plots)
#' }
#' @export
RawPlots <- function(plots, mapYear = NULL) {
  # Type checking
  if (!is.data.frame(plots)) {
    stop('Input file should be a data frame with XY coordinates')
  }

  # Helper function for column selection or manual entry
  select_column <- function(prompt) {
    choice <- menu(c("Manual entry", names(plots)), title = prompt)
    if (choice == 1) {
      manual_entry <- readline("Enter the numeric value for manual entry: ")
      return(as.numeric(manual_entry))
    } else {
      return(as.numeric(plots[, choice - 1]))
    }
  }

  id <- select_column("Which column is your unique Plot ID?")
  agb <- select_column("Which column is your plot AGB?")
  x <- select_column("Select longitude column")
  y <- select_column("Which column is your latitude?")
  size <- select_column("Select plot size column")
  year <- select_column("Select year column")

  # Convert sizes if in m2 to ha
  size <- ifelse(!is.na(size) & size > 50, size / 10000, size)

  # Initialize unused columns
  fez <- gez <- NA

  # Create and format data frame
  plt <- data.frame(id, x, y, agb, size, fez, gez, year)
  names(plt) <- c('PLOT_ID', 'POINT_X', 'POINT_Y', 'AGB_T_HA', 'SIZE_HA', 'FEZ', 'GEZ', 'AVG_YEAR')

  # Filter rows where AGB is not NA
  subset(plt, !is.na(plt$AGB_T_HA))
}



#' Format Tree-Level Plot Data
#'
#' This function formats raw tree-level plot data into a standardized structure for further processing.
#'
#' @param plots A data frame containing tree-level plot data with Latitude, Longitude coordinates.
#'
#' @return A list containing two data frames:
#'   1. Tree-level data with columns for id, genus, species, diameter, (height), size, fez, gez, year
#'   2. Plot-level data with columns for id, x, y
#'
#' @details
#' The function prompts the user to select column indices for key tree and plot attributes.
#' It handles cases with and without tree height data.
#'
#' @examples
#' # Assuming 'raw_tree_plots' is your input data frame
#' formatted_tree_plots <- RawPlotsTree(raw_tree_plots)
#'
#' @export
RawPlotsTree <- function(plots=plots){
  if(is.data.frame(plots) == F){
    stop('input file should be a data frame with XY coordinates')
  }

  id <-plots [,menu(names(plots), title="which column is your unique Plot ID?")]
  genus <-plots [,menu(names(plots), title="column of tree Genus?")]

  species <-plots [,menu(names(plots), title="column of tree Species?")]

  diameter <-as.numeric(plots [,menu(names(plots), title="column of tree DBH?")])

  ans <- menu(c('yes', 'no'))

  if (ans=='yes'){
    height <-as.numeric(plots [,menu(names(plots), title="column of tree Height?")])
  }


  x <-as.numeric(plots [,menu(names(plots),
                              title="put longitude  column")])
  y <-as.numeric(plots [,menu(names(plots),
                              title="which column is your latitude")])

  size <- as.numeric(plots [,menu(names(plots),
                                  title="plot size column")])

  fez <- NA
  gez <- NA
  year <-as.numeric(plots [,menu(names(plots),
                                 title=" year  column")])


  plt <- data.frame(id,	genus	,species,diameter, size, fez, gez, year)

  if (ans=='yes'){
    plt <- data.frame(id,	genus	,species,diameter,height, size, fez, gez, year)
  }

  plt1 <- data.frame(id,	x,y)


  plt$id <- factor(plt$id, levels=unique(plt$id), labels=seq_along(nrow(plt)))
  plt1$id <- factor(plt1$id, levels=unique(plt1$id), labels=seq_along(nrow(plt1)))


  list(plt,plt1)

}

