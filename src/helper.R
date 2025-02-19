# plt helper and transformation functions


#' Add longitude and latitude columns to an sf object
#'
#' This function takes an `sf` object containing point geometries and extracts the longitude (`POINT_X`)
#' and latitude (`POINT_Y`) coordinates from the geometry column. These coordinates are added as new columns
#' to the `sf` object.
#'
#' @param sf_object An `sf` object containing point geometries.
#'
#' @return An updated `sf` object with additional columns: `POINT_X` (longitude) and `POINT_Y` (latitude).
#'
#' @importFrom sf st_coordinates
#'
#' @export
#'
#' @examples
#' library(sf)
#' # Example sf object
#' sampled_plots <- st_as_sf(data.frame(
#'   PLOT_ID = c("EU2", "EU1"),
#'   AGB_T_HA = c(87.87, 67.79),
#'   AVG_YEAR = c(2001, 2008),
#'   SIZE_HA = c(0.196, 0.015),
#'   geometry = st_sfc(st_point(c(1.305915, 42.59214)), st_point(c(15.21873, 59.81792)))
#' ), crs = 4326)
#'
#' # Convert to include POINT_X and POINT_Y
#' updated_sf <- sf_to_sf_with_coords(sampled_plots)
#'
#' @export
sf_to_sf_with_coords <- function(sf_object) {
  if (!inherits(sf_object, "sf")) {
    stop("Input must be an sf object.")
  }

  # Extract coordinates from geometry
  coords <- sf::st_coordinates(sf_object)

  # Add POINT_X and POINT_Y columns
  sf_object$POINT_X <- coords[, "X"]
  sf_object$POINT_Y <- coords[, "Y"]

  return(sf_object)
}


#' Convert sf object to dataframe with longitude and latitude columns
#'
#' This function takes an sf object and converts it to a data frame,
#' adding "POINT_X" and "POINT_Y" columns for longitude and latitude.
#' If the input sf object is not in WGS 84, it will be transformed.
#'
#' @param sf_object An sf object with POINT geometry
#' @return A data frame with all original columns plus POINT_X and POINT_Y in WGS 84 CRS.
#' @import sf
#' @export
#'
#' @examples
#' library(sf)
#' # Assuming 'sampled_plots$non_deforested_plots' is your sf object
#' df <- sf_to_df_with_coords(sampled_plots$non_deforested_plots)
sf_to_df_with_coords <- function(sf_object) {
  # Check if input is an sf object
  if (!inherits(sf_object, "sf")) {
    stop("Input must be an sf object")
  }

  # Transform to WGS 84 if necessary
  if (st_crs(sf_object) != st_crs(4326)) {
    sf_object <- st_transform(sf_object, 4326)
  }

  # Extract coordinates
  coords <- st_coordinates(sf_object)

  # Convert to data frame and add coordinates
  df <- cbind(st_drop_geometry(sf_object),
              POINT_X = coords[,1],
              POINT_Y = coords[,2])

  return(df)
}



#' Check and covert plt input
#'
#' This helper function checks if the input is an sf object or a dataframe,
#' and ensures it contains the required coordinate columns.
#'
#' @inheritParams Deforested
#'
#' @return A dataframe with "POINT_X" and "POINT_Y" columns
#'
#' @details
#' If the input is an sf object, it is converted to a dataframe with coordinate columns.
#' The function then checks for the presence of "POINT_X" and "POINT_Y" columns.
#'
#' @importFrom sf st_drop_geometry
#'
check_and_convert_plt <- function(plt, ez = FALSE) {
  # Check if plt is an sf object or a dataframe
  if (inherits(plt, "sf")) {
    plt <- sf_to_df_with_coords(plt)
  }

  # Check if "POINT_X" and "POINT_Y" columns are available in dataframe
  if (!all(c("POINT_X", "POINT_Y") %in% colnames(plt))) {
    stop("Input plt data must contain 'POINT_X' and 'POINT_Y' columns in WGS 84.")
  }

  # Check if EZ data is present:
  if(isTRUE(ez)) {
    if (!all(c("ZONE", "FAO.ecozone", "GEZ") %in% colnames(plt))) {
      stop("Input plt data must contain 'ZONE', 'FAO.ecozone', 'GEZ' columns. Try to run BiomePair(plt) function before.")
    }
  }

  return(plt)
}



