## Updates made to the new framework:
# 20/01/25:
# - Make code more robust, for example not assuming a certain projection for cent.sf data as previous version hardcoded buffer resolution:
#           pol <- st_buffer(cent.sf, dist = 5.64, nQuadSegs = 1) # square buffer approx. 1km resolution
# - Replacing sp with sf package


#' Format Tree-Level Data from Nested Plots
#'
#' This function processes tree-level data from nested plots, combining centroid information
#' with tree measurements using a unique ID from these datasets.
#'
#' @param centroids_sf A sf object containing centroids information of sub-plots.
#' @param tree_table A data frame containing tree-level measurements.
#'
#' @return A list containing two elements:
#'   \item{plotTree}{A data frame with formatted tree-level data}
#'   \item{xyTree}{A data frame with plot ID and coordinates}
#'
#' @details
#' The function performs the following steps:
#' 1. Converts centroid data to WGS84 projection
#' 2. Creates a buffer around centroids
#' 3. Filters tree data for alive trees
#' 4. Prompts user for column selections and additional information
#' 5. Processes tree height data
#' 6. Combines tree data with centroid coordinates
#'
#' @import sf
#' @importFrom dplyr left_join
#'
#' @examples
#' centroid_data <- readOGR(dsn = "path/to/shapefile", layer = "centroid_layer")
#' tree_data <- read.csv("path/to/tree_data.csv")
#' nested_data <- Nested(centroid_data, tree_data)
#'pol
#' @export
Nested <- function(centroids_sf, tree_table) {

  cent.wgs <- sf::st_transform(centroids_sf, 4326) # to WGS84
  sf_use_s2(TRUE)
  pol <- sf::st_buffer(cent.wgs, dist = 1000, nQuadSegs = 1) # square buffer approx. 1km resolution

  # cent.sf <- st_as_sf(centroid_shp)
  # cent.wgs <- spTransform(centroid_shp, CRS("+init=epsg:4326")) #to WGS84
  # pol <- st_buffer(cent.sf, 5.64) #square buffer approx. 1km resolution

  plotTree0 <- subset(tree_table, tree_table$TREE_ALIVE == 1 &
                        tree_table$TREE_OR_STUMP == 1) #only alive!

  id <-plotTree0 [,menu(names(plotTree0), title="which column is your unique Plot ID?")]
  diameter <- as.numeric(plotTree0 [,menu(names(plotTree0),
                                          title="which column is your unique  DBH (cm)?")])
  height <-as.numeric(plotTree0 [,menu(names(plotTree0),
                                       title="which column is your unique Tree Height (m)?")])
  genus <- readline(prompt="Enter tree genus: ")#Picea
  species <- readline(prompt="Enter tree species: ")#sitchensis
  size <- as.numeric(readline(prompt="Enter plot size in m2: ")) #100
  fez <- NA
  gez <- NA
  year<- 2010

  plotTree <- data.frame(id, genus, species, diameter, size, fez, gez, year, height)

  #fill height for NAs
  for (i in unique(plotTree$id)){
    plotTree$height <- ifelse(plotTree$id==i & is.na(plotTree$height),
                              mean(subset(plotTree, plotTree$id == i)$height,na.rm=T),
                              plotTree$height)
  }

  # cent.wgs$x <-cent.wgs@coords[,1]
  # cent.wgs$y <- cent.wgs@coords[,2]
  cent.wgs$x <- sf::st_coordinates(cent.wgs)[,1]
  cent.wgs$y <- sf::st_coordinates(cent.wgs)[,2]

  #xyTree <- left_join(plotTree, as.data.frame(cent.wgs), by=c('id'='POINT_GUID'))
  xyTree <- dplyr::left_join(plotTree, cent.wgs, by=c('id'='POINT_GUID'))
  xyTree <- xyTree[,c('id', 'y', 'x')]

  list(plotTree,xyTree)
}


