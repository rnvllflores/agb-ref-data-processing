# update open and tier hard-coded values

library(here)
# Load the 'config.RData' file from the 'src/config' directory
load(here("src", "config", "directoryConfig.RData"))
dataDir <- config$dataDir

add_bio_realms <- function(df) {
 
  
  biome_path <- file.path(dataDir, "Ecoregions2017_biome.tif")
  realm_path <- file.path(dataDir, "Ecoregions2017_realm.tif")
  bio_id_path <- file.path(dataDir, "biome_id.csv")
  realm_id_path <- file.path(dataDir, "realm_id.csv")
  
  # Set the input data as spatial points
  coordinates(df) <- ~POINT_X + POINT_Y
  
  # Load the required raster files
  biome <- raster(biome_path)
  realm <- raster(realm_path)
  
  # Load the ID mappings for biome and realm
  bioID <- read.csv(bio_id_path)
  realmID <- read.csv(realm_id_path)
  
  # Extract the biome and realm values from the raster data
  vlsBiome <- raster::extract(biome, df)
  vlsRealm <- raster::extract(realm, df)
  
  # Add the extracted values to the data frame
  df$BIO <- vlsBiome
  df$REALM <- vlsRealm
  
  # Handle missing or zero values in the extracted values
  df$BIO <- ifelse(df$BIO == 0, df$BIO[which(df$BIO == 0) + 1], df$BIO)
  df$BIO <- ifelse(df$BIO == 0, df$BIO[which(df$BIO == 0) + 1], df$BIO)
  df$REALM <- ifelse(df$REALM == 0, df$REALM[which(df$REALM == 0) + 1], df$REALM)
  df$REALM <- ifelse(df$REALM == 0, df$REALM[which(df$REALM == 0) + 1], df$REALM)
  
  # Join with biome and realm ID data
  df <- left_join(as.data.frame(df), bioID, by = c('BIO' = 'ID'))
  df <- left_join(df, realmID, by = c('REALM' = 'ID'))
  
  # Reorder columns to include the BIO and REALM names
  df$BIO <- df[[length(df) - 1]]
  df$REALM <- df[[length(df)]]
  
  # Remove the old BIO and REALM columns (IDs) and reorder the columns
  df <- df[, -c(length(df) - 1, length(df))]
  
  # Add new columns for OPEN and TIER with default values
  df$OPEN <- 0  # open = 0
  df$TIER <- NA # tier is not available initially
  
  # # Select and reorder the columns of the final data frame
  # df <- df %>%
  #   dplyr::select(CODE, AGB_T_HA, SIZE_HA, GEZ, AVG_YEAR, ZONE, POINT_X, POINT_Y,
  #                 sdTree, sdSE, AGB_T_HA_ORIG, sdGrowth, varTot = varPlot, MapName,
  #                 VER, sdMap, BIO, REALM, OPEN, INVENTORY, TIER)
  
  return(df)
}
