
#' Download ESA CCI Biomass GeoTIFF Data
#'
#' This function downloads ESA CCI Biomass GeoTIFF data from the CEDA Archive.
#'
#' @param esacci_biomass_year The ESA CCI BIOMASS AGB tiles year to use. Use either 2010, 2015, 2016, 2017, 2018, 2019,
#' 2020, 2021 or "latest" (default).
#' @param esacci_biomass_version The ESA CCI BIOMASS AGB tiles version to use. Use either "v2.0", "v3.0", "v4.0",
#' "v5.0", "v5.01" or "latest" (default).
#' @param esacci_folder Directory to save downloaded ESA CCI BIOMASS AGB files. Default is the relative path "data/ESACCI-BIOMASS".
#' @param n_cores Number of cores to use for parallel download.
#' @param timeout Number of seconds for reaching file download timeout.
#' @param file_names Character vector of specific filenames to download. If NULL (default), all files will be downloaded.
#'
#' @return A character vector of downloaded file paths.
#'
#' @import parallel
#' @import rvest
#' @import httr
#' @import pbapply
#'
#' @export
#'
#' @references [Santoro, M.; Cartus, O. (2024): ESA Biomass Climate Change Initiative (Biomass_cci): Global datasets of forest above-ground biomass for the years 2010, 2015, 2016, 2017, 2018, 2019, 2020 and 2021, v5.01. NERC EDS Centre for Environmental Data Analysis, 22 August 2024.](https://dx.doi.org/10.5285/bf535053562141c6bb7ad831f5998d77)
#'
download_esacci_biomass <- function(esacci_biomass_year = "latest",
                                    esacci_biomass_version = "latest",
                                    esacci_folder = "data/ESACCI-BIOMASS",
                                    n_cores = parallel::detectCores() - 1,
                                    timeout = 600,
                                    file_names = NULL) {

  base_url <- "https://data.ceda.ac.uk/neodc/esacci/biomass/data/agb/maps"

  esacci_args <- validate_esacci_biomass_args(esacci_biomass_year, esacci_biomass_version)
  esacci_biomass_year <- esacci_args$esacci_biomass_year
  esacci_biomass_version <- esacci_args$esacci_biomass_version

  # Check if output directory exists, if not create it
  if (!dir.exists(esacci_folder)) {
    dir.create(esacci_folder, recursive = TRUE)
    message(paste("Created output directory:", esacci_folder))
  }

  # Construct URL
  url <- file.path(base_url, esacci_biomass_version, "geotiff", as.character(esacci_biomass_year))

  # Fetch file list
  page <- read_html(url)
  file_table <- html_table(page, fill = TRUE)[[1]]
  available_files <- file_table$X1

  # If specific file_names are provided, use those. Otherwise, use all available files.
  if (!is.null(file_names)) {
    file_names <- intersect(file_names, available_files)
    if (length(file_names) == 0) {
      if (esacci_biomass_version == "v5.01") {

        # Create versions with 5.0 and 5.01
        v5_0 <- gsub("-fv[0-9.]+", "-fv5.0", file_names)
        v5_01 <- gsub("-fv[0-9.]+", "-fv5.01", file_names)

        # Combine both versions
        result <- c(v5_0, v5_01)

        file_names <- intersect(result, available_files)

      } else {
        stop("None of the specified file names are available for download.")
      }

    }
  } else {
    file_names <- available_files
  }

  # Download function
  download_file <- function(file_name) {
    options(timeout = max(timeout, getOption("timeout")))

    file_url <- file.path(url, file_name)
    output_file <- file.path(esacci_folder, file_name)
    download.file(file_url, output_file, mode = "wb", quiet = TRUE)
    return(output_file)
  }

  message(paste0("Downloading ", length(file_names), " ESA CCI Biomass ", esacci_biomass_version, " file(s) for year ", esacci_biomass_year, "..."))

  # Setup parallel processing with progress bar
  cl <- makeCluster(n_cores)

  # Parallel download with progress and error handling
  downloaded_files <- pblapply(file_names, function(file_name) {
    tryCatch({
      download_file(file_name)
    }, error = function(e) {
      warning(paste("Failed to download:", file_name, "-", e$message))
      return(file.path(esacci_folder, file_name))
    })
  }, cl = cl)

  stopCluster(cl)

  # Check which files actually exist and return only those
  downloaded_files <- unlist(downloaded_files)
  existing_files <- downloaded_files[file.exists(downloaded_files)]

  return(existing_files)
}



# Helper function used for args validation
validate_esacci_biomass_args <- function (esacci_biomass_year, esacci_biomass_version) {

  # Validate and process input parameters
  valid_years <- c(2010, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
  valid_versions <- c("v2.0", "v3.0", "v4.0", "v5.0", "v5.01")
  valid_years_v4 <- c(2010, 2017, 2018, 2019, 2020)
  valid_years_v2_v3 <- c(2010, 2017, 2018)

  if (esacci_biomass_year == "latest") esacci_biomass_year <- max(valid_years)
  if (esacci_biomass_version == "latest") esacci_biomass_version <- valid_versions[length(valid_versions)]

  if (!(esacci_biomass_year %in% valid_years)) stop("Invalid year specified, valid years: ", paste(valid_years, collapse = " "))
  if (!(esacci_biomass_version %in% valid_versions)) stop("Invalid version specified")

  if (esacci_biomass_version == "v2.0" | esacci_biomass_version == "v3.0") {
    if (!(esacci_biomass_year %in% valid_years_v2_v3)) stop("Invalid year specified for v2.0 or v3.0, valid years: ", paste(valid_years_v2_v3, collapse = " "))
  }

  if (esacci_biomass_version == "v4.0") {
    if (!(esacci_biomass_year %in% valid_years_v4)) stop("Invalid year specified for v4.0, valid years: ", paste(valid_years_v4, collapse = " "))
  }

  return(
    list(
      "esacci_biomass_year" = esacci_biomass_year,
      "esacci_biomass_version" = esacci_biomass_version
    )
  )
}








# Tests:

# test_that("AGBtileNames and download_esacci_biomass functions behaves consistently", {
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
#   result_amazon <- ESACCIAGBtileNames(amazon_pol_sf, esacci_biomass_year=2021, esacci_biomass_version="v5.01")
#   result_mexico <- ESACCIAGBtileNames(mexico_pol_sf, esacci_biomass_year=2021, esacci_biomass_version="v5.01")
#
#   dwn_amazon <- download_esacci_biomass(file_names = result_amazon)
#   dwn_mexico <- download_esacci_biomass(file_names = result_mexico)
#
#   expect_equal(dwn_amazon, "data/ESACCI-BIOMASS/N00W070_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2021-fv5.0.tif")
#   expect_equal(dwn_mexico, c("data/ESACCI-BIOMASS/N20W110_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2021-fv5.0.tif",
#                              "data/ESACCI-BIOMASS/N20W100_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2021-fv5.0.tif"))
#
# })



