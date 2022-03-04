# Load packages for data handling etc.
library(sf)
library(dplyr)
library(purrr)
library(data.table)
library(stringr)
library(rgee)
library(raster)
library(tidyverse)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthhires)
library(parallel)
library(terra)

# Load lsatTS package
library(lsatTS)

# Intialize the Earth Engine with rgee
ee_Initialize()

# Specify a region
test_poly <- st_polygon(
  list(matrix(c(-138.90125, 69.58413,
                -138.88988, 69.58358,
                -138.89147, 69.58095,
                -138.90298, 69.57986,
                -138.90125, 69.58413),
              ncol = 2, byrow = TRUE)))
test_poly_sf <- st_sf(st_sfc(test_poly, crs = 4326))

# Use lsat_get_pixel_centers to retrieve pixel centers and plot to a file that can be added to this documentation.
# We set plot_map to a file path (or just TRUE) to view
pixel_list_test_poly <- lsat_get_pixel_centers(test_poly_sf, plot_map = TRUE)

# Export Landsat Time-Series from GEE using lsatTS
task_list <- lsat_export_ts(pixel_list_test_poly)

# Monitor export progress, waiting for last export to hahttps://github.com/jtkerb/HiLDEN_core/blob/master/scripts/jakob/retrieve_ls8_v2.Rve finished
map(task_list, ee_monitoring)

# Copy exported file(s) to tempfolder in R using ee_drive_to_local()
temp_files <- map(task_list, ee_drive_to_local)

# Load list
lsat_ts <- read.csv(temp_files[[1]])
# (or use predownloaded data)
#lsat_ts <- read.csv("tests/ts_validation_gee_time_series.csv")

# Check distinct satellites
satellite_ids <- unique(lsat_ts$SATELLITE)
# Good the three main satellites are there.

# Set sample ids for testing (for semi cloud free)
# If not run before you code below can generate a random sample
# set.seed(1)
# lsat_ids_sample <- lsat_ts %>%
#   filter(CLOUD_COVER <= 50) %>%
#   group_by(SATELLITE) %>%
#   group_map(function(...) tibble(...) %>% pull(LANDSAT_ID) %>% sample(3))
# Or use this code to grab any pred - ownloaded lsat ids:
# list.files(donwloads_folder, ".tif", recursive = T) %>%
#   gsub(".*/(L.*_T1).*", "\\1", .) %>%
#   unique() %>%
#   paste0('"', ., '"', collapse = ',\n') %>%
#   cat()
lsat_ids_sample <- c("LC08_L1TP_066011_20180627_20180704_01_T1",
                     "LC08_L1TP_067011_20130807_20170309_01_T1",
                     "LC08_L1TP_067011_20180618_20180703_01_T1",
                     "LE07_L1TP_066011_20170616_20170713_01_T1",
                     "LE07_L1TP_067011_20010830_20160929_01_T1",
                     "LE07_L1TP_068011_20080707_20160918_01_T1",
                     "LT05_L1TP_066011_19860603_20170217_01_T1",
                     "LT05_L1TP_066011_20090805_20161022_01_T1",
                     "LT05_L1TP_067011_20090609_20160905_01_T1")


## The following code stoped working, instead the following scences
## can be ordered and downloaded from the USGS EarthExplorer:
# LC080660112018062701T1
# LC080670112013080701T1
# LC080670112018061801T1
# LE070660112017061601T1
# LE070670112001083001T1
# LE070680112008070701T1
# LT050660111986060301T1
# LT050660112009080501T1
# LT050670112009060901T1

# # Source USGS scene retrival functions from HiLDEN core to download scenes from EROS
# source("https://gist.githubusercontent.com/klmr/3551433/raw/e9f8955cdbc5239ca3237bd578d05d75f5a6e361/rs.r")
# rs("https://raw.githubusercontent.com/jtkerb/HiLDEN_core/master/scripts/jakob/retrieve_ls8_v2.R?token=ACQD6QRE3BOVYZEKGWDIXALAYHSNA", 1, 26)
# rs("https://raw.githubusercontent.com/jtkerb/HiLDEN_core/master/scripts/jakob/retrieve_ls8_v2.R?token=ACQD6QRE3BOVYZEKGWDIXALAYHSNA", 53, 604)
#
# # Log on to USGS and retrieve API key
# api_key <- EROS_login()
#
# # Get dataset ids
# data_set_ids <- lapply(c("Landsat 4-5", "Landsat 7 ", "Landsat 8"), EROS_get_dataset_id,
#        collection_name = "Collection 1",
#        level = "Level-2",
#        api_key = api_key) %>% unlist()
#
# # Add scenes to list
# mapply(function(scene_ids, dataset_id){
#   # get dataset oriduct filter ids
#   filter_ids <- EROS_get_dataset_filters(dataset_id, api_key)
#   filter_id <-  filter_ids$id[filter_ids$fieldLabel =="Landsat Product Identifier"]
#   # query scene entity ids
#   entity_ids <- lapply(scene_ids, EROS_get_lsat_entity_id,
#                        dataset_id = dataset_id,
#          filter_id = filter_id,
#          api_key = api_key) %>% unlist()
#   cat(entity_ids, sep = "\n")
#   # add scenes to scene list
#   lapply(entity_ids, EROS_add_to_scene_list,
#          scene_list_name = "lsatTS_validation_4",
#          api_key = api_key,
#          dataset_id = dataset_id)
# }, lsat_ids_sample, data_set_ids)
#
# # Submit order for "hilden_landsat_export" list on USGS Earth Explorer website:
# browseURL("https://earthexplorer.usgs.gov/order/index/")
#
# # Download order "hilden_landsat_export" using the USGS bulk download app:
# browseURL("https://dds.cr.usgs.gov/bulk")
#

# Specify downloads folder
donwloads_folder <- "/Users/au634851/Desktop/level-2-test/"

# Unpack downloads
tar_files <- list.files(donwloads_folder,
                        pattern = ".tar.gz",
                        full.names = T,
                        recursive = T)
lapply(tar_files, function(x){
  product_id <- gsub(".*/(.*)\\.tar.gz", "\\1", x)
  output_folder <- paste0(donwloads_folder, "/", product_id)
  dir.create(output_folder)
  untar(x, exdir = output_folder)
})

# List tif files
tif_files <- list.files(donwloads_folder,
                        pattern = ".tif$",
                        full.names = T,
                        recursive = T)
tif_files <- tif_files[!grepl("VCID", tif_files)]

# Get pixel coordinates
pixel_location <- pixel_list_test_poly

# Extract values for each Scene
usgs_values <- lapply(unlist(lsat_ids_sample), function(scene_id){
  # Status
  cat("Extracting scene id:", scene_id, "\n")
  # Match band file names to scene id
  scene_tifs <- tif_files[grepl(scene_id, tif_files)]
  # Extract data from bands
  scene_extracts <- lapply(scene_tifs,
                             #mc.cores = 7,
                             FUN = function(raster_file){
    # Skip qa bands etc.
    if(!grepl("band", raster_file)) return(NULL)
    # Get band id
    band_id <- gsub(".*band(.?).tif", "B\\1", raster_file)
    # Attach scene id to pixel locations
    pixel_location$LANDSAT_ID <- scene_id
    # Load raster
    band_raster <- raster(raster_file)
    # Transform pixel locations to local CRS
    pixel_location_sp <- pixel_location %>%
      st_transform(st_crs(band_raster)) %>%
      as_Spatial() %>%
      vect()
    # Convert raster to spatRaster
    band_raster <- rast(band_raster)
    names(band_raster) <- band_id
    # Extract pixel values
    pixel_location$pixel_value <- terra::extract(band_raster, pixel_location_sp)[,2]
    # Assign column names
    colnames(pixel_location) <- gsub("pixel_value", band_id, colnames(pixel_location))
    # Remove id column
    #pixel_location <- dplyr::select(pixel_location, -pixel_value.ID)
    # Status
    cat(".")
    # Return polygon
    return(st_drop_geometry(pixel_location))
  }) %>%
    # Filter out null values
    Filter(Negate(is.null),.) %>%
    # Join dataframes (by cloumn)
    reduce(full_join) %>%
    suppressMessages()
  # Status
  cat("\nFinished scene:", scene_id, "\n\n")
  return(scene_extracts)
}) %>% bind_rows()

# combine into joint dataframe
gee_values <- lsat_ts %>%
  filter(LANDSAT_ID %in% usgs_values$LANDSAT_ID) %>%
  dplyr::select(site, LANDSAT_ID, B1, B2, B3, B4, B5, B6, B7) %>%
  pivot_longer(cols = 3:9, names_to = "band", values_to = "gee_value")
usgs_values <- usgs_values %>%
  dplyr::select(site, LANDSAT_ID, B1, B2, B3, B4, B5, B6, B7) %>%
  pivot_longer(cols = 3:9, names_to = "band", values_to = "usgs_value")
combined_values <- full_join(gee_values, usgs_values)

# Calculate differences
combined_values$diff <- combined_values$gee_value - combined_values$usgs_value
combined_values[is.na(combined_values$gee_value),]$diff <- 2 # GEE NA
combined_values[is.na(combined_values$usgs_value),]$diff <- -2 # USGS NA
combined_values[is.na(combined_values$gee_value) & is.na(combined_values$usgs_value),]$diff <- 3 # USGS and GEE NA

# Calculate total percentage different
(total <- nrow(combined_values))
(total_match <- sum(combined_values$diff == 0) + sum(combined_values$diff == 3))
(total_match_percent <- round(100 * (total_match/total),2))
(total_diff <- total - total_match)
(total_diff_precent <- round(100 * (total_diff/total),2))
(total_usgs_na <- sum(combined_values$diff == -2))
(total_usgs_na_precent <- round(100 * (total_usgs_na/total),2))
(total_diff_dn <- sum(combined_values$diff == -1) + sum(combined_values$diff == 1))
(total_diff_dn_precent <- round(100 * (total_diff_dn/total),2))
# Add shorthand ID for the Landsat Scenes
combined_values$LANDSAT_ID_short <- gsub("(L.*)_L1TP.*([0-9]{8})_[0-9]{8}_01_T1",
                                         "\\1 \\2", combined_values$LANDSAT_ID)

# Plot histogram
hist_diff <- ggplot(combined_values, aes(x = diff, fill = LANDSAT_ID_short)) +
  geom_histogram(bins = 6) +
  scale_x_continuous(breaks = seq(-2,3,1),
                   labels = c("-2" = "USGS = NA",
                              "-1" = "-1",
                              "0" = "0",
                              "1" = "1",
                              "2" = "GEE = NA",
                              "3" = "GEE & USGS = NA")
                   ) +
  labs(x = "Diff. SR (GEE-USGS)", fill = "Lsat. ID & Date") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot("tests/ts_validation_hist.png", hist_diff,
          base_height = 5)

# .. by band
n_diffs <- combined_values %>% group_by(band, diff) %>%
  summarise(n = n())
hist_diff_by_band <- ggplot(combined_values, aes(x = diff, fill = LANDSAT_ID_short)) +
  geom_histogram(bins = 6) +
  #scale_y_continuous(breaks = seq(0,10,2)) +
  #scale_x_continuous(limits = c(-1.5,1.5), breaks = c(-1,0,1)) +
  scale_x_continuous(breaks = seq(-2,3,1),
                     labels = c("-2" = "USGS = NA",
                                "-1" = "-1",
                                "0" = "0",
                                "1" = "1",
                                "2" = "GEE = NA",
                                "3" = "GEE & USGS = NA")
  ) +
  labs(y = "count",
       x = "Diff. SR (GEE-USGS)", fill = "Lsat. ID & Date") +
  facet_wrap(~band, scales = "free_x") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot("tests/ts_validation_hist_by_band.png", hist_diff_by_band,
          base_height = 9,
          base_asp = 1.6)

# Plot pixel locations
pixel_location_utm <- pixel_location %>% st_transform(st_crs(raster(tif_files[4])))
bound_box <- pixel_location_utm %>%
  st_bbox()
pixel_locations <- ggplot() +
  geom_sf(data = pixel_location_utm,
          size = 0.5,
          colour = "red") +
  geom_sf(data = ne_coastline(scale = "large", "sf")) +
  coord_sf(xlim = c(bound_box["xmin"]-2500, bound_box["xmax"]+2500),
           ylim = c(bound_box["ymin"]-2500, bound_box["ymax"]+2500))
save_plot("tests/ts_validation_pixel_locations.png", pixel_locations,
          base_height = 6)

