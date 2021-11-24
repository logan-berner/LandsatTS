# Example data prep script
# Jakob J. Assmann j.assmann@bio.au.dk 16 September 2021

# Dependencies
library(raster)
library(sf)
library(dplyr)

## Prepare Arctic DEM
# download ArcticDEM tile
download.file("https://data.pgc.umn.edu/elev/dem/setsm/ArcticDEM/mosaic/v3.0/2m/18_37/18_37_2_2_2m_v3.0.tar.gz",
              "man/manuscript/data/18_37_2_2_2m_v3.0.tar.gz")

# unpack
dem_file <- gzfile()
untar("man/manuscript/data/18_37_2_2_2m_v3.0.tar.gz")

# Load
dem <- raster("man/manuscript/data/18_37_2_2_2m_v3.0_reg_dem.tif")

dem <- raster("C:/Users/Logan/My Drive/research/code/lsatTS/man/manuscript/data/disko_arcticDEM_crop.tif")

## WorldView
# WorldView image for Disko Island acquired on 14 July 2019, purchased by Signe Normand

# Load data
wv <- stack("man/manuscript/data/19JUL14160357-M2AS-010456716010_01_P001.TIF")

## Sentinel 2 image 
# Cloud free from 30 July 2018
s2 <- stack("man/manuscript/data/T21WXS_20180730T154911_TCI.tif")

## Crop data to common extent

# Set extent of area of interest
area_extent <- st_polygon(list(matrix(
  c(-332950,-2243300,
    -334950,-2243300,
    -334950,-2245300,
    -332950,-2245300,
    -332950,-2243300),
  ncol = 2, byrow = T))) %>%
  st_sfc(crs = st_crs(dem))

# Save extent of interest
write_sf(area_extent, "man/manuscript/data/aoi_extent.kml")

# Crop image stacks
dem_crop <- crop(dem, as_Spatial(area_extent))
wv_crop <- crop(wv, as_Spatial(st_transform(area_extent, st_crs(wv))))
s2_crop <- crop(s2, as_Spatial(st_transform(area_extent, st_crs(s2))))

# Visual check
plot(dem_crop)
plotRGB(wv_crop, 5, 3, 2)
plotRGB(s2_crop)

# Write out image stacks
writeRaster(wv_crop, "man/manuscript/data/disko_wv_2019_crop.tif")
writeRaster(dem_crop, "man/manuscript/data/disko_arcticDEM_crop.tif")        
writeRaster(s2_crop, "man/manuscript/data/disko_s2_crop.tif")
