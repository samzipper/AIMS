## Watershed+StreamDelineation.R
# Modified from script by Nate Jones: https://github.com/bamaecohydro/AIMS_watersheds/blob/main/R/2_TWI_Konza.R

#Load relevant packages
library(tidyverse) #join the cult
library(whitebox)
library(sf)
library(raster)
library(stars)

## workspace setup
## lat/long and watershed name
# # N04D weir
# lat <- 39.08737527246821
# long <- -96.58441113417668
# shed_name <- "N04D"
# # N02B weir
# lat <- 39.08977226933759
# long <- -96.58863963672495
# shed_name <- "N02B"
# # N01B weir
# lat <- 39.08670776289967
# long <- -96.57725097737058
# shed_name <- "N01B"
# N20B weir
lat <- 39.08857000774791
long <- -96.57659651837531
shed_name <- "N20B"

# area threshold for drainage to create stream network
threshold <- 22500  # SFKC: 22500 works

#master crs
p <- "+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#load DEM
dem_in <- raster(file.path("C:/Users", "s947z036", "OneDrive - University of Kansas", 
                         "Research", "Kansas", "Konza", "terrain", "dem_2m_GIS200", "GIS200.tif"))

# scratch and output save directories
scratch_dir <- file.path("C:/Users", "s947z036", "Desktop", "scratch")
output_dir <- file.path("results")

## begin processing
#Load DEM and pour points
writeRaster(dem_in, file.path(scratch_dir,"dem.tif"), overwrite=T) # write to scratch directory
dem <- raster(file.path(scratch_dir,"dem.tif"))
#Create pour point
pp<-tibble(
  x = lat,
  y = long) %>% 
  st_as_sf(., coords = c("y","x"), crs=4326) %>% 
  st_transform(., crs = st_crs(p))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: Spatial Analysis ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Create function to create watershed shape ---------------------------------

#Smooth DEM
wbt_gaussian_filter(
  input = "dem.tif", 
  output = "dem_smoothed.tif",
  wd = scratch_dir)

#breach depressions
wbt_breach_depressions(
  dem =    "dem_smoothed.tif",
  output = "dem_breached.tif",
  fill_pits = F,
  wd = scratch_dir)

#Estimate slope
wbt_slope(
  dem ="dem_breached.tif",
  output = "slope.tif", 
  wd=scratch_dir
)

#Flow direction raster
wbt_d8_pointer(
  dem= "dem_breached.tif",
  output ="fdr.tif",
  wd = scratch_dir
)

#Flow accumulation raster
wbt_d8_flow_accumulation(
  input = "dem_breached.tif",
  out_type= "cells",
  output = "fac.tif",
  wd = scratch_dir
)

# calculate cell area
cell_area <- res(dem)[1] * res(dem)[2]

#Create Stream Layer
wbt_extract_streams(
  flow_accum = "fac.tif",
  output = "stream.tif",
  threshold = threshold,
  wd = scratch_dir
)

#estimate SCA
wbt_d8_flow_accumulation(
  input = "dem_breached.tif",
  output = "sca.tif",
  out_type="specific contributing area", 
  wd = scratch_dir
)

#Run TWI Function
wbt_wetness_index(
  sca    = "sca.tif",
  slope  = "slope.tif",
  output = "twi.tif",
  wd     = scratch_dir
)

#Estimate distance to outlet
wbt_distance_to_outlet(
  d8_pntr = 'fdr.tif',
  streams = "stream.tif",
  output =  'dist.tif', 
  wd =       scratch_dir
)

#Paste point points in scratch dir
st_write(pp, file.path(scratch_dir,"pp.shp"), delete_dsn = T)

#Snap pour point
wbt_jenson_snap_pour_points(
  pour_pts = "pp.shp", 
  streams = "stream.tif",
  snap_dist = 100,
  output =  "snap.shp",
  wd= scratch_dir)

#delineate watershed 
wbt_watershed(
  d8_pntr = "fdr.tif",
  pour_pts = "snap.shp", 
  output = "sheds.tif" ,
  wd=scratch_dir)

#load watershed raster into R env
sheds<-raster(file.path(scratch_dir,"sheds.tif"))

#Convert raster to vector
sheds<- sheds %>% st_as_stars() %>% st_as_sf(., merge = TRUE)

#Add pp ID
sheds$name <- shed_name

#B. Stream Analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Convert to polygon
wbt_raster_streams_to_vector(
  streams = "stream.tif",
  d8_pntr = "fdr.tif",
  output = "streams.shp",
  wd = scratch_dir
)

#Bring stream polygon into r environment
streams<-st_read(file.path(scratch_dir, "streams.shp"))
st_crs(streams)<-crs(dem)

#Crop sheds to basin
streams<-streams[sheds,]

#Bring twi, fac, and slope into R env
twi<-raster(file.path(scratch_dir,"twi.tif"))
twi<-crop(twi, sheds)
fac<-raster(file.path(scratch_dir,"fac.tif"))
fac<-crop(fac, sheds)
slope<-raster(file.path(scratch_dir,"slope.tif"))
slope<-crop(slope, sheds)
dem<-raster(file.path(scratch_dir,"dem_smoothed.tif"))
dem<-crop(dem, sheds)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 3: Plot ------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot() +
  geom_sf(data = sheds) +
  geom_sf(data = streams, color = "blue")

plot(twi)
plot(dem)
plot(slope)

#3.2 save output --------------------------------------------------------------------

st_write(sheds, file.path(output_dir, paste0(shed_name, "_Watershed.shp")), append = F)
st_write(streams, file.path(output_dir, paste0(shed_name, "_StreamNetwork.shp")), append = F)
writeRaster(twi, file.path(output_dir, paste0(shed_name, "_TWI.tif")), overwrite=T)
writeRaster(dem, file.path(output_dir, paste0(shed_name, "_DEM_m.tif")), overwrite=T)
writeRaster(twi, file.path(output_dir, paste0(shed_name, "_Slope_degrees.tif")), overwrite=T)
