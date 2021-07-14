## OkaYanahli_STICsites.R
# Modified from Konza_SynopticSites.R

#Load relevant packages
library(tidyverse) #join the cult
library(ggsn) # for scalebar
library(patchwork)
#install.packages("whitebox", lib=file.path("C:/Users", "samzipper", "scratch"), repos="http://R-Forge.R-project.org")
library(whitebox, lib.loc=file.path("C:/Users", "samzipper", "scratch"))
library(sf)
library(raster)
library(stars)
library(mapview) # needs to be dev version:    remotes::install_github("r-spatial/mapview")
mapviewOptions(fgb = FALSE)  # for pandoc export issue
library(htmlwidgets)

# load color palettes etc.
source(file.path("code", "paths+packages.R"))

#Define data directories
data_dir<-file.path("C:/Users", "samzipper", "OneDrive - The University of Kansas", "Research", "AIMS-IntermittentStreams")
scratch_dir<-file.path("C:/Users", "samzipper", "scratch")
output_dir<-file.path(data_dir, "watersheds+twi")

# load preserve boundaries
sf_boundaries <- sf::st_read(file.path(data_dir, "OkaYanahli", "OYP Boundary", "Oka Yanahli Preserve Boundary .shp"))

# load streamclimes STICs
sf_streamclimes <-
  file.path(data_dir, "hydro", "STIC", "BlueRiverSTICs_StreamCLIMES.csv") %>% 
  readr::read_csv() %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
#master crs
p<-"+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#Load DEM
dem_in<-raster(file.path(data_dir,"OkaYanahli", "ned19_n34x50_w096x75_ok_washitariverbasin_2009", "ned19_n34x50_w096x75_ok_washitariverbasin_2009.img"))
writeRaster(dem_in, file.path(scratch_dir,"dem.tif"), overwrite=T) # write to scratch directory
dem <- 
  raster(file.path(scratch_dir,"dem.tif")) %>% 
  projectRaster(crs=p)

#Create pour point
pp<-tibble(
  x = 34.455846,
  y = -96.664498) %>% 
  st_as_sf(., coords = c("y","x"), crs=4326) %>% 
  st_transform(., crs = st_crs(p))

# area threshold for drainage
threshold <- 20000

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

#Paste pour points in scratch dir
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
name <- "OkaYanahli"
sheds$name <-name

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
st_crs(streams)<-4326


m <- 
  mapview(streams) +
  mapview(sf_boundaries) +
  mapview(sf_streamclimes, zcol = "name")
m

mapshot(m, file.path("docs", "OkaYanahli_Boundary.html"))
