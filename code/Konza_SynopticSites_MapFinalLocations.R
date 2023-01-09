## Konza_SynopticSites.R
# This script tweaks the synoptic sites identified by the Konza_SynopticSites.R script
# to update locations based on field data collection.

#Load relevant packages
library(tidyverse) #join the cult
library(patchwork)
#install.packages("whitebox", lib=file.path("C:/Users", "s947z036", "scratch"), repos="http://R-Forge.R-project.org")
library(whitebox, lib.loc=file.path("C:/Users", "s947z036", "scratch"))
library(sf)
library(raster)
library(stars)
library(mapview) # needs to be dev version:    remotes::install_github("r-spatial/mapview")
mapviewOptions(fgb = FALSE)  # for pandoc export issue
library(htmlwidgets)

# load color palettes etc.
source(file.path("code", "paths+packages.R"))

#Define data directories
data_dir<-file.path("C:/Users", "s947z036", "OneDrive - University of Kansas", "Research", "Kansas", "Konza")
stic_dir<-file.path("C:/Users", "s947z036", "OneDrive - University of Kansas", "Research", "AIMS-IntermittentStreams", "hydro", "STIC")
scratch_dir<-file.path("C:/Users", "s947z036", "scratch")
output_dir<-file.path(data_dir, "watersheds+twi")

#master crs
p<-"+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#Bring stream polygon into r environment
streams<-st_read(file.path("results", "Konza_StreamNetwork.shp"))
st_crs(streams)<-st_crs(dem@crs)

pnts_stic <- 
  file.path(stic_dir, "Konza_AllSTICs.csv") %>% 
  read_csv() %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  sf::st_transform(crs = p) %>% 
  dplyr::select(STIC_name)

## mapview format
m <-
  mapview(streams, legend = F) +
  mapview(pnts_stic, label = "STIC_name", legend = F)
m

# export
#Save map file
mapshot(m, file.path("docs", "Konza_AllSTICs.html"))

### get information about drainage area, etc.
#Load DEM and pour points
dem_in<-raster(file.path(data_dir,"terrain", "dem_2m_GIS200", "GIS200.tif"))
writeRaster(dem_in, file.path(scratch_dir,"dem.tif"), overwrite=T) # write to scratch directory
dem <- raster(file.path(scratch_dir,"dem.tif"))

#master crs
p<-"+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#Create pour point
pp<-tibble(
  x = 39.09225847,
  y = -96.58724405) %>% 
  st_as_sf(., coords = c("y","x"), crs=4326) %>% 
  st_transform(., crs = st_crs(p))

# area threshold for drainage
threshold <- 22500

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
name <- "Konza"
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
st_crs(streams)<-st_crs(dem@crs)

#Crop sheds to basin
streams<-streams[sheds,]
streams$L1 <- seq(1, dim(streams)[1])

# set stream watershed names
streams$ShedName <- "NONE"
streams$ShedName[streams$FID %in% c(30, 29, 59, 58, 57)] <- "SFM" # South Fork Main
streams$ShedName[streams$FID %in% c(117, 121)] <- "SFT" # South Fork trib
streams$ShedName[streams$FID %in% c(68, 67, 66)] <- "02M" # N02B main
streams$ShedName[streams$FID %in% c(84, 112)] <- "02T" # N02B trib
streams$ShedName[streams$FID %in% c(28, 27, 26, 25, 24)] <- "04M" # N04D main
streams$ShedName[streams$FID %in% c(83, 63, 60)] <- "04T" # N04D main
streams$ShedName[streams$FID %in% c(75)] <- "04W" # N04D west trib
streams$ShedName[streams$FID %in% c(56, 55, 54, 53)] <- "01M" # N01B main
streams$ShedName[streams$FID %in% c(81, 61, 62, 69, 73)] <- "01T" # N01B trib
streams$ShedName[streams$FID %in% c(86, 85)] <- "20M" # N20B main
streams$ShedName[streams$FID %in% c(111)] <- "20T" # N20B trib

#Bring twi, fac, and slope into R env
twi<-raster(file.path(scratch_dir,"twi.tif"))
twi<-crop(twi, sheds)
fac<-raster(file.path(scratch_dir,"fac.tif"))
fac<-crop(fac, sheds)
slope<-raster(file.path(scratch_dir,"slope.tif"))
slope<-crop(slope, sheds)
dem<-raster(file.path(scratch_dir,"dem_smoothed.tif"))
dem<-crop(dem, sheds)
dist<-raster(file.path(scratch_dir,"dist.tif"))
dist<-crop(dist, sheds)

#Convert stream network to point w/ TWI and Aws
pnts<-
  st_line_sample(
    x=streams, 
    density = 2) %>%  # 2m=resolution of DEM
  st_coordinates() %>% 
  as_tibble() %>% 
  st_as_sf(
    coords  = c('X','Y'), 
    crs = st_crs(dem@crs)) %>% 
  dplyr::left_join(st_drop_geometry(streams)[,c("L1", "FID", "ShedName")], by = "L1") %>% 
  dplyr::rename(StreamReach = FID) %>% 
  mutate(
    twi = extract(twi, .), 
    con_area_ha = extract(fac, .)*res(fac)[1]*res(fac)[2]/10000,
    elevation_m = extract(dem, .),
    slope = extract(slope, .), 
    dist = extract(dist, .),
    pid = seq(1, nrow(.)))  # a unique identifier for each point

# find closest pnt to STIC locations
pnts_stic$closest_pid <- NA
pnts_stic$closest_pid_dist <- NA
for (i in 1:dim(pnts_stic)[1]){
  i_dist <- as.numeric(st_distance(pnts_stic[i,], pnts))
  # pid is the unique identifier in pnts
  # figure out the closest pid to each current stic
  pnts_stic$closest_pid[i] <- pnts$pid[which.min(i_dist)]
  pnts_stic$closest_pid_dist[i] <- i_dist[which.min(i_dist)]
}

# add in data from pnts
pnts_stic[, c("TWI", "ContributingArea_ha", "Elevation_m", "Slope_prc")] <- 
  sf::st_drop_geometry(pnts)[match(pnts_stic$closest_pid, pnts$pid), 
                             c("twi", "con_area_ha", "elevation_m", "slope")]

# save output
pnts_stic[, c("long", "lat")] <- 
  pnts_stic %>% 
  st_transform(crs = 4326) %>% 
  st_coordinates() %>% 
  as_tibble()

pnts_stic %>% 
  st_drop_geometry() %>% 
  dplyr::select(-closest_pid, -closest_pid_dist) %>% 
  write_csv(file.path(stic_dir, "Konza_AllSTICsWithDetails.csv"))
