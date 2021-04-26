## Konza_PlaceSTICs.R
# Modified from script by Nate Jones: https://github.com/bamaecohydro/AIMS_watersheds/blob/main/R/3_STIC_Placement_Fun.R

#Load relevant packages
library(tidyverse) #join the cult
library(ggsn) # for scalebar
library(patchwork)
#install.packages("whitebox", lib=file.path("C:/Users", "samzipper", "scratch"), repos="http://R-Forge.R-project.org")
library(whitebox, lib.loc=file.path("C:/Users", "samzipper", "scratch"))
library(sf)
library(raster)
library(stars)
library(mapview)
library(htmlwidgets)

# load color palettes etc.
source(file.path("code", "paths+packages.R"))

#Define data directories
data_dir<-file.path("C:/Users", "samzipper", "OneDrive - The University of Kansas", "Research", "Kansas", "Konza")
scratch_dir<-file.path("C:/Users", "samzipper", "scratch")
output_dir<-file.path(data_dir, "watersheds+twi")

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

#EStimate location of STICS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

n_stics <- 50  # synoptic points
buffer_dist <- 100  # [m] buffer distance between STICs - cannot be closer than this

# load existing points
sf_stics <- st_read(file.path("data", "STIClocations_20210421.kml")) %>% 
  sf::st_transform(crs = p)

n_stics_current <- dim(sf_stics)[1]

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
  mutate(
    twi = extract(twi, .), 
    con_area_ha = extract(fac, .)/10000,
    elevation_m = extract(dem, .),
    slope = extract(slope, .), 
    dist = extract(dist, .))

#Define 'scaled rank' based on twi and Aws
pnts<-
  pnts %>% 
  mutate(scale_twi = scale(twi), 
         scale_dist = scale(1/dist), 
         scale_area = scale(1/con_area_ha), 
         scale_sum = scale_twi + scale_area) %>% 
         #scale_sum = scale_twi + scale_dist) %>% 
  arrange(scale_sum) %>% 
  mutate(scale_rank = seq(1, nrow(.)))

# find closest pnt to current STIC locations
sf_stics$i_closest <- NA
for (i in 1:dim(sf_stics)[1]){
  i_dist <- st_distance(sf_stics[i,], pnts)
  sf_stics$i_closest[i] <- which.min(i_dist)
}

# add in data from pnts
sf_stics[, c("twi", "con_area_ha", "elevation_m", "slope", "dist", 
           "scale_twi", "scale_dist", "scale_area", "scale_sum")] <- 
  sf::st_drop_geometry(pnts)[sf_stics$i_closest, c("twi", "con_area_ha", "elevation_m", "slope", "dist", 
                                                   "scale_twi", "scale_dist", "scale_area", "scale_sum")]

# eliminate any pnts with con_area > the WQ sensor location (the streamline extends past it)
pnts_trim <-
  pnts %>% 
  subset(con_area_ha <= max(sf_stics$con_area_ha))

#Select n points from across distribution
rank<-seq(1,nrow(pnts_trim), length.out = (n_stics - n_stics_current + 1))
rank<-rank[2:(n_stics - n_stics_current + 1)] %>% round(0)
pnts_select <-
  pnts_trim %>% filter(scale_rank %in% rank)

#Plot for testing 
p_map <-
  ggplot() +
  geom_sf(data = sheds, color = col.gray) +
  geom_sf(data = streams, color = "black") +
  geom_sf(data = sf_stics, color = col.cat.red) +
  geom_sf(data = pnts_select, color = col.cat.blu)

# plot distribution of TWI and drainage area of selected points vs all points
p_dist <-
  ggplot() +
  geom_point(data = pnts, aes(x = con_area_ha, y = twi), shape = 1, color = col.gray) +
  geom_point(data = pnts_select, aes(x = con_area_ha, y = twi), color = col.cat.blu) +
  geom_point(data = sf_stics, aes(x = con_area_ha, y = twi), color = col.cat.red) +
  scale_x_continuous(name = "Drainage Area [ha]") +
  scale_y_continuous(name = "TWI")

(p_map + p_dist) +
  plot_layout(ncol = 2) + 
  plot_annotation(title = "Distributed based on TWI and drainage area",
                  subtitle = "50 locations; red = existing STICs, blue = potential new sites") +
  ggsave(file.path("plots", "Konza_PlaceSTICs.png"),
         width = 10, height = 5, units = "in")
