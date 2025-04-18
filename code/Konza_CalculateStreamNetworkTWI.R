## Konza_CalculateStreamNetworkTWI.R
# Modified from script by Nate Jones: https://github.com/bamaecohydro/AIMS_watersheds/blob/main/R/2_TWI_Konza.R

#Load relevant packages
library(tidyverse) #join the cult
#install.packages("whitebox", lib=file.path("C:/Users", "samzipper", "scratch"), repos="http://R-Forge.R-project.org")
library(whitebox, lib.loc=file.path("C:/Users", "samzipper", "scratch"))
library(sf)
library(raster)
library(stars)
library(mapview)
library(htmlwidgets)

#master crs
p<-"+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#Define data directories
data_dir<-file.path("C:/Users", "samzipper", "OneDrive - The University of Kansas", "Research", "Kansas", "Konza")
scratch_dir<-file.path("C:/Users", "samzipper", "scratch")
output_dir<-file.path(data_dir, "watersheds+twi")

#Load DEM and pour points
dem_in<-raster(file.path(data_dir,"terrain", "dem_2m_GIS200", "GIS200.tif"))
writeRaster(dem_in, file.path(scratch_dir,"dem.tif"), overwrite=T) # write to scratch directory
dem <- raster(file.path(scratch_dir,"dem.tif"))
#Create pour point
pp<-tibble(
  x = 39.093064,
  y = -96.588494) %>% 
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
st_write(pp, paste0(scratch_dir,"pp.shp"), delete_dsn = T)

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
sheds$name <-"Konza"

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

#Convert to point
pnts<-
  st_line_sample(
    x=streams, 
    density = 0.01) %>% 
  st_coordinates(stream_pnt) %>% 
  as_tibble() %>% 
  st_as_sf(
    coords  = c('X','Y'), 
    crs = st_crs(dem@crs)) %>% 
  mutate(
    twi = extract(twi, .), 
    con_area_ha = extract(fac, .)/10000,
    elevation_m = extract(dem, .),
    slope = extract(slope, .))

#2.6 export points
return(list(sheds, pnts, streams))

#2.2 run function~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shed<-output[[1]]
pnts<-output[[2]]
streams<-output[[3]]
export<-pnts %>% st_drop_geometry()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 3: Plot ------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.1 Create map ----------------------------------------------------------------
area_max<-max(pnts$con_area_ha, na.rm=T)
pnts<-pnts %>% mutate(area_prop =con_area_ha/area_max)
st_crs(pnts)<-st_crs(shed)

#plot
m<-mapview(
  shed,
  alpha.regions=0.3) +
  mapview(streams) +
  mapview(pnts, zcol='twi') +
  mapview(pnts, zcol='area_prop')
m

#3.2 Create Plot ---------------------------------------------------------------
p<-export %>% 
  as_tibble() %>% 
  ggplot(aes(y=twi, x = con_area_ha)) +
  geom_point(pch=19, col="grey30", alpha=70) + 
  theme_bw() + 
  xlab("Contributing Area [ha]") +
  ylab("TWI") + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) 
p

#3.3 Export --------------------------------------------------------------------
#Save map file
mapshot(m, file.path("docs", "twi_Konza.html"))

png("twi_Konza.png", height = 3, width = 3.5, units = "in", res=100)
p
dev.off()