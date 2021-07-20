## Konza_Piezos.R
# Modified from Youngmeyer_Piezos.R

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
streams$L1 <- seq(1, dim(streams)[1])

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

#EStimate location of piezos ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
  dplyr::left_join(st_drop_geometry(streams)[,c("L1", "FID")], by = "L1") %>% 
  dplyr::rename(StreamReach = FID) %>% 
  mutate(
    twi = raster::extract(twi, .), 
    con_area_ha = raster::extract(fac, .)/10000,
    elevation_m = raster::extract(dem, .),
    slope = raster::extract(slope, .), 
    dist = raster::extract(dist, .),
    pid = seq(1, nrow(.)))  # a unique identifier for each point


# manually place points based on coordinates from google maps
sf_manual <- 
  dplyr::bind_rows(
    tibble(long = c(-96.587193),
           lat = c(39.092281),
           Description = "Supersensor"),
    tibble(long = c(-96.58832556),
           lat = c(39.09084425),
           Description = "02 outlet: 02M02, downstream of weir, at StreamCLIMES STICs"),
    tibble(long = -96.578458,
           lat = 39.088683,
           Description = "SFM07, downstream of 20 and 01 junction"),
    tibble(long = -96.586195,
           lat = 39.089842,
           Description = "04 outlet: 04M01, downstream of weir and 04W"),
    tibble(long = -96.581629,
           lat = 39.079941,
           Description = "04 upstream: 04M09"),
    tibble(long = -96.5831,
           lat = 39.08502,
           Description = "04 midstream: 04M05, downstream of spring inflow"),
    tibble(long = -96.58366,
           lat = 39.08685,
           Description = "04 downstream: 04M03, upstream of weir, at StreamCLIMES STICs")
  ) %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  sf::st_transform(crs = p)

sf_manual$closest_pid <- NA
sf_manual$closest_pid_dist <- NA
for (i in 1:dim(sf_manual)[1]){
  i_dist <- as.numeric(st_distance(sf_manual[i,], pnts))
  # pid is the unique identifier in pnts
  # figure out the closest pid to each current stic
  sf_manual$closest_pid[i] <- pnts$pid[which.min(i_dist)]
  sf_manual$closest_pid_dist[i] <- i_dist[which.min(i_dist)]
}


# add a column to the pnts_all data frame
pnts_piezo <- 
  sf_manual %>% 
  dplyr::left_join(st_drop_geometry(pnts), by = c("closest_pid" = "pid")) %>% 
  dplyr::arrange(StreamReach, -con_area_ha)

# get points within primary subwatershed
reachnums_longitude <- c(28, 75, 27, 83, 26, 63, 25, 60, 24) # get by looking at streams.shp in QGIS
pnts_longitude <- subset(pnts, 
                         StreamReach %in% reachnums_longitude & 
                           con_area_ha <= pnts_piezo$con_area_ha[pnts_piezo$Description == "04 outlet: 04M01, downstream of weir and 04W"])

# determine quantile
area_ecdf <- ecdf(pnts_longitude$con_area_ha)
pnts_piezo %>% 
  subset(StreamReach %in% reachnums_longitude) %>% 
  mutate(quantile_area = area_ecdf(con_area_ha)) %>% 
  dplyr::select(Description, con_area_ha, twi, quantile_area)

## plot
p_map <-
  ggplot() +
  #geom_sf(data = sheds, color = col.cat.yel, fill = "NA") +
  geom_sf(data = streams, color = col.gray) +
  #geom_sf(data = sf_springs, aes(shape = factor(SpringSeepClass))) +
  geom_sf(data = pnts_piezo, color = "red")

# plot distribution of TWI and drainage area of selected points vs all points
p_dist <-
  ggplot() +
  geom_point(data = pnts, aes(x = con_area_ha, y = twi), shape = 1, color = col.gray) +
  geom_point(data = pnts_piezo, aes(x = con_area_ha, y = twi), color = "red") +
  scale_x_continuous(name = "Drainage Area [ha]") +
  scale_y_continuous(name = "TWI")

(p_map + p_dist) +
  plot_layout(ncol = 2, guides = "collect") + 
  ggsave(file.path("plots", "Konza_Piezos.png"),
         width = 10, height = 4, units = "in")

## mapview format
m <-
  mapview(sheds,
          alpha.regions=0.3) +
  mapview(streams) +
  mapview(pnts_piezo, zcol='twi', label='Description')
m

# export
#Save map file
mapshot(m, file.path("docs", "Konza_Piezos.html"))


## map of overall youngmeyer TWI
pnts_map <- 
  pnts_all %>% 
  dplyr::arrange(con_area_ha)
pnts_map <- pnts_map[floor(seq(from = 1, to = dim(pnts_map)[1], length.out = 500)), ]

m2<-mapview(
  sheds,
  alpha.regions=0.3) +
  mapview(streams) +
  mapview(pnts_map, zcol='twi') 
m2

#export
mapshot(m2, file.path("docs", "Youngmeyer_TWI.html"), selfcontained=T)