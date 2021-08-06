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

#master crs
p<-"+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# load preserve boundaries
sf_boundaries <- sf::st_read(file.path(data_dir, "OkaYanahli", "OYP Boundary", "Oka Yanahli Preserve Boundary .shp"))

# load streamclimes STICs
sf_streamclimes <-
  file.path(data_dir, "hydro", "STIC", "BlueRiverSTICs_StreamCLIMES.csv") %>% 
  readr::read_csv() %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(crs = st_crs(p))

#Load DEM
dem_in<-raster(file.path(data_dir,"OkaYanahli", "ned19_n34x50_w096x75_ok_washitariverbasin_2009", "ned19_n34x50_w096x75_ok_washitariverbasin_2009.img"))

#Create pour point
pp<-tibble(
  x = 34.455846,
  y = -96.664498) %>% 
  st_as_sf(., coords = c("y","x"), crs=4326) %>% 
  st_transform(., crs = st_crs(p))

# this is a huge DEM. crop to within 3 km of the pour point and reproject
pp_buffer <- st_buffer(pp, dist = 3*1000)
dem_crop <- raster::crop(dem_in, st_transform(pp_buffer, crs(dem_in)))
dem_mask <- raster::mask(dem_crop, st_transform(pp_buffer, crs(dem_in))) %>% 
  projectRaster(crs=p)
writeRaster(dem_mask, file.path(scratch_dir,"dem.tif"), overwrite=T) # write to scratch directory

dem <- 
  raster(file.path(scratch_dir,"dem.tif"))

# area threshold for drainage
threshold <- 15000

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
st_crs(streams)<-p

# crop streams and streamclimes stics to just the watershed of interest
streams_watershed <- st_intersects(streams, sheds, sparse = F)[,1]
streams <- streams[streams_watershed, ]
streams$L1 <- seq(1, nrow(streams))

streamclimes_watershed <- st_intersects(sf_streamclimes, sheds, sparse = F)[,1]
sf_streamclimes <- sf_streamclimes[streamclimes_watershed, ]

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

#Estimate location of STICS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

n_stics <- 20  # 1 at supersensor location, 19 more
buffer_dist <- 100  # [m] buffer distance between STICs - cannot be closer than this

#Convert stream network to point w/ TWI and Aws
pnts<-
  streams %>% 
  st_line_sample(
    x=., 
    density = 2) %>%  # 2m=resolution of DEM
  st_coordinates() %>% 
  as_tibble() %>% 
  st_as_sf(
    coords  = c('X','Y'), 
    crs = st_crs(streams)) %>% 
  dplyr::left_join(st_drop_geometry(streams)[,c("L1", "FID")], by = "L1") %>% 
  dplyr::rename(StreamReach = FID) %>% 
  mutate(
    twi = extract(twi, .), 
    con_area_ha = extract(fac, .)/10000,
    elevation_m = extract(dem, .),
    slope = extract(slope, .), 
    dist = extract(dist, .),
    pid = seq(1, nrow(.)))  # a unique identifier for each point


# calculate the variables you want to use to distribute stics
pnts_all <-
  pnts %>% 
  mutate(scale_twi = scale(twi), 
         scale_dist = scale(1/dist), 
         scale_area = scale(1/con_area_ha), 
         scale_sum = scale_twi + scale_area) %>% 
  arrange(scale_sum)

# load existing points
current_stics <- 
  sf_streamclimes %>% 
  dplyr::mutate(Description = "StreamCLIMES STIC")

# find closest pnt to current STIC locations
current_stics$closest_pid <- NA
current_stics$closest_pid_dist <- NA
for (i in 1:dim(current_stics)[1]){
  i_dist <- as.numeric(st_distance(current_stics[i,], pnts))
  # pid is the unique identifier in pnts
  # figure out the closest pid to each current stic
  current_stics$closest_pid[i] <- pnts$pid[which.min(i_dist)]
  current_stics$closest_pid_dist[i] <- i_dist[which.min(i_dist)]
  
  # drop all scale_ranks that are within buffer distance, except the closest one
  pid_drop <- pnts$pid[i_dist <= buffer_dist]
  
  # trim pnts
  if (i == 1){
    pid_drop_all <- pid_drop
  } else {
    pid_drop_all <- c(pid_drop_all, pid_drop)
  }
}

# remove the closest pid to each point from the drop list (since we want to keep the point with the stic), but don't drop from pnts yet
pid_drop_all <- pid_drop_all[!(pid_drop_all %in% current_stics$closest_pid)]

## split drainage area into equal-interval groups, and make sure you have same # sites per group
min_area <- min(pnts_all$con_area_ha, na.rm = T)
max_area <- max(pnts_all$con_area_ha, na.rm = T)
n_groups_area <- 10

# manually set upper breaks because there are so few points with large drainage area and large gaps in drainage area distribution
breaks_area <- c(min_area-0.01, 2.25, 4.44, 8, 10, 13.27, 15, 20, 28, max_area+0.01)

ggplot() + 
  geom_point(data = pnts_all, aes(x = con_area_ha, y = twi)) + 
  geom_point(data = subset(pnts_all, pid %in% current_stics$closest_pid), aes(x = con_area_ha, y = twi), color = "red") +
  geom_vline(xintercept = breaks_area, color = "blue")

pnts_all$area_group <- cut(pnts_all$con_area_ha, breaks = breaks_area)
table(pnts_all$area_group)

# since there are a lot of STICs already in the biggest group (streamCLIMES), divvy up the other 20 STICs as follows
sites_per_group <- c(3, 3, 3, 2, 2, 2, 2, 2, 1)

# loop through groups and place STICs
set.seed(1)
STIC_pids <- numeric(0)
no_pts_count <- 0
for (i in length(levels(pnts_all$area_group)):1){
  g <- levels(pnts_all$area_group)[i]
  
  sites_to_place <- sites_per_group[i]
    
  # get points in this group
  pnts_g <- 
    pnts_all %>% 
    subset(area_group == g)
  
  # within that group, break twi up into quantiles
  g_twi_ecdf <- ecdf(pnts_g$twi)
  pnts_g$quantile_twi <- g_twi_ecdf(pnts_g$twi)
  pnts_g$quantile_group_twi <- cut(pnts_g$quantile_twi, breaks = seq(0, 1, length.out = sites_to_place+1), include.lowest = T)
  
  # figure out which quantile bands need stics
  q_need_stics <- levels(pnts_g$quantile_group_twi)
  
  # if there are multiple existing sites in a single quantile group, there will be fewer available sites 
  # then quantile groups to fill. in that case, just randomly choose
  q_get_stics <- sample(q_need_stics, sites_to_place)
  
  # place stics
  for (q in q_get_stics){
    pnts_g_q <- 
      pnts_g %>% 
      subset(quantile_group_twi == q) %>% 
      # remove points too close to existing locations
      subset(!(pid %in% pid_drop_all))
    
    if (dim(pnts_g_q)[1] > 0){
      # select a pid
      g_pid <- sample(pnts_g_q$pid, 1)  # select a pid from this group
      g_reach <- pnts_g_q$StreamReach[pnts_g_q$pid == g_pid]
      STIC_pids <- c(STIC_pids, g_pid) # add to overall list of STIC sites
      i_dist <- as.numeric(st_distance(pnts_g_q[pnts_g_q$pid == g_pid, ], pnts_all)) # get distance from this point to all other points
      drop_dist <- which(i_dist < buffer_dist)
      
      i_pnts_drop <- unique(drop_dist)
      pid_drop_all <- unique(c(pid_drop_all, pnts_all$pid[i_pnts_drop]))
    } else {
      no_pts_count <- no_pts_count + 1
      print(paste0("g = ", g, ", q = ", q, ", no points available"))
    }
  }
}

# inspect stics
m <- 
  mapview(sf_boundaries) +
  mapview(streams) +
  mapview(subset(pnts_all, pid %in% STIC_pids), label = "pid")

# points to drop, which can then be placed manually
STIC_pids <- STIC_pids[STIC_pids != 5700] # shift slightly so on TNC property
STIC_pids <- STIC_pids[STIC_pids != 1444] # shift slightly so on TNC property

# see how many STICs are left and choose some locations to put them
stics_to_place <- n_stics - length(STIC_pids)

# manually place points based on filling in gaps in network
sf_manual <- 
  dplyr::bind_rows(
    tibble(long = c(-96.665893),
           lat = c(34.454418),
           Description = "AIMS STIC"),
    tibble(long = -96.673766,
           lat = 34.442414,
           Description = "AIMS STIC"),
    tibble(long = -96.668758,
           lat = 34.449520,
           Description = "AIMS STIC")
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

STIC_pids <- c(STIC_pids, sf_manual$closest_pid)

# add a column to the pnts_all data frame
pnts_streamclimes <- 
  subset(pnts_all, pid %in% current_stics$closest_pid) %>% 
  dplyr::mutate(Description = "StreamCLIMES STIC")

pnts_STIC <- 
  subset(pnts_all, pid %in% STIC_pids) %>% 
  dplyr::mutate(Description = "AIMS STIC")

## now, figure out piezometers

# manually place points based on coordinates from google maps
sf_manual_piezo <- 
  dplyr::bind_rows(
    tibble(long = c(-96.665940),
           lat = c(34.454223),
           Description = "AIMS Piezo"),
    tibble(long = -96.667496,
           lat = 34.451846,
           Description = "AIMS Piezo"),
    tibble(long = -96.669569,
           lat = 34.449199,
           Description = "AIMS Piezo"),
    tibble(long = -96.670368,
           lat = 34.449294,
           Description = "AIMS Piezo"),
    tibble(long = -96.670116,
           lat = 34.448923,
           Description = "AIMS Piezo"),
    tibble(long = -96.674687,
           lat = 34.449649,
           Description = "AIMS Piezo"),
    tibble(long = -96.684026,
           lat = 34.448908,
           Description = "AIMS Piezo")
  ) %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  sf::st_transform(crs = p)

sf_manual_piezo$closest_pid <- NA
sf_manual_piezo$closest_pid_dist <- NA
for (i in 1:dim(sf_manual_piezo)[1]){
  i_dist <- as.numeric(st_distance(sf_manual_piezo[i,], pnts))
  # pid is the unique identifier in pnts
  # figure out the closest pid to each current stic
  sf_manual_piezo$closest_pid[i] <- pnts$pid[which.min(i_dist)]
  sf_manual_piezo$closest_pid_dist[i] <- i_dist[which.min(i_dist)]
}

pnts_piezo <- subset(pnts_all, pid %in% sf_manual_piezo$closest_pid) %>% 
  dplyr::mutate(Description = "AIMS Piezo")

pnts_sensors <-
  bind_rows(pnts_streamclimes, pnts_STIC, pnts_piezo) %>% 
  dplyr::arrange(StreamReach, -con_area_ha)

## plot
p_map <-
  ggplot() +
  #geom_sf(data = sheds, color = col.cat.yel, fill = "NA") +
  geom_sf(data = streams, color = col.gray) +
  geom_sf(data = pnts_sensors, aes(color = Description)) +
  scale_color_manual(values = c(col.cat.blu, col.cat.red, col.cat.org))

# plot distribution of TWI and drainage area of selected points vs all points
p_dist <-
  ggplot() +
  geom_point(data = pnts_all, aes(x = con_area_ha, y = twi), shape = 1, color = col.gray) +
  geom_point(data = pnts_sensors, aes(x = con_area_ha, y = twi, color = Description)) +
  scale_x_continuous(name = "Drainage Area [ha]") +
  scale_y_continuous(name = "TWI") +
  scale_color_manual(values = c(col.cat.blu, col.cat.red, col.cat.org))

p_combined <- 
  (p_map + p_dist) +
  plot_layout(ncol = 2, guides = "collect") + 
  plot_annotation(title = "Distributed based on TWI and drainage area")

ggsave(file.path("plots", "OkaYanahli_STICs+Piezos_Map+Dist.png"), p_combined, width = 10, height = 4, units = "in")

## mapview format
m <-
  mapview(sheds,
          alpha.regions=0.3) +
  mapview(streams) +
  mapview(pnts_sensors, zcol='Description')
m

# export
#Save map file
mapshot(m, file.path("docs", "OkaYanahli_STICs+Piezo.html"))