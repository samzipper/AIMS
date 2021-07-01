## Youngmeyer_STICsites.R
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
data_dir<-file.path("C:/Users", "samzipper", "OneDrive - The University of Kansas", "Research", "Kansas", "Youngmeyer Ranch")
scratch_dir<-file.path("C:/Users", "samzipper", "scratch")
output_dir<-file.path(data_dir, "watersheds+twi")

#Load DEM and pour points
dem_in_N <- raster(file.path(data_dir,"terrain", "BE_2013_14SQG2060", "BE_14SQG2060.img"))
dem_in_S <- raster(file.path(data_dir,"terrain", "BE_2013_14SQG2055", "BE_14SQG2055.img"))
dem_in <- raster::merge(dem_in_N, dem_in_S)
writeRaster(dem_in, file.path(scratch_dir,"dem.tif"), overwrite=T) # write to scratch directory
dem <- raster(file.path(scratch_dir,"dem.tif"))

#master crs
p<-"+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#Create pour point
pp<-tibble(
  x = 37.563677,
  y = -96.489299) %>% 
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
name <- "Youngmeyer"
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

#EStimate location of STICS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

n_stics <- 20  # 1 at supersensor location, 19 more
buffer_dist <- 100  # [m] buffer distance between STICs - cannot be closer than this

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
    twi = extract(twi, .), 
    con_area_ha = extract(fac, .)/10000,
    elevation_m = extract(dem, .),
    slope = extract(slope, .), 
    dist = extract(dist, .),
    pid = seq(1, nrow(.)))  # a unique identifier for each point


# calculate the variables you want to use to distribute stics
pnts_all <-
  pnts %>% 
  #subset(con_area_ha <= max(set_sites$con_area_ha)) %>% 
  mutate(scale_twi = scale(twi), 
         scale_dist = scale(1/dist), 
         scale_area = scale(1/con_area_ha), 
         scale_sum = scale_twi + scale_area) %>% 
  arrange(scale_sum) %>% 
  # split data up into n_stics equally sized groups
  mutate(rank_group = floor(seq(from = 1, to = n_stics+0.99999, length.out = dim(.)[1])))
#table(pnts_all$rank_group)  # check if groups area equal in size - each has ~700 points

## split drainage area into equal-interval groups, and make sure you have same # sites per group
min_area <- min(pnts_all$con_area_ha, na.rm = T)
max_area <- max(pnts_all$con_area_ha, na.rm = T)
n_groups_area <- 10

ggplot(pnts_all, aes(x = con_area_ha, y = twi)) + geom_point() + geom_vline(xintercept = 40)

# manually set upper breaks because there are so few points with large drainage area
breaks_area <- c(seq(min_area-0.1, 30, length.out = n_groups_area - 3), 40, 100, 120, 150)
breaks_area <- c(2, 6, 10, 15, 20, 25, 32.5, 50, 120, 150)

pnts_all$area_group <- cut(pnts_all$con_area_ha, breaks = breaks_area)
table(pnts_all$area_group)

ggplot(pnts_all, aes(x = con_area_ha, y = twi)) + 
  geom_point() + 
  geom_vline(xintercept = breaks_area, color = "red") +
  scale_x_continuous(name = "Drainage Area [ha]") +
  scale_y_continuous(name = "TWI") +
  ggsave(file.path("plots", "Youngmeyer_SynopticSites_AreaGroups.png"),
         width = 8, height = 8, units = "in")

# calculate number of sites per group
sites_per_group <- n_stics/n_groups_area


# loop through groups, figure out number of sites already in that group, and then randomly assign the rest
no_pts_count <- 0
set.seed(1)
pid_drop_all <- numeric(0)
STIC_pids <- numeric(0)
for (g in rev(levels(pnts_all$area_group))){
  #g <- rev(levels(pnts_all$area_group))[1]
  
  # figure out number of previously set sites in this group
  sites_to_place <- sites_per_group
  
  # get points in this group
  pnts_g <- 
    pnts_all %>% 
    subset(area_group == g)
  
  # within that group, break twi up into quantiles
  g_twi_ecdf <- ecdf(pnts_g$twi)
  pnts_g$quantile_twi <- g_twi_ecdf(pnts_g$twi)
  pnts_g$quantile_group_twi <- cut(pnts_g$quantile_twi, breaks = seq(0, 1, length.out = sites_per_group+1), include.lowest = T)
  
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

# see how many STICs are left and choose some locations to put them
stics_to_place <- n_stics - length(STIC_pids) 

# manually place points based on filling in gaps in network
sf_manual <- 
  dplyr::bind_rows(
    tibble(long = c(-96.489299),
           lat = c(37.563677),
           Description = "Manual"),
    tibble(long = -96.495655,
           lat = 37.566182,
           Description = "Manual"),
    tibble(long = -96.502422,
           lat = 37.565530,
           Description = "Manual")
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
pnts_synoptic <- subset(pnts_all, pid %in% STIC_pids) %>% 
  dplyr::arrange(StreamReach, -con_area_ha)

pnts_synoptic$SiteID_Placeholder <- seq(1, dim(pnts_synoptic)[1], 1)

# write a CSV file of lat/long'
pnts_csv <- 
  pnts_synoptic %>% 
  st_transform(crs = 4326) %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  rename(long = X, lat = Y) %>% 
  mutate(SiteID_Placeholder = pnts_synoptic$SiteID_Placeholder)
write_csv(pnts_csv, file.path("results", "Youngmeyer_STICsites_20210701.csv"))

## plot
p_map <-
  ggplot() +
  #geom_sf(data = sheds, color = col.cat.yel, fill = "NA") +
  geom_sf(data = streams, color = col.gray) +
  #geom_sf(data = sf_springs, aes(shape = factor(SpringSeepClass))) +
  geom_sf(data = pnts_synoptic, color = "red")

# plot distribution of TWI and drainage area of selected points vs all points
p_dist <-
  ggplot() +
  geom_point(data = pnts_all, aes(x = con_area_ha, y = twi), shape = 1, color = col.gray) +
  geom_point(data = pnts_synoptic, aes(x = con_area_ha, y = twi), color = "red") +
  scale_x_continuous(name = "Drainage Area [ha]") +
  scale_y_continuous(name = "TWI")

(p_map + p_dist) +
  plot_layout(ncol = 2, guides = "collect") + 
  plot_annotation(title = "Distributed based on TWI and drainage area") +
  ggsave(file.path("plots", "Youngmeyer_PlaceSTICs_Map+Dist.png"),
         width = 10, height = 4, units = "in")

## mapview format
m <-
  mapview(sheds,
          alpha.regions=0.3) +
  mapview(streams) +
  mapview(pnts_synoptic, zcol='twi')
m

# export
#Save map file

mapshot(m, file.path("docs", "Youngmeyer_STICs.html"))
