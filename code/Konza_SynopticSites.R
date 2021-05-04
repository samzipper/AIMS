## Konza_SynopticSites.R
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

#EStimate location of STICS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

n_stics <- 50  # synoptic points
buffer_dist <- 100  # [m] buffer distance between STICs - cannot be closer than this

# load existing points
current_stics <- st_read(file.path("data", "STIClocations_20210421.kml")) %>% 
  sf::st_transform(crs = p) %>% 
  st_zm() %>% 
  dplyr::mutate(Description = "STIC")

# key sites that we want to put stics
key_sites <- read_csv(file.path("data", "KeySites.csv")) %>% 
  sf::st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>% 
  sf::st_transform(crs = p) %>% 
  dplyr::mutate(Description = "Weir")

set_sites <- dplyr::bind_rows(current_stics, key_sites)

n_stics_current <- dim(set_sites)[1]

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

# find closest pnt to current STIC locations
set_sites$closest_pid <- NA
set_sites$closest_pid_dist <- NA
for (i in 1:dim(set_sites)[1]){
  i_dist <- as.numeric(st_distance(set_sites[i,], pnts))
  # pid is the unique identifier in pnts
  # figure out the closest pid to each current stic
  set_sites$closest_pid[i] <- pnts$pid[which.min(i_dist)]
  set_sites$closest_pid_dist[i] <- i_dist[which.min(i_dist)]
  
  # drop all scale_ranks that are within buffer distance, except the closest one
  pid_drop <- pnts$pid[i_dist <= buffer_dist]
  
  # trim pnts
  if (i == 1){
    pid_drop_all <- pid_drop
  } else {
    pid_drop_all <- c(pid_drop_all, pid_drop)
  }
}

# remove the closest scale rank to each point from the drop list, but don't drop from pnts yet
pid_drop_all <- pid_drop_all[!(pid_drop_all %in% set_sites$closest_pid)]

# add in data from pnts
set_sites[, c("twi", "con_area_ha", "elevation_m", "slope", "dist")] <- 
  sf::st_drop_geometry(pnts)[match(set_sites$closest_pid, pnts$pid), 
                             c("twi", "con_area_ha", "elevation_m", "slope", "dist")]

# eliminate pnts with con_area > the WQ sensor location (the streamline extends past it)
# and calculate the variables you want to use to distribute stics
pnts_all <-
  pnts %>% 
  subset(con_area_ha <= max(set_sites$con_area_ha)) %>% 
  mutate(scale_twi = scale(twi), 
         scale_dist = scale(1/dist), 
         scale_area = scale(1/con_area_ha), 
         scale_sum = scale_twi + scale_area) %>% 
  #scale_sum = scale_twi + scale_dist) %>% 
  arrange(scale_sum) %>% 
  # split data up into n_stics equally sized groups
  mutate(rank_group = floor(seq(from = 1, to = n_stics+0.99999, length.out = dim(.)[1])))
#table(pnts_trim$rank_group)  # check if groups area equal in size - each has 576 or 577 points

twi_ecdf <- ecdf(pnts_all$twi)
dist_ecdf <- ecdf(pnts_all$dist)
area_ecdf <- ecdf(pnts_all$con_area_ha)

# calculate quantiles
pnts_all$quantile_twi <- twi_ecdf(pnts_all$twi)
pnts_all$quantile_dist <- dist_ecdf(pnts_all$dist)
pnts_all$quantile_area <- area_ecdf(pnts_all$con_area_ha)

# split quantiles up into breaks
n_breaks <- 5
quantile_breaks <- seq(0, 1, length.out = n_breaks+1)
pnts_all$quantile_group_twi <- cut(pnts_all$quantile_twi, breaks = quantile_breaks, include.lowest = T)
pnts_all$quantile_group_dist <- cut(pnts_all$quantile_dist, breaks = quantile_breaks, include.lowest = T)
pnts_all$quantile_group_area <- cut(pnts_all$quantile_area, breaks = quantile_breaks, include.lowest = T)

# combined twi and area quantile
pnts_all$quantile_group_select <- paste0(pnts_all$quantile_group_twi, "_", pnts_all$quantile_group_area)

# first, eliminate the groups that already have a stic based on the current locations, 
# and all points within the buffer distance of those
pnts_trim <- 
  pnts_all %>% 
  subset(!(rank_group %in% pnts_all$rank_group[pnts_all$pid %in% set_sites$closest_pid])) %>% 
  subset(!(pid %in% pid_drop_all))

# define stream reaches you only want max of 1 sample on
StreamReach_limit <- c(117, 121)
ggplot(streams, aes(color = (FID %in% StreamReach_limit))) + geom_sf()

# find closest pnt to current STIC locations
# place the remaining synoptic sites
synoptic_pids <- set_sites$closest_pid
groups_left <- unique(pnts_trim$rank_group) # rank_groups that still need stics
set.seed(1)
for (i in 1:length(groups_left)){
  g <- groups_left[i] # get rank_group
  g_pid <- sample(pnts_trim$pid[pnts_trim$rank_group == g], 1)  # select a pid from this group
  g_reach <- pnts_trim$StreamReach[pnts_trim$pid == g_pid]
  synoptic_pids <- c(synoptic_pids, g_pid) # add to overall list of stics
  i_dist <- as.numeric(st_distance(pnts_trim[pnts_trim$pid == g_pid, ], pnts_trim)) # get distance from this point to all other points
  drop_dist <- which(i_dist < buffer_dist)
  
  # check if it is one of the limited stream reaches
  if (g_reach %in% StreamReach_limit){
    drop_reach <- which(pnts_trim$StreamReach == g_reach)
  } else {
    drop_reach <- integer(0)
  }
  
  i_pnts_drop <- unique(c(drop_dist, drop_reach))
  pnts_trim <- pnts_trim[-i_pnts_drop, ] # drop points that are within buffer distance or on same stream reach (if one of limited reach)
}

# add a column to the pnts_all data frame
pnts_synoptic <- subset(pnts_all, pid %in% synoptic_pids) %>% 
  mutate(Site = "New")
pnts_synoptic$Site[pnts_synoptic$pid %in% set_sites$closest_pid[set_sites$Description == "STIC"]] <- "STIC"
pnts_synoptic$Site[pnts_synoptic$pid %in% set_sites$closest_pid[set_sites$Description == "Weir"]] <- "Weir"

## plot
# load springs/seeps to put on map
sf_springs <- read_csv(file.path("data", "SFKC_SeepsSprings.csv")) %>% 
  sf::st_as_sf(coords = c("LONGdegW", "LATdegN"), crs = 4326) %>% 
  sf::st_transform(crs = p) 

p_map <-
  ggplot() +
  geom_sf(data = sheds, color = col.gray) +
  geom_sf(data = streams, color = "black") +
  geom_sf(data = sf_springs, aes(shape = factor(SpringSeepClass))) +
  geom_sf(data = pnts_synoptic, aes(color = Site)) +
  scale_color_manual(values = c("STIC" = col.cat.red, "Weir" = col.cat.org, "New" = col.cat.blu)) +
  scale_shape_discrete(name = "Spring Class")

# plot distribution of TWI and drainage area of selected points vs all points
p_dist <-
  ggplot() +
  geom_point(data = pnts_all, aes(x = con_area_ha, y = twi), shape = 1, color = col.gray) +
  geom_point(data = pnts_synoptic, aes(x = con_area_ha, y = twi, color = Site)) +
  scale_x_continuous(name = "Drainage Area [ha]") +
  scale_y_continuous(name = "TWI") +
  scale_color_manual(values = c("STIC" = col.cat.red, "Weir" = col.cat.org, "New" = col.cat.blu))

(p_map + p_dist) +
  plot_layout(ncol = 2, guides = "collect") + 
  plot_annotation(title = "Distributed based on TWI and drainage area",
                  subtitle = "50 locations; red = existing STICs, orange = weirs, blue = potential new sites") +
  ggsave(file.path("plots", "Konza_PlaceSTICs_Map+Dist.png"),
         width = 10, height = 4, units = "in")

# k-s test and ecdfs for drainage area and twi
ks_area <- ks.test(pnts_all$con_area_ha, pnts_synoptic$con_area_ha)
p_ecfd_area <-
  ggplot() + 
  stat_ecdf(data = pnts_all, aes(x = con_area_ha), geom = "step", color = col.gray) + 
  stat_ecdf(data = pnts_synoptic, aes(x = con_area_ha), geom = "step", color = col.cat.org) +
  scale_x_continuous(name = "Drainage Area [ha]") +
  scale_y_continuous(name = "Cumulative Proportion") +
  labs(subtitle = paste0("K-S test p-value = ", round(ks_area$p.value, 2)))

ks_twi <- ks.test(pnts_all$twi, pnts_synoptic$twi)
p_ecfd_twi <-
  ggplot() + 
  stat_ecdf(data = pnts_all, aes(x = twi), geom = "step", color = col.gray) + 
  stat_ecdf(data = pnts_synoptic, aes(x = twi), geom = "step", color = col.cat.org) +
  scale_x_continuous(name = "TWI [-]") +
  scale_y_continuous(name = "Cumulative Proportion") +
  labs(subtitle = paste0("K-S test p-value = ", round(ks_twi$p.value, 2)))

(p_ecfd_area + p_ecfd_twi) +
  plot_layout(ncol = 2) + 
  plot_annotation(title = "ECDFs of stream network (gray) and synoptic sites (orange)") +
  ggsave(file.path("plots", "Konza_PlaceSTICs_ECDFs.png"),
         width = 10, height = 5, units = "in")


## mapview format
m<-
  mapview(sheds,
          alpha.regions=0.3) +
  mapview(streams) +
  mapview(sf_springs, zcol = 'SpringSeepClass', col.regions = c(col.cat.red, col.cat.org, col.cat.yel), color = "white") +
  mapview(pnts_synoptic, zcol='twi')
m

# export
#Save map file

mapshot(m, file.path("docs", "Konza_Synoptic.html"))
