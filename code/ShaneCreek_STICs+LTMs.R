## ShaneCreek_STICs+LTMs.R
# Modified from Youngmeyer_STICsites+Piezos.R

#Load relevant packages
library(tidyverse) #join the cult
library(ggsn) # for scalebar
library(patchwork)
#install.packages("whitebox", lib=file.path("C:/Users", "samzipper", "scratch"), repos="http://R-Forge.R-project.org")
#library(whitebox, lib.loc=file.path("C:/Users", "samzipper", "scratch"))
library(whitebox)
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
scratch_dir<-file.path("C:/Users", "s947z036", "scratch")
output_dir<-file.path(data_dir, "watersheds+twi")

#Load DEM and pour points
dem_in<-raster(file.path(data_dir,"terrain", "dem_2m_GIS200", "GIS200.tif"))
writeRaster(dem_in, file.path(scratch_dir,"konza_dem.tif"), overwrite=T) # write to scratch directory
dem <- raster(file.path(scratch_dir,"konza_dem.tif"))

#master crs
p<-"+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# load planned LTMs
sf_ltms <- 
  read_csv(file.path("data", "ShaneCreek_LTMsDD.csv"), col_types = "ccc") |> 
  st_as_sf(coords = c("long_dd", "lat_dd"), crs = 4326)

#Create pour point
pp <- sf_ltms[sf_ltms$Type == "Supersensor", ] |> 
  st_transform(., crs = st_crs(p))

# area threshold for drainage
threshold <- 20000

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: Spatial Analysis ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Create function to create watershed shape ---------------------------------

#Smooth DEM
wbt_gaussian_filter(
  input = "konza_dem.tif", 
  output = "konza_dem_smoothed.tif",
  wd = scratch_dir)

#breach depressions
wbt_breach_depressions(
  dem =    "konza_dem_smoothed.tif",
  output = "konza_dem_breached.tif",
  fill_pits = F,
  wd = scratch_dir)

#Estimate slope
wbt_slope(
  dem ="konza_dem_breached.tif",
  output = "konza_slope.tif", 
  wd=scratch_dir
)

#Flow direction raster
wbt_d8_pointer(
  dem= "konza_dem_breached.tif",
  output ="konza_fdr.tif",
  wd = scratch_dir
)

#Flow accumulation raster
wbt_d8_flow_accumulation(
  input = "konza_dem_breached.tif",
  out_type= "cells",
  output = "konza_fac.tif",
  wd = scratch_dir
)

#Create Stream Layer
wbt_extract_streams(
  flow_accum = "konza_fac.tif",
  output = "konza_stream.tif",
  threshold = threshold,
  wd = scratch_dir
)

#estimate SCA
wbt_d8_flow_accumulation(
  input = "konza_dem_breached.tif",
  output = "konza_sca.tif",
  out_type="specific contributing area", 
  wd = scratch_dir
)

#Run TWI Function
wbt_wetness_index(
  sca    = "konza_sca.tif",
  slope  = "konza_slope.tif",
  output = "konza_twi.tif",
  wd     = scratch_dir
)

#Estimate distance to outlet
wbt_distance_to_outlet(
  d8_pntr = 'konza_fdr.tif',
  streams = "konza_stream.tif",
  output =  'konza_dist.tif', 
  wd =       scratch_dir
)

#Paste point points in scratch dir
st_write(pp, file.path(scratch_dir,"konzaShane_pp.shp"), delete_dsn = T)

#Snap pour point
wbt_jenson_snap_pour_points(
  pour_pts = "konzaShane_pp.shp", 
  streams = "konza_stream.tif",
  snap_dist = 100,
  output =  "konzaShane_snap.shp",
  wd= scratch_dir)

#delineate watershed 
wbt_watershed(
  d8_pntr = "konza_fdr.tif",
  pour_pts = "konzaShane_snap.shp", 
  output = "konzaShane_sheds.tif" ,
  wd=scratch_dir)

#load watershed raster into R env
sheds <- raster(file.path(scratch_dir,"konzaShane_sheds.tif"))

#Convert raster to vector
sheds <- 
  sheds |> 
  st_as_stars() |> 
  st_as_sf(merge = TRUE)

#Add pp ID
name <- "Konza Shane Creek"
sheds$name <-name

#B. Stream Analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Convert to polygon
wbt_raster_streams_to_vector(
  streams = "konza_stream.tif",
  d8_pntr = "konza_fdr.tif",
  output = "konzaShane_streams.shp",
  wd = scratch_dir
)

#Bring stream polygon into r environment
streams<-st_read(file.path(scratch_dir, "konzaShane_streams.shp"))
st_crs(streams)<-st_crs(dem@crs)

#Crop sheds to basin
streams<-streams[sheds,]
streams$L1 <- seq(1, dim(streams)[1])

# save to repo
st_write(streams, file.path("results", "konzaShane_StreamNetwork.shp"), append = F)

#Bring twi, fac, and slope into R env
twi<-raster(file.path(scratch_dir,"konza_twi.tif"))
twi<-crop(twi, sheds)
writeRaster(twi, file.path("results", "konzaShane_TWI.tif"), overwrite=T) # write
fac<-raster(file.path(scratch_dir,"konza_fac.tif"))
fac<-crop(fac, sheds)
writeRaster(fac, file.path("results", "konzaShane_FlowAccumulation_cells.tif"), overwrite=T) # write
slope<-raster(file.path(scratch_dir,"konza_slope.tif"))
slope<-crop(slope, sheds)
writeRaster(slope, file.path("results", "konzaShane_Slope_degrees.tif"), overwrite=T) # write
dem<-raster(file.path(scratch_dir,"konza_dem_smoothed.tif"))
dem<-crop(dem, sheds)
writeRaster(dem, file.path("results", "konzaShane_DEM_m.tif"), overwrite=T) # write
dist<-raster(file.path(scratch_dir,"konza_dist.tif"))
dist<-crop(dist, sheds)

#EStimate location of STICS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

n_stics <- 27  # 1 at each LTM, 20 more
buffer_dist <- 100  # [m] buffer distance between STICs - cannot be closer than this

#Convert stream network to point w/ TWI and Aws
pnts<-
  st_line_sample(
    x=streams, 
    density = 2) |>  # 2m=resolution of DEM
  st_coordinates() |> 
  as_tibble() |> 
  st_as_sf(
    coords  = c('X','Y'), 
    crs = st_crs(dem@crs)) |> 
  dplyr::left_join(st_drop_geometry(streams)[,c("L1", "FID")], by = "L1") |> 
  dplyr::rename(StreamReach = FID)
pnts$twi <- raster::extract(twi, pnts)
pnts$con_area_ha <- raster::extract(fac, pnts)*res(fac)[1]*res(fac)[2]/10000
pnts$elevation_m <- raster::extract(dem, pnts)
pnts$slope <- raster::extract(slope, pnts)
pnts$dist <- raster::extract(dist, pnts)
pnts$pid = seq(1, nrow(pnts))

# calculate the variables you want to use to distribute stics
pnts_all <-
  pnts #|> 
#   #subset(con_area_ha <= max(set_sites$con_area_ha)) |> 
#   mutate(scale_twi = scale(twi), 
#          scale_dist = scale(1/dist), 
#          scale_area = scale(1/con_area_ha), 
#          scale_sum = scale_twi + scale_area) |> 
#   arrange(scale_sum) %>% 
#   # split data up into n_stics equally sized groups
#   mutate(rank_group = floor(seq(from = 1, to = n_stics+0.99999, length.out = dim(.)[1])))
# #table(pnts_all$rank_group)  # check if groups area equal in size - each has ~700 points

## split drainage area into equal-interval groups, and make sure you have same # sites per group
min_area <- min(pnts_all$con_area_ha, na.rm = T)
max_area <- max(pnts_all$con_area_ha, na.rm = T)
n_groups_area <- 9

# find closest pnt to planned LTM locations
set_sites <- 
  sf_ltms |>
  st_transform(., crs = st_crs(p))
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

## check set sites and move as needed
ggplot() +
  #geom_sf(data = sheds, color = col.cat.yel, fill = "NA") +
  geom_sf(data = streams, color = col.gray) +
  #geom_sf(data = sf_springs, aes(shape = factor(SpringSeepClass))) +
  geom_sf(data = subset(pnts_all, pid %in% set_sites$closest_pid))

set_sites[, c("twi", "con_area_ha", "elevation_m", "slope", "dist")] <- 
  sf::st_drop_geometry(pnts)[match(set_sites$closest_pid, pnts$pid), 
                             c("twi", "con_area_ha", "elevation_m", "slope", "dist")]

# get rid of points downstream of supersensor
pnts_all <- subset(pnts_all, con_area_ha <= set_sites$con_area_ha[set_sites$Type == "Supersensor"])

# manually set upper breaks because there are so few points with large drainage area
breaks_area <- c(seq(min_area-0.1, 30, length.out = 5), seq(35, 85, length.out = 4), 250, 500)
#breaks_area <- c(2, 6, 10, 15, 20, 25, 32.5, 50, 120, 150)

pnts_all$area_group <- cut(pnts_all$con_area_ha, breaks = breaks_area)
table(pnts_all$area_group)

ggplot(pnts_all, aes(x = con_area_ha, y = twi)) + 
  geom_point() + 
  geom_vline(xintercept = breaks_area, color = "red") +
  scale_x_continuous(name = "Drainage Area [ha]") +
  scale_y_continuous(name = "TWI")

# calculate number of sites per group
sites_per_group <- round(n_stics/n_groups_area)

# loop through groups, figure out number of sites already in that group, and then randomly assign the rest
no_pts_count <- 0
set.seed(1)
STIC_pids <- set_sites$closest_pid
set_sites$area_group <- cut(set_sites$con_area_ha, breaks = breaks_area)

for (g in rev(levels(pnts_all$area_group))){
  #g <- rev(levels(pnts_all$area_group))[1]
  
  # figure out number of previously set sites in this group
  sites_in_group <- sum(set_sites$area_group == g)
  sites_to_place <- sites_per_group - sites_in_group  
  
  if (sites_to_place > 0){
    
    # get points in this group
    pnts_g <- 
      pnts_all |> 
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
        pnts_g |> 
        subset(quantile_group_twi == q) |> 
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
  
}

# see how many STICs are left and choose some locations to put them
stics_to_place <- n_stics - length(STIC_pids) 

# plot STIC locations - first gather them
pnts_STIC <- subset(pnts_all, pid %in% STIC_pids) |> 
  dplyr::arrange(StreamReach, -con_area_ha) |> 
  left_join(st_drop_geometry(set_sites[,c("closest_pid", "Type")]), by = c("pid"="closest_pid")) |> 
  rename(Description = Type)
pnts_STIC$Description[is.na(pnts_STIC$Description)] <- "STIC"

# test map
ggplot() +
  #geom_sf(data = sheds, color = col.cat.yel, fill = "NA") +
  geom_sf(data = streams, color = col.gray) +
  #geom_sf(data = sf_springs, aes(shape = factor(SpringSeepClass))) +
  geom_sf(data = pnts_STIC, aes(color = Description)) +
  geom_sf_text(data = pnts_STIC, aes(label = pid))

# PIDs to cut
pids_cut <- c(22365, 13841, 16082, 8639, 13038, 20600, 15575)

# update test map
ggplot() +
  #geom_sf(data = sheds, color = col.cat.yel, fill = "NA") +
  geom_sf(data = streams, color = col.gray) +
  #geom_sf(data = sf_springs, aes(shape = factor(SpringSeepClass))) +
  geom_sf(data = subset(pnts_STIC, !(pid %in% pids_cut)), aes(color = Description)) +
  geom_sf_text(data = subset(pnts_STIC, !(pid %in% pids_cut)), aes(label = pid))

# manually place points based on filling in gaps in network
sf_manual <- 
  dplyr::bind_rows(
    tibble(long = c(-96.55170853481998),
           lat = c(39.09725676232502),
           Description = "STIC"),
    tibble(long = c(-96.55091460098815),
           lat = c(39.10350128463241),
           Description = "STIC"),
    tibble(long = c(-96.55278141849116),
           lat = c(39.11012819944679),
           Description = "STIC")
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

pnts_manual <-
  subset(pnts_all, pid %in% sf_manual$closest_pid) |> 
  dplyr::arrange(StreamReach, -con_area_ha) |> 
  left_join(st_drop_geometry(sf_manual[,c("closest_pid")]), by = c("pid"="closest_pid")) |> 
  mutate(Description = "STIC")

pnts_STIC_refined <-
  subset(pnts_STIC, (!pid %in% pids_cut)) |> 
  bind_rows(pnts_manual)

# final plots
p_map <-
  ggplot() +
  #geom_sf(data = sheds, color = col.cat.yel, fill = "NA") +
  geom_sf(data = streams, color = col.gray) +
  #geom_sf(data = sf_springs, aes(shape = factor(SpringSeepClass))) +
  geom_sf(data = pnts_STIC_refined, aes(color = Description))

p_dist <-
  ggplot() +
  geom_point(data = pnts_all, aes(x = con_area_ha, y = twi), shape = 1, color = col.gray) +
  geom_point(data = pnts_STIC_refined, aes(x = con_area_ha, y = twi, color = Description)) +
  scale_x_continuous(name = "Drainage Area [ha]") +
  scale_y_continuous(name = "TWI")

p_combined <- 
  (p_map + p_dist) +
  plot_layout(ncol = 2, guides = "collect") + 
  plot_annotation(title = "Distributed based on TWI and drainage area")

ggsave(file.path("plots", "konzaShane_PlaceSTICs_Map+Dist.png"), p_combined,
       width = 10, height = 4, units = "in")

## mapview format
m <-
  mapview(sheds,
          alpha.regions=0.3) +
  mapview(streams) +
  mapview(pnts_STIC_refined, zcol='Description', label='pid')
m

# export
#Save map file
mapshot(m, file.path("docs", "konzaShane_STICs+LTMs.html"))