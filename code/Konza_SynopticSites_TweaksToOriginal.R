## Konza_SynopticSites.R
# This script tweaks the synoptic sites identified by the Konza_SynopticSites.R script
# to update locations based on field data collection.

#Load relevant packages
library(tidyverse) #join the cult
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

# set stream watershed names
streams$ShedName <- "NONE"
streams$ShedName[streams$FID %in% c(30, 29, 59, 58, 57)] <- "SFM" # South Fork Main
streams$ShedName[streams$FID %in% c(117, 121)] <- "SFT" # South Fork trib
streams$ShedName[streams$FID %in% c(68, 67, 66)] <- "02M" # N02B main
streams$ShedName[streams$FID %in% c(84, 112)] <- "02T" # N02B trib
streams$ShedName[streams$FID %in% c(28, 27, 26, 25, 24)] <- "04M" # N04D main
streams$ShedName[streams$FID %in% c(75, 83, 63, 60)] <- "04T" # N04D main
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
    con_area_ha = extract(fac, .)/10000,
    elevation_m = extract(dem, .),
    slope = extract(slope, .), 
    dist = extract(dist, .),
    pid = seq(1, nrow(.)))  # a unique identifier for each point

#Load previous synoptic site locations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pnts_original <- 
  file.path("results", "Konza_SynopticSites_20210510.csv") %>% 
  read_csv() %>% 
  arrange(SiteID_Placeholder)

pnts_new <- 
  tribble(
    ~lat,      ~long,      ~SiteID_Placeholder, ~TypeOfSite,
    # move weirs slightly downstream of actual physical structure
    39.089913, -96.588498,   32,                  "Weir",
    39.087410, -96.584568,   10,                  "Weir",
    39.087040, -96.577034,   19,                  "Weir",
    39.088418, -96.576964,   47,                  "Weir",
    39.091266, -96.586322,   14,                  "Random",  # move above road crossing
    39.091032, -96.578487,   49,                  "Random",  # move further up in watershed
    39.079947, -96.581054,   1,                   "Random"   # move back from confluence
)

# replace original points with new points
pnts_combo <- 
  bind_rows(pnts_original[-match(pnts_new$SiteID_Placeholder, pnts_original$SiteID_Placeholder), ],
            pnts_new) %>% 
  arrange(SiteID_Placeholder) %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  sf::st_transform(crs = p)

# find closest pnt to STIC locations
pnts_combo$closest_pid <- NA
pnts_combo$closest_pid_dist <- NA
for (i in 1:dim(pnts_combo)[1]){
  i_dist <- as.numeric(st_distance(pnts_combo[i,], pnts))
  # pid is the unique identifier in pnts
  # figure out the closest pid to each current stic
  pnts_combo$closest_pid[i] <- pnts$pid[which.min(i_dist)]
  pnts_combo$closest_pid_dist[i] <- i_dist[which.min(i_dist)]
}

# add in data from pnts
pnts_combo[, c("twi", "con_area_ha", "elevation_m", "slope", "dist", "ShedName")] <- 
  sf::st_drop_geometry(pnts)[match(pnts_combo$closest_pid, pnts$pid), 
                             c("twi", "con_area_ha", "elevation_m", "slope", "dist", "ShedName")] %>% 
  group_by(ShedName) %>% 
  arrange(-con_area_ha) %>% 
  mutate(ShedCount = 1:n())

# assign AIMS location id
pnts_combo$AIMS_LocationID <- paste0(pnts_combo$ShedName, sprintf("%02d", pnts_combo$ShedCount))

# write a CSV file of lat/long'
pnts_csv <- 
  pnts_combo %>% 
  st_transform(crs = 4326) %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  rename(long = X, lat = Y) %>% 
  mutate(AIMS_LocationID = pnts_combo$AIMS_LocationID,
         TypeOfSite = pnts_combo$TypeOfSite)
write_csv(pnts_csv, file.path("results", "Konza_SynopticSites_20210517-STICinstalls.csv"))




## plot
p_map <-
  ggplot() +
  #geom_sf(data = sheds, color = col.cat.yel, fill = "NA") +
  geom_sf(data = streams, color = col.gray) +
  #geom_sf(data = sf_springs, aes(shape = factor(SpringSeepClass))) +
  geom_sf(data = pnts_combo, aes(color = TypeOfSite)) +
  scale_color_manual(values = c("STIC" = col.cat.red, "Weir" = col.cat.org, "Spring/Seep" = col.cat.blu, "Random" = "black")) +
  scale_shape_discrete(name = "Spring Class")

# plot distribution of TWI and drainage area of selected points vs all points
p_dist <-
  ggplot() +
  geom_point(data = pnts_all, aes(x = con_area_ha, y = twi), shape = 1, color = col.gray) +
  geom_point(data = pnts_synoptic, aes(x = con_area_ha, y = twi, color = Site)) +
  scale_x_continuous(name = "Drainage Area [ha]") +
  scale_y_continuous(name = "TWI") +
  scale_color_manual(values = c("STIC" = col.cat.red, "Weir" = col.cat.org, "Spring/Seep" = col.cat.blu, "Random" = "black"))

(p_map + p_dist) +
  plot_layout(ncol = 2, guides = "collect") + 
  plot_annotation(title = "Distributed based on TWI and drainage area") +
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
m <-
  mapview(sheds,
          alpha.regions=0.3) +
  mapview(streams) +
  #mapview(sf_springs, zcol = 'SpringSeepClass', col.regions = c(col.cat.red, col.cat.org, col.cat.yel), color = "white") +
  mapview(pnts_synoptic, zcol='twi')
m

# export
#Save map file

mapshot(m, file.path("docs", "Konza_Synoptic.html"))
