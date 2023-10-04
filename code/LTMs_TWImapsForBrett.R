## LTMs_TWImapsForBrett.R
# This script is intended to map all the LTMs and their corresponding TWI values.
# For Brett:
#  For Konza, could we do one graph with the all STICS and one graph with just LTM's? 
#  And for Youngmeyer and Oka'Yanahli, just the LTM's  will work.

source(file.path("code", "paths+packages.R"))
library(sf)
library(raster)

## first load KNZ and make 2 maps
sf_knz <- 
  st_read(
    "G:/.shortcut-targets-by-id/1KSx3E1INg4hQNlHWYnygPi41k_ku66fz/Track 2 AIMS/Data [working files]/GIS/Konza/konza_stics_all_with_details.gpkg"
    ) |> 
  mutate(STIC_spot = str_sub(STIC_name, -2, -1))

sf_knz_streams <- st_read(file.path("results", "Konza_StreamNetwork.shp"))

twi_knz <- raster("results/Konza_TWI.tif")
sf_knz$TWI <- raster::extract(twi_knz, sf_knz, method = "bilinear")

knz_ltms <- c("04M01_1", "SFM07_1", "02M02_1", "04M03_1", "04M05_1", "04M09_1", "SFM01_1")

## youngmeyer
sf_ymr_streams <- st_read(file.path("results", "Youngmeyer_StreamNetwork.shp"))
sf_ymr <-
  read_csv("C:/Users/s947z036/OneDrive - University of Kansas/Research/AIMS-IntermittentStreams/Youngmeyer/Youngmeyer_Site_Names_20220414.csv") |> 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(sf_knz))

twi_ymr <- raster("results/Youngmeyer_TWI.tif")
sf_ymr$TWI <- raster::extract(twi_ymr, sf_ymr, method = "bilinear")

## oka yanahli
sf_oka_streams <- st_read(file.path("results", "OkaYanahli_StreamNetwork.shp"))
sf_oka <-
  read_csv("C:/Users/s947z036/OneDrive - University of Kansas/Research/AIMS-IntermittentStreams/OkaYanahli/OkaYanahli_STICs+Piezos_Final.csv") |> 
  st_as_sf(coords = c("long", "lat"), crs = st_crs(sf_knz))

twi_oka <- raster("results/Oka_TWI.tif")
sf_oka$TWI <- raster::extract(twi_oka, sf_oka, method = "bilinear")

## maps
TWImin <- min(c(sf_knz$TWI, sf_ymr$TWI, sf_oka$TWI))
TWImax <- max(c(sf_knz$TWI, sf_ymr$TWI, sf_oka$TWI))

p_knz_all <-
  ggplot() +
  geom_sf(data = sf_knz_streams, color = col.cat.blu) +
  geom_sf(data = subset(sf_knz, STIC_spot == "_1"), aes(color = TWI)) +
  scale_color_viridis_c(direction = -1, limits = c(TWImin, TWImax)) +
  labs(title = "(a) South Fork Kings Creek - All STICs")
p_knz_all

p_knz_ltm <-
  ggplot() +
  geom_sf(data = sf_knz_streams, color = col.cat.blu) +
  geom_sf(data = subset(sf_knz, STIC_name %in% knz_ltms), aes(color = TWI)) +
  scale_color_viridis_c(direction = -1, limits = c(TWImin, TWImax)) +
  labs(title = "(b) South Fork Kings Creek - LTM Only")
p_knz_ltm

p_ymr_ltm <-
  ggplot() +
  geom_sf(data = sf_ymr_streams, color = col.cat.blu) +
  geom_sf(data = subset(sf_ymr, Type %in% c("Piezo", "SuperSensor+STIC")), aes(color = TWI)) +
  scale_color_viridis_c(direction = -1, limits = c(TWImin, TWImax)) +
  labs(title = "(c) Youngmeyer Ranch LTM")
p_ymr_ltm

p_oka_ltm <-
  ggplot() +
  geom_sf(data = sf_oka_streams, color = col.cat.blu) +
  geom_sf(data = subset(sf_oka, sensor %in% c("Piezo", "Supersensor")), aes(color = TWI)) +
  scale_color_viridis_c(direction = -1, limits = c(TWImin, TWImax)) +
  labs(title = "(d) Oka Yanahli LTM")
p_oka_ltm

## combine
library(patchwork)
p_combo <- 
  (p_knz_all + p_knz_ltm + p_ymr_ltm + p_oka_ltm) +
  plot_layout(ncol = 2, guides = "collect")
ggsave(file.path("plots", "LTMs_TWImapsForBrett.png"),
       plot = p_combo, width = 190, height = 120, units = "mm")
