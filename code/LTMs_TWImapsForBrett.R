## LTMs_TWImapsForBrett.R
# This script is intended to map all the LTMs and their corresponding TWI values.
# For Brett:
#  For Konza, could we do one graph with the all STICS and one graph with just LTM's? 
#  And for Youngmeyer and Oka'Yanahli, just the LTM's  will work.

source(file.path("code", "paths+packages.R"))
library(sf)
library(raster)

# location of site info files that have lat/long
dir_sites <- "G:/.shortcut-targets-by-id/1KSx3E1INg4hQNlHWYnygPi41k_ku66fz/Track 2 AIMS/QA QCed Data/STIC/GP/GP_STIC_locations"

## first load KNZ and make 2 maps
df_knz <- 
  read_csv(file.path(dir_sites, "KNZ_SiteInfo_WithMetadata.csv"))
sf_knz <- st_as_sf(df_knz, coords = c("long", "lat"), crs = st_crs(4326))

sf_knz_streams <- st_read(file.path("results", "Konza_StreamNetwork.shp"))

knz_ltms <- c("04M01", "SFM07", "02M02", "04M03", "04M05", "04M09", "SFM01")

## youngmeyer
sf_ymr_streams <- st_read(file.path("results", "Youngmeyer_StreamNetwork.shp"))
ymr_ltms <- c("EN201", "EN301", "ENM01", "ENM04", "ENM06", "ENM08", "ENM11")
sf_ymr <-
  read_csv(file.path(dir_sites, "YMR_SiteInfo_WithMetadata.csv")) |> 
  st_as_sf(coords = c("long", "lat"), crs = st_crs(sf_knz)) |> 
  subset(siteID %in% ymr_ltms)


## oka yanahli
sf_oka_streams <- st_read(file.path("results", "OkaYanahli_StreamNetwork.shp"))
oka_ltms <- c("OKM01", "OKM05", "OKM09", "OKM12", "OKM15", "OKM16", "OKT05")

sf_oka <-
  read_csv(file.path(dir_sites, "OKA_SiteInfo_WithMetadata.csv")) |> 
  st_as_sf(coords = c("long", "lat"), crs = st_crs(sf_knz)) |> 
  subset(siteID %in% oka_ltms)



## maps
TWImin <- min(c(sf_knz$TWI, sf_ymr$TWI, sf_oka$TWI))
TWImax <- max(c(sf_knz$TWI, sf_ymr$TWI, sf_oka$TWI))

p_knz_all <-
  ggplot() +
  geom_sf(data = sf_knz_streams, color = col.cat.blu) +
  geom_sf(data = sf_knz, aes(color = TWI)) +
  scale_color_viridis_c(direction = -1, limits = c(TWImin, TWImax),
                        breaks = seq(10, 25, 5)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01),
                     breaks = seq(-96.6, -96.57, 0.01)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01),
                     breaks = seq(39.07, 39.1, 0.01)) +
  labs(title = "(a) South Fork Kings Creek - All STICs")
p_knz_all

p_knz_ltm <-
  ggplot() +
  geom_sf(data = sf_knz_streams, color = col.cat.blu) +
  geom_sf(data = subset(sf_knz, siteID %in% knz_ltms), aes(color = TWI)) +
  scale_color_viridis_c(direction = -1, limits = c(TWImin, TWImax),
                        breaks = seq(10, 25, 5)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01),
                     breaks = seq(-96.6, -96.57, 0.01)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01),
                     breaks = seq(39.07, 39.1, 0.01)) +
  labs(title = "(b) South Fork Kings Creek - LTM Only")
p_knz_ltm

p_ymr_ltm <-
  ggplot() +
  geom_sf(data = sf_ymr_streams, color = col.cat.blu) +
  geom_sf(data = subset(sf_ymr, siteID %in% ymr_ltms), aes(color = TWI)) +
  scale_color_viridis_c(direction = -1, limits = c(TWImin, TWImax),
                        breaks = seq(10, 25, 5)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.001),
                     breaks = seq(37.560, 37.568, 0.004)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01),
                     breaks = seq(-96.5, -96.49, 0.01)) +
  labs(title = "(c) Youngmeyer Ranch LTM")
p_ymr_ltm

p_oka_ltm <-
  ggplot() +
  geom_sf(data = sf_oka_streams, color = col.cat.blu) +
  geom_sf(data = subset(sf_oka, siteID %in% oka_ltms), aes(color = TWI)) +
  scale_color_viridis_c(direction = -1, limits = c(TWImin, TWImax),
                        breaks = seq(10, 25, 5)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.001),
                     breaks = seq(34.434, 37.440, 0.003)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01),
                     breaks = seq(-96.67, -96.65, 0.01)) +
  labs(title = "(d) Oka Yanahli LTM")
p_oka_ltm

## combine
library(patchwork)
p_combo <- 
  (p_knz_all + p_knz_ltm + p_ymr_ltm + p_oka_ltm) +
  plot_layout(ncol = 2, guides = "collect")
ggsave(file.path("plots", "LTMs_TWImapsForBrett.png"),
       plot = p_combo, width = 190, height = 150, units = "mm")
ggsave(file.path("plots", "LTMs_TWImapsForBrett.pdf"),
       plot = p_combo, width = 190, height = 150, units = "mm", device = cairo_pdf)
