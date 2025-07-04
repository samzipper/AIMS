## KNZ_GetSiteMetadata.R

library(tidyverse)
library(terra)
library(sf)
library(tidyterra)
library(readxl)

# location of site info files that have lat/long
dir_sites <- "G:/.shortcut-targets-by-id/1KSx3E1INg4hQNlHWYnygPi41k_ku66fz/Track 2 AIMS/QA QCed Data/STIC/GP/GP_STIC_locations"

# load site coordinates
df_sites <- 
  read_xlsx(file.path("data", "ENVI_GP_approach3_20210603_20210812_V1.0.xlsx"), sheet = "Final Data") |> 
  dplyr::select(siteId, long, lat) |> 
  unique() |> 
  rename(siteID = siteId)
sf_sites <- st_as_sf(df_sites, coords = c("long", "lat"), crs = st_crs(4326))

# load DEM, TWI, etc.
r_dem <- rast(file.path("results", "Konza_DEM_m.tif"))
r_twi <- rast(file.path("results", "Konza_TWI.tif"))
r_flowacc <- rast(file.path("results", "Konza_FlowAccumulation_cells.tif"))
r_slope <- rast(file.path("results", "Konza_Slope_degrees.tif"))

# since lat/long of some poitns aren't exactly on mapped stream network - use 10m buffer
sf_sites_buffer <- st_buffer(sf_sites, 15)

df_sites$elevation_m <- extract(r_dem, sf_sites_buffer, fun = min)[,2]
df_sites$TWI <- extract(r_twi, sf_sites_buffer, fun = max)[,2]
df_sites$drainageArea_ha <- extract(r_flowacc, sf_sites_buffer, fun = max)[,2]*res(r_flowacc)[1]*res(r_flowacc)[2]/10000
df_sites$slope_degrees <- extract(r_slope, sf_sites_buffer, fun = mean)[,2]

ggplot() +
  geom_spatraster(data = r_flowacc) +
  geom_sf(data = sf_sites)

ggplot(df_sites, aes(x = long, y = lat, color = TWI)) +
  geom_point() +
  scale_color_viridis_c()

# save output
write_csv(df_sites, file.path(dir_sites, "KNZ_SiteInfo_WithMetadata.csv"))
