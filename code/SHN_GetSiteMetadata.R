## SHN_GetSiteMetadata.R
# This script is intended to read in a CSV file of site locations at SHN and
# extract the following metadata: elevation, TWI, drainage area.

library(tidyverse)
library(terra)
library(sf)
library(tidyterra)

# location of site info files that have lat/long
dir_sites <- "G:/.shortcut-targets-by-id/1KSx3E1INg4hQNlHWYnygPi41k_ku66fz/Track 2 AIMS/QA QCed Data/STIC/GP/GP_STIC_locations"

# load site coordinates
df_sites <- read_csv(file.path(dir_sites, "SHN_STICs.csv"))
sf_sites <- st_as_sf(df_sites, coords = c("long", "lat"), crs = st_crs(4326))

# load DEM, TWI, etc.
r_dem <- rast(file.path("results", "konzaShane_DEM_m.tif"))
r_twi <- rast(file.path("results", "konzaShane_TWI.tif"))
r_flowacc <- rast(file.path("results", "konzaShane_FlowAccumulation_cells.tif"))

# since lat/long of some poitns aren't exactly on mapped stream network - use 10m buffer
sf_sites_buffer <- st_buffer(sf_sites, 10)

df_sites$elevation_m <- extract(r_dem, sf_sites_buffer, fun = min)[,2]
df_sites$TWI <- extract(r_twi, sf_sites_buffer, fun = max)[,2]
df_sites$drainageArea_ha <- extract(r_flowacc, sf_sites_buffer, fun = max)[,2]*res(r_flowacc)[1]*res(r_flowacc)[2]/10000

ggplot() +
  geom_spatraster(data = r_flowacc) +
  geom_sf(data = sf_sites)

# save output
write_csv(df_sites, file.path(dir_sites, "SHN_SiteInfo_WithMetadata.csv"))
