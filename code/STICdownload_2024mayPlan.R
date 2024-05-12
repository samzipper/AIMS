## STICdownload_2024mayPlan.R

library(tidyverse)
library(sf)
library(stars)
library(mapview) # needs to be dev version:    remotes::install_github("r-spatial/mapview")
mapviewOptions(fgb = FALSE)  # for pandoc export issue
library(htmlwidgets)
source(file.path("code", "paths+packages.R"))

## load file of locations
sf_knz_streams <- st_read(file.path("results", "Konza_StreamNetwork.shp"))
sf_shn_streams <- st_read(file.path("results", "konzaShane_StreamNetwork.shp"))

sf_stic <- 
  read_csv("G:/.shortcut-targets-by-id/1KSx3E1INg4hQNlHWYnygPi41k_ku66fz/Track 2 AIMS/QA QCed Data/STIC/GP/GP_STIC_locations/KNZ+SHN_CurrentSTICs2024May.csv") |> 
  st_as_sf(coords = c("long", "lat"))

## mapview format
m <-
  mapview(sf_knz_streams) +
  mapview(sf_shn_streams) +
  mapview(sf_stic, zcol='CollectTeam', label = 'siteID', col.regions = list(col.cat.red, col.cat.blu, col.cat.org, col.cat.grn))
m

table(sf_stic$CollectTeam)

# export
#Save map file

mapshot(m, file.path("docs", "STICdownload_2024mayPlan.html"))

st_write(sf_stic, "G:/.shortcut-targets-by-id/1KSx3E1INg4hQNlHWYnygPi41k_ku66fz/Track 2 AIMS/QA QCed Data/STIC/GP/GP_STIC_locations/KNZ+SHN_CurrentSTICs2024May.kml", driver = "kml", delete_dsn = TRUE)
