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
stic_dir<-file.path("C:/Users", "samzipper", "OneDrive - The University of Kansas", "Research", "AIMS-IntermittentStreams", "hydro", "STIC")
scratch_dir<-file.path("C:/Users", "samzipper", "scratch")
output_dir<-file.path(data_dir, "watersheds+twi")

#master crs
p<-"+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#Bring stream polygon into r environment
streams<-st_read(file.path("results", "StreamNetwork.shp"))
st_crs(streams)<-st_crs(dem@crs)

pnts_stic <- 
  file.path(stic_dir, "Konza_AllSTICs.csv") %>% 
  read_csv() %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  sf::st_transform(crs = p)

## mapview format
m <-
  mapview(streams) +
  mapview(pnts_stic, label = "STIC_name")
m

# export
#Save map file

mapshot(m, file.path("docs", "Konza_AllSTICs.html"))
