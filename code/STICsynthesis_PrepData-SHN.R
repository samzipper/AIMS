## STICsynthesis_PrepData-SHN.R

## Prep workspace
# load packages
source(file.path("code", "paths+packages.R"))
library(sf)

# path to AIMS STIC data QAQCed folder
path_data <- file.path("G:/.shortcut-targets-by-id/1KSx3E1INg4hQNlHWYnygPi41k_ku66fz/Track 2 AIMS", "QA QCed Data", "STIC", "GP", "AIMS_GP_SHN_approach1_STIC")

# path to save output files needed
path_out <- file.path("C:/Users", "s947z036", "OneDrive - University of Kansas", "Research", "AIMS", "Warix_STICsynthesis")

## load all data
# site locations
df_envi <- 
  file.path("G:/.shortcut-targets-by-id/1KSx3E1INg4hQNlHWYnygPi41k_ku66fz/Track 2 AIMS", "QA QCed Data", "Environmental_Data", "GP", "ENVI_GP_SHN_V1.0.xlsx") |> 
  readxl::read_xlsx(sheet = "Final Data")

# STIC data
stic_files <- fs::dir_ls(path_data, regexp = "\\.csv$")

# load files
df_all_raw <- bind_rows(lapply(stic_files, read_csv, col_types = "cTccccnncnccc"))
df_all_raw$siteId_subloc <- paste0(df_all_raw$siteId, "-", df_all_raw$sublocation)
df_all_raw$Timezone <- "UTC"
length(unique(df_all_raw$siteId))
length(unique(df_all_raw$siteId_subloc))

# check for duplicates
dups <-
  df_all_raw |>
  dplyr::summarise(n = dplyr::n(), .by = c(datetime, siteId)) |>
  dplyr::filter(n > 1L)

# inspect
df_shb07 <- subset(df_all_raw, siteId == "SHB07" & month(datetime) == 10 & year(datetime) == 2023) |> 
  arrange(datetime)

df_all_raw_trim <- 
  df_all_raw |> 
  subset(!(SN == "21736453" & rep == "20231003-20240113" & datetime <= ymd("2023-10-17")))

dups <-
  df_all_raw_trim |>
  dplyr::summarise(n = dplyr::n(), .by = c(datetime, siteId)) |>
  dplyr::filter(n > 1L)

## save 1 CSV per watershed
# compile all data
df_sensors_all <- 
  df_all_raw_trim |> 
  dplyr::select(datetime, Timezone, raw_output = condUncal, binary_flow = wetdry, qaqc_code = QAQC, sensor_name = siteId) |> 
  arrange(sensor_name, datetime)

# write one CSV file per sensor
for (s in unique(df_sensors_all$sensor_name)){
  df_s <- subset(df_sensors_all, sensor_name == s)
  write_csv(df_s, file.path(path_out, "Sensors_SHN", paste0("Sensor_", s, "_SHN.csv")))
}

## save binary matrix
# binary matrix
df_binary <- 
  df_sensors_all |> 
  pivot_wider(id_cols = c("datetime", "Timezone"), 
              values_from = binary_flow, 
              names_from = sensor_name,
              names_sort = TRUE,
              values_fill = NA) |> 
  arrange(datetime)

write_csv(df_binary, file.path(path_out, "Binary_SHN.csv"))

## save lat/long data
df_location <-
  df_envi |> 
  subset(siteId %in% df_sensors_all$sensor_name) |> 
  dplyr::select(sensor_name = siteId, latitude = lat, longitude = long) |> 
  mutate(sensor_type = "flow_presence")

write_csv(df_location, file.path(path_out, "Locations_SHN.csv"))

## save watershed boundary
sf_watershed <- 
  st_read(file.path("results", "konzaShane_Watershed.shp")) |> 
  st_transform(crs = 4326)

st_write(sf_watershed, file.path(path_out, "SHN.shp"))

st_area(sf_watershed)
