## STICsynthesis_PrepData-KNZ.R

## Prep workspace
# load packages
source(file.path("code", "paths+packages.R"))
library(sf)

# path to AIMS STIC data QAQCed folder
path_data <- file.path("G:/.shortcut-targets-by-id/1KSx3E1INg4hQNlHWYnygPi41k_ku66fz/Track 2 AIMS", "QA QCed Data", "STIC", "GP", "AIMS_GP_KNZ_approach1_STIC")

# path to save output files needed
path_out <- file.path("C:/Users", "s947z036", "OneDrive - University of Kansas", "Research", "AIMS", "Warix_STICsynthesis")

# SFKC --------------------------------------------------------------------


## load all data
# site locations
df_envi <- 
  file.path("G:/.shortcut-targets-by-id/1KSx3E1INg4hQNlHWYnygPi41k_ku66fz/Track 2 AIMS", "QA QCed Data", "Environmental_Data", "GP", "ENVI_GP_approach3_20210603_20210812_V2.0.xlsx") |> 
  readxl::read_xlsx(sheet = "Final Data") |> 
  subset(month == "June")

# STIC data
years_all <- seq(2021, 2025)
for (y in years_all){
  # get CSV files
  stic_files_y <- fs::dir_ls(file.path(path_data, y), regexp = "\\.csv$")
  stic_hs_y <- stic_files_y[str_sub(stic_files_y, -11, -10) %in% c("HS", "SW")]
  
  # load files
  df_all_y <- bind_rows(lapply(stic_hs_y, read_csv, col_types = "cTccccnncnccc"))
  
  if (y == 2021){
    df_all_raw <- df_all_y
  } else {
    df_all_raw <- bind_rows(df_all_raw, df_all_y)
  }
}

# issue with 01M03 in 2024 - duplicates - inspect 
df_04M03_2024 <- subset(df_all_raw, siteId == "01M03" & year(datetime) == 2024)

df_all_raw_trim <- 
  df_all_raw |> 
  subset(!(SN == "20946480" & rep == "20240524-20250307" & datetime <= ymd("2024-08-21")))

df_04M03_2024_trim <- subset(df_all_raw_trim, siteId == "01M03" & year(datetime) == 2024)

# produce columns that will be needed
df_all_raw_trim$sensor_name <- paste0(df_all_raw_trim$siteId, "-", df_all_raw_trim$sublocation)
df_all_raw_trim$Timezone <- "UTC"

length(unique(df_all_raw_trim$sensor_name))
length(unique(df_all_raw_trim$siteId))

## save 1 CSV per watershed
# compile all data
df_sensors_all <- 
  df_all_raw_trim |> 
  dplyr::select(datetime, Timezone, raw_output = condUncal, binary_flow = wetdry, qaqc_code = QAQC, sensor_name)

# write one CSV file per sensor
for (s in unique(df_sensors_all$sensor_name)){
  df_s <- subset(df_sensors_all, sensor_name == s)
  write_csv(df_s, file.path(path_out, "Sensors_SFKC", paste0("Sensor_", s, "_SFKC.csv")))
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

write_csv(df_binary, file.path(path_out, "Binary_SFKC.csv"))

# dups <- 
#   df_sensors_all |>
#   dplyr::summarise(n = dplyr::n(), .by = c(datetime, Timezone, sensor_name)) |>
#   dplyr::filter(n > 1L) 

## save lat/long data
df_location <-
  data.frame(siteId = rep(df_envi$siteId, 2),
             latitude = rep(df_envi$lat, 2),
             longitude = rep(df_envi$long, 2),
             sublocation = c(rep("SW", length(df_envi$siteId)),
                             rep("HS", length(df_envi$siteId))),
             sensor_type = "flow_presence") |> 
  mutate(sensor_name = paste0(siteId, "-", sublocation)) |> 
  subset(sensor_name %in% df_sensors_all$sensor_name) |> 
  dplyr::select(sensor_name, latitude, longitude, sensor_type)

write_csv(df_location, file.path(path_out, "Locations_SFKC.csv"))

## save watershed boundary
sf_watershed <- 
  st_read(file.path("results", "Konza_Watershed.shp")) |> 
  st_transform(crs = 4326)

st_write(sf_watershed, file.path(path_out, "SFKC.shp"))

# Subset to N04D above weir ----------------------------------------------------------

stics_N04D <- c(paste0("04M", sprintf("%02d", seq(3, 13))),
                       "04T01", "04T02")


## save 1 CSV per watershed
# compile all data
df_sensors_N04D <- 
  df_sensors_all |> 
  mutate(siteId = substr(sensor_name, 1, 5)) |> 
  subset(siteId %in% stics_N04D) |> 
  dplyr::select(-siteId)

# write one CSV file per sensor
for (s in unique(df_sensors_N04D$sensor_name)){
  df_s <- subset(df_sensors_N04D, sensor_name == s)
  write_csv(df_s, file.path(path_out, "Sensors_N04D", paste0("Sensor_", s, "_SFKC.csv")))
}

## save binary matrix
# binary matrix
df_binary_N04D <- 
  df_sensors_N04D |> 
  pivot_wider(id_cols = c("datetime", "Timezone"), 
              values_from = binary_flow, 
              names_from = sensor_name,
              names_sort = TRUE,
              values_fill = NA) |> 
  arrange(datetime)

write_csv(df_binary_N04D, file.path(path_out, "Binary_N04D.csv"))

# dups <- 
#   df_sensors_all |>
#   dplyr::summarise(n = dplyr::n(), .by = c(datetime, Timezone, sensor_name)) |>
#   dplyr::filter(n > 1L) 

## save lat/long data
df_location_N04D <-
  df_location |> 
  subset(sensor_name %in% df_sensors_N04D$sensor_name)

write_csv(df_location_N04D, file.path(path_out, "Locations_N04D.csv"))
