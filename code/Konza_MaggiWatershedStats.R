## Konza_MaggiWatershedStats.R
# Required stats:
#  - Mean and stdev annual air temperature
#  - Mean and stdev annual precip
#  - Mean and stdev runoff ratio
#  - Mean and stdev annual discharge

source(file.path("code", "paths+packages.R"))

## METEOROLOGICAL DATA
# load met data - downloaded from https://lter.konza.ksu.edu/content/awe01-meteorological-data-konza-prairie-headquarters-weather-station
df_met <- read.csv(file.path("data", "Konza_AWE012.csv"))

## STREAMFLOW DATA
# load discharge data - USGS
# USGS: https://waterdata.usgs.gov/monitoring-location/06879650/#parameterCode=00060&period=P7D&showMedian=false
pCodes = c("00060") # discharge = 00060, stage = 00065
USGS_gage <- "06879650"  # USGS 06879650 KINGS C NR MANHATTAN, KS
daily_raw <- 
  dataRetrieval::readNWISdv(siteNumbers = USGS_gage, 
                            parameterCd = pCodes,
                            statCd = "00003") # daily mean

df_usgs <-
  daily_raw |> 
  subset(year(Date) < 2025) |> 
  mutate(discharge_m3s = X_00060_00003*(0.3048^3)) |> 
  dplyr::select(Date, discharge_m3s)

## SUBSET AND PROCESS TO COMMON PERIOD - 1983 to 2024
# met data
df_met_yr <- 
  df_met |> 
  filter(RECYEAR >= 1983 & RECYEAR <= 2024) |> 
  group_by(RECYEAR) |> 
  summarise(
    precip_mm = sum(as.numeric(DPPT), na.rm = TRUE),
    precip_missing = sum(is.na(as.numeric(DPPT))),
    Tair_C = mean(as.numeric(TAVE), na.rm = TRUE),
    Tair_missing = sum(is.na(as.numeric(TAVE)))
  )

precip_mm_mean <- mean(df_met_yr$precip_mm, na.rm = TRUE)
precip_mm_stdev <- sd(df_met_yr$precip_mm, na.rm = TRUE)
Tair_C_mean <- mean(df_met_yr$Tair_C, na.rm = TRUE)
Tair_C_stdev <- sd(df_met_yr$Tair_C, na.rm = TRUE)

# streamflow data
watershed_area_m2 <- 4.44*(1.60934^2)*(1000^2) # square miles to square meters
df_usgs_yr <- 
  df_usgs |> 
  filter(year(Date) >= 1983 & year(Date) <= 2024) |> 
  group_by(year(Date)) |> 
  summarise(
    discharge_mmyr = 1000*86400*365.25*mean(discharge_m3s, na.rm = TRUE)/watershed_area_m2,
    discharge_missing = sum(is.na(discharge_m3s))
  )
  

discharge_mmyr_mean <- mean(df_usgs_yr$discharge_mmyr, na.rm = TRUE)
discharge_mmyr_stdev <- sd(df_usgs_yr$discharge_mmyr, na.rm = TRUE)

# runoff ratio
df_usgs.met_yr <- 
  df_usgs_yr |> 
  left_join(df_met_yr, by = c("year(Date)" = "RECYEAR")) |> 
  mutate(runoff_ratio = 100*discharge_mmyr/precip_mm)

runoff_ratio_mean <- mean(df_usgs.met_yr$runoff_ratio, na.rm = TRUE)
runoff_ratio_stdev <- sd(df_usgs.met_yr$runoff_ratio, na.rm = TRUE)
