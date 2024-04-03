# Konza_CompileDischargeData.R

library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(patchwork)
source(file.path("code", "paths+packages.R"))

# USGS: https://waterdata.usgs.gov/monitoring-location/06879650/#parameterCode=00060&period=P7D&showMedian=false
pCodes = c("00060") # discharge = 00060, stage = 00065
USGS_gage <- "06879650"  # USGS 06879650 KINGS C NR MANHATTAN, KS
daily_raw <- 
  dataRetrieval::readNWISdv(siteNumbers = USGS_gage, 
                            parameterCd = pCodes,
                            statCd = "00003") # daily mean

df_usgs <-
  daily_raw |> 
  subset(year(Date) < 2024) |> 
  mutate(discharge_m3s = X_00060_00003*(0.3048^3)) |> 
  dplyr::select(Date, discharge_m3s)

#subdaily_raw <- 
#  dataRetrieval::readNWISuv(siteNumbers = USGS_gage, 
#                            parameterCd = pCodes)

# Konza LTER
dir_lter <- "C:/Users/s947z036/OneDrive - University of Kansas/Research/AIMS-IntermittentStreams/hydro/Discharge-LTER"
df_n04d <- 
  read_csv(file.path(dir_lter, "ASD022.csv")) |> 
  mutate(Date = ymd(paste0(RECYEAR, "-", RECMONTH, "-", RECDAY)),
         discharge_m3s = as.numeric(MEANDISCHARGE)) |> 
  subset((QUAL_FLAG != 1 & INCOMPLETE_FLAG != 1 & MAINTENANCE_FLAG != 1) | is.na(QUAL_FLAG)) |> 
  dplyr::select(Date, discharge_m3s)

df_n20b <- 
  read_csv(file.path(dir_lter, "ASD042.csv")) |> 
  mutate(Date = ymd(paste0(RECYEAR, "-", RECMONTH, "-", RECDAY)),
         discharge_m3s = as.numeric(MEANDISCHARGE)) |>  
  subset((QUAL_FLAG != 1 & INCOMPLETE_FLAG != 1 & MAINTENANCE_FLAG != 1) | is.na(QUAL_FLAG)) |> 
  dplyr::select(Date, discharge_m3s)

df_n01b <- 
  read_csv(file.path(dir_lter, "ASD052.csv")) |> 
  mutate(Date = ymd(paste0(RECYEAR, "-", RECMONTH, "-", RECDAY)),
         discharge_m3s = as.numeric(MEANDISCHARGE)) |> 
  subset((QUAL_FLAG != 1 & INCOMPLETE_FLAG != 1 & MAINTENANCE_FLAG != 1) | is.na(QUAL_FLAG)) |> 
  dplyr::select(Date, discharge_m3s)

df_n02b <- 
  read_csv(file.path(dir_lter, "ASD062.csv")) |> 
  mutate(Date = ymd(paste0(RECYEAR, "-", RECMONTH, "-", RECDAY)),
         discharge_m3s = as.numeric(MEANDISCHARGE)) |> 
  subset((QUAL_FLAG != 1 & INCOMPLETE_FLAG != 1 & MAINTENANCE_FLAG != 1) | is.na(QUAL_FLAG)) |> 
  dplyr::select(Date, discharge_m3s)

# investigate
test <- 
  df_n04d |> 
  group_by(year(Date)) |> 
  summarize(n_days = n(),
            n_finite = sum(is.finite(discharge_m3s)),
            discharge_m3s_mean = mean(discharge_m3s, na.rm = T))

# join
df_all <- 
  full_join(df_usgs, df_n01b, by = "Date", suffix = c("_USGS", "_N01B")) |> 
  full_join(df_n02b, by = "Date") |> 
  rename(discharge_m3s_N02B = discharge_m3s) |> 
  full_join(df_n04d, by = "Date") |> 
  rename(discharge_m3s_N04D = discharge_m3s) |> 
  full_join(df_n20b, by = "Date") |> 
  rename(discharge_m3s_N20B = discharge_m3s)

# long form, facet plot
df_long <-
  df_all |> 
  pivot_longer(-Date)

# save output
write_csv(df_all, file.path("Konza_Discharge_USGS+Weirs.csv"))

ggplot(df_long, aes(x = Date, y = value, color = name)) +
  geom_line() +
  scale_y_log10()
