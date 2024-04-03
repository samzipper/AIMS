## KNZ_04_CompileLongTermMetrics.R
# Goal: Create average wet/dry probability and temperature over the full 
#       period of record (requested by Charli, 8/25/2023).

source(file.path("code", "paths+packages.R"))
library(patchwork)

# path to QAQCed data
path_data <- "G:/.shortcut-targets-by-id/1KSx3E1INg4hQNlHWYnygPi41k_ku66fz/Track 2 AIMS/QA QCed Data/GP_STIC_QAQC/"

# load data and combine
df_2021 <- read_csv(file.path(path_data, "KNZ_AllSTICsCleaned_20210522-20211231.csv")) |> 
  mutate(site = str_sub(siteID, 1, 5)) |> 
  mutate(datetime_central = with_tz(datetime, tzone = "America/Chicago"))

df_2022 <- read_csv(file.path(path_data, "KNZ_AllSTICsCleaned_20220101-20221231.csv")) |> 
  mutate(site = str_sub(siteID, 1, 5)) |> 
  mutate(datetime_central = with_tz(datetime, tzone = "America/Chicago"))

df <- bind_rows(df_2021, df_2022) |> 
  subset(qual_rating != "poor")

# calculate max possible timesteps
n_ts <- length(unique(df$datetime_central))

# summarize by siteID
df_siteID <-
  df |> 
  group_by(siteID) |> 
  summarize(n_wet = sum(wetdry == "wet"),
            n_total = n(),
            prc_missing = (n_ts - n_total)/n_ts,
            prc_wet = n_wet/n_total,
            tempC_mean = mean(tempC))

# save output
write_csv(df_siteID, file.path("results", "KNZ_Avg2021-2022STICstatsNoPoor.csv"))


# summarize by siteID - first year of data only
df_siteID_1yr <-
  df |> 
  subset(datetime <= df$datetime[1] + years(1)) |> 
  group_by(siteID) |> 
  summarize(n_wet = sum(wetdry == "wet"),
            n_total = n(),
            prc_missing = (n_ts - n_total)/n_ts,
            prc_wet = n_wet/n_total,
            tempC_mean = mean(tempC))

# save output
write_csv(df_siteID_1yr, file.path("results", "KNZ_FirstYearSTICstatsNoPoor.csv"))

# compare
left_join(df_siteID_1yr, df_siteID, by = "siteID", suffix = c("_1yr", "_all")) |> 
  ggplot(aes(x = prc_wet_1yr, y = prc_wet_all)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1)
