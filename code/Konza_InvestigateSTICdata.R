## Konza_InvestigateSTICdata.R

source(file.path("code", "paths+packages.R"))

# path to QAQCed data
path_data <- "G:/.shortcut-targets-by-id/1KSx3E1INg4hQNlHWYnygPi41k_ku66fz/Track 2 AIMS/QA QCed Data/GP_STIC_QAQC/KNZ_STIC_QAQC/"

# list of files
data_files <- list.files(path_data, pattern = ".csv")

# load all files
for (i in 1:length(data_files)){
  df_f <- read_csv(file.path(path_data, data_files[i])) |> 
    dplyr::select(datetime, siteID, sublocation, wetdry, qual_rating)
  
  if (i == 1){
    df_all <- df_f
  } else {
    df_all <- bind_rows(df_all, df_f)
  }
}

# get rid of everything before 2021-05-22 (STIC deployment)
df_trimmed <- subset(df_all, datetime >= ymd("2021-05-22"))

# summary stats
df_stats <-
  df_trimmed |> 
  subset(qual_rating %in% c("excellent", "good")) |> 
  subset(sublocation == "HS") |> 
  group_by(datetime) |> 
  summarize(n_stic = sum(!is.na(wetdry)),
            prc_wet = sum(wetdry == "wet")/n_stic,
            prc_exc = sum(qual_rating == "Excellent"))

# choose which datetimes to keep - anything with at least 20 data points
dt_keep <- subset(df_stats, n_stic > 20)$datetime

df_stats |> 
  subset(datetime %in% dt_keep) |> 
  ggplot(aes(x = datetime, y = prc_wet)) +
  geom_line()
