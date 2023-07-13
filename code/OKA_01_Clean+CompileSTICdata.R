## OKA_01_Clean+CompileSTICdata.R

source(file.path("code", "paths+packages.R"))

# path to QAQCed data
path_data <- "G:/.shortcut-targets-by-id/1KSx3E1INg4hQNlHWYnygPi41k_ku66fz/Track 2 AIMS/QA QCed Data/GP_STIC_QAQC/OKA_STIC_QAQC/"

# list of files
data_files <- list.files(path_data, pattern = ".csv")

# load all files
for (i in 1:length(data_files)){
  df_f <- read_csv(file.path(path_data, data_files[i])) |> 
    dplyr::select(datetime, siteID, SN, sublocation, condUncal, tempC, SpC, wetdry, qual_rating, QAQC)
  
  if (i == 1){
    df_all <- df_f
  } else {
    df_all <- bind_rows(df_all, df_f)
  }
}

df_all |> 
  group_by(datetime) |> 
  summarize(n_stic = sum(!is.na(wetdry)),
            prc_wet = sum(wetdry == "wet")/n_stic,
            prc_exc = sum(qual_rating == "excellent", na.rm = T)/n_stic) |> 
  ggplot(aes(x = datetime, y = n_stic)) + geom_line()

## step 1: get rid of any NA classifications (these are associated with non-data logs in the record)
df_trimmed <- subset(df_all, !is.na(wetdry) & datetime >= ymd("2022-02-15"))

## step 2: find any datetime/site with more than one STIC reading - 
#  these can then be manually adjusted and the above code can be rerun to re-generate df_trimmed
dups <- which(duplicated(df_trimmed[,c("datetime", "siteID")]))
# none exist

## step 3: find any sites that were off time (not logging at the right timestep interval)
# create 15 minute time series of all datetimes
ts_full <- seq(min(df_trimmed$datetime), max(df_trimmed$datetime), by = "15 min")
offtime <- which(!(df_trimmed$datetime %in% ts_full))
# none exist

## step 4: create/inspect summary stats
df_stats <-
  df_trimmed |> 
  #subset(qual_rating %in% c("excellent", "good")) |> 
  #subset(sublocation == "HS") |> 
  group_by(datetime) |> 
  summarize(n_stic = sum(!is.na(wetdry)),
            prc_wet = sum(wetdry == "wet")/n_stic,
            prc_exc = sum(qual_rating == "excellent", na.rm = T)/n_stic)

ggplot(df_stats, aes(x = datetime, y = n_stic)) + geom_line()
ggplot(df_stats, aes(x = datetime, y = prc_wet)) + geom_line()
ggplot(df_stats, aes(x = datetime, y = prc_exc)) + geom_line()

## step 5: save output by year
# 2022
df_trimmed |> 
  subset(year(datetime) == 2022) |> 
  mutate(condUncal = round(condUncal, 1),
         tempC = round(tempC, 2),
         SpC = round(SpC, 2),
         QAQC = replace_na(QAQC, "")) |> 
  write_csv(file.path(path_data, "..", paste0("OKA_AllSTICsCleaned_20220215-20221025.csv")))
