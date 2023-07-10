## KNZ_01_Clean+CompileSTICdata.R

source(file.path("code", "paths+packages.R"))

# path to QAQCed data
path_data <- "G:/.shortcut-targets-by-id/1KSx3E1INg4hQNlHWYnygPi41k_ku66fz/Track 2 AIMS/QA QCed Data/GP_STIC_QAQC/KNZ_STIC_QAQC/"

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

## step 1: get rid of everything before 2021-05-22 (STIC deployment) 
##         and any NA classifications (these are associated with non-data logs in the record)
good_data_start <- ymd("2021-05-22")
df_trimmed <- subset(df_all, datetime >= good_data_start & !is.na(wetdry))

## step 2: find any datetime/site with more than one STIC reading - 
#  these can then be manually adjusted and the above code can be rerun to re-generate df_trimmed
dups <- which(duplicated(df_trimmed[,c("datetime", "siteID")]))

# find sites with duplicates
table(df_trimmed$siteID[dups])

# look at a specific site
#df_inspect <- subset(df_trimmed, siteID == "20M02_1")
#dups_inspect <- which(duplicated(df_inspect[,c("datetime", "siteID")]))
#ggplot(df_inspect, aes(x = datetime, y = factor(SN), color = wetdry)) + geom_point()

## step 3: find any sites that were off time (not logging at the right timestep interval)
# create 15 minute time series of all datetimes
ts_full <- seq(min(df_trimmed$datetime), max(df_trimmed$datetime), by = "15 min")
offtime <- which(!(df_trimmed$datetime %in% ts_full))

# find sites with bad timestamps
df_offtime <- df_trimmed[offtime, ]
table(df_offtime$siteID) 
subset(df_offtime, siteID == "02M06_2")
# off time: 02M06_2 (logging every 10 minutes starting 2022-01-14) - for 15 and 45 timesteps, use data from 10 and 40 min (nearest neighbor)
#           04M02_2 (logging every 5 minutes starting 2022-01-14)  - delete off-time values
#           20M03_1 (logging every 10 minutes starting 2022-01-14) - for 15 and 45 timesteps, use data from 10 and 40 min (nearest neighbor)

# deal with each manually
df_04M02_2 <- 
  df_trimmed |> 
  subset(siteID == "04M02_2") |> 
  subset(datetime %in% ts_full)

df_02M06_2 <- 
  df_trimmed |> 
  subset(siteID == "02M06_2")
df_02M06_2 <- subset(df_02M06_2, minute(datetime) %in% c(0, 10, 15, 30, 40, 45))
i_10 <- which(minute(df_02M06_2$datetime) == 10)
i_40 <- which(minute(df_02M06_2$datetime) == 40)
df_02M06_2$datetime[c(i_10, i_40)] <- df_02M06_2$datetime[c(i_10, i_40)] + minutes(5)

df_20M03_1 <- 
  df_trimmed |> 
  subset(siteID == "20M03_1")
df_20M03_1 <- subset(df_20M03_1, minute(datetime) %in% c(0, 10, 15, 30, 40, 45))
i_10 <- which(minute(df_20M03_1$datetime) == 10)
i_40 <- which(minute(df_20M03_1$datetime) == 40)
df_20M03_1$datetime[c(i_10, i_40)] <- df_20M03_1$datetime[c(i_10, i_40)] + minutes(5)

# remove from df_trimmed and replace
df_trimmed_timeMatch <-
  df_trimmed |> 
  subset(siteID != "04M02_2") |> 
  bind_rows(df_04M02_2) |> 
  subset(siteID != "02M06_2") |> 
  bind_rows(df_02M06_2) |> 
  subset(siteID != "20M03_1") |> 
  bind_rows(df_20M03_1)

# check if any still offtime
which(!(df_trimmed_timeMatch$datetime %in% ts_full))

## step 4: create/inspect summary stats
df_stats <-
  df_trimmed_timeMatch |> 
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
# 2021
df_trimmed_timeMatch |> 
  subset(year(datetime) == 2021) |> 
  mutate(condUncal = round(condUncal, 1),
         tempC = round(tempC, 2),
         SpC = round(SpC, 2),
         QAQC = replace_na(QAQC, "")) |> 
  write_csv(file.path(path_data, "..", paste0("KNZ_AllSTICsCleaned_", str_replace_all(good_data_start, "-", ""), "-20211231.csv")))

# 2022
df_trimmed_timeMatch |> 
  subset(year(datetime) == 2022) |> 
  mutate(condUncal = round(condUncal, 1),
         tempC = round(tempC, 2),
         SpC = round(SpC, 2),
         QAQC = replace_na(QAQC, "")) |> 
  write_csv(file.path(path_data, "..", paste0("KNZ_AllSTICsCleaned_20220101-20221231.csv")))
