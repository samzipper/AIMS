## KNZ_03_CompileSynopticMetrics.R

source(file.path("code", "paths+packages.R"))
library(patchwork)

# path to QAQCed data
path_data <- "G:/.shortcut-targets-by-id/1KSx3E1INg4hQNlHWYnygPi41k_ku66fz/Track 2 AIMS/QA QCed Data/GP_STIC_QAQC/"

# load 2021 data and combine
df <- read_csv(file.path(path_data, "KNZ_AllSTICsCleaned_20210522-20211231.csv")) |> 
  mutate(site = str_sub(siteID, 1, 5)) |> 
  mutate(datetime_central = with_tz(datetime, tzone = "America/Chicago"))

# load sampling dates
df_dates <- 
  read_csv(file.path("data", "Konza_Synoptic_Dates.csv")) |> 
  unique() # some dates duplicated - not sure why

# loop through months
months_all <- c("June", "July", "August")
window_lengths <- c(7, 14, 21)

start <- T
for (m in months_all){
  # join for that month
  df_m <- left_join(df, subset(df_dates, month == m), by = "site")
  
  # loop through sites
  for (s in unique(df_dates$site)){
    df_ms <- subset(df_m, site == s)
    sdate <- ymd(unique(df_ms$date))
    
    for (t in window_lengths){
      
      # pull data for just that length
      df_mst <- subset(df_ms, 
                       (datetime >= sdate - days(t)) & 
                         (datetime <= sdate))
      
      # summary states 
      df_stats <- 
        df_mst |> 
        group_by(site, siteID, sublocation) |> 
        summarize(nWet = sum(wetdry == "wet"),
                  nTotal = sum(!is.na(wetdry)),
                  prcWet = nWet/nTotal,
                  prcData = nTotal/(t*24*4+1),
                  meanTemp_C = mean(tempC)) |> 
        rename(STIC = siteID) |> 
        mutate(windowLength_days = t,
               month = m) |> 
        # put in logical order
        dplyr::select(site, STIC, sublocation, month, windowLength_days, prcData, prcWet, meanTemp_C)
      
      # combine
      if (start){
        df_all <- df_stats
        start <- F
      } else {
        df_all <- bind_rows(df_all, df_stats)
      }
      
    }
  }
}

# save output
write_csv(df_all, file.path("results", "KNZ_SynopticSTICstats.csv"))

## for Lydia- compare high stic and low stic for 3 weeks before June synoptics
df_hsls <- subset(df_all, month == "June" & windowLength_days == 21) |> 
  ungroup()

# sites with low STIC
sites_ls <- subset(df_hsls, sublocation == "LS")$site

df_pivot <-
  df_hsls |> 
  subset(site %in% sites_ls) |> 
  dplyr::select(site, sublocation, prcWet, meanTemp_C)

df_pivot |> 
  dplyr::select(-meanTemp_C) |> 
  pivot_wider(id_cols = "site", names_from = "sublocation", values_from = "prcWet") |> 
  ggplot(aes(x = HS, y = LS)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1))
