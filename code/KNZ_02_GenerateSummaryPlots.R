## KNZ_02_GenerateSummaryPlots.R

source(file.path("code", "paths+packages.R"))
library(patchwork)

# path to QAQCed data
path_data <- "G:/.shortcut-targets-by-id/1KSx3E1INg4hQNlHWYnygPi41k_ku66fz/Track 2 AIMS/QA QCed Data/GP_STIC_QAQC/"

# load 2021 and 2022 files and combine
df_2021 <- read_csv(file.path(path_data, "KNZ_AllSTICsCleaned_20210522-20211231.csv"))
df_2022 <- read_csv(file.path(path_data, "KNZ_AllSTICsCleaned_20220101-20221231.csv"))
df <- bind_rows(df_2021, df_2022)

# calculate % wet for following scenarios:
# (1) all stics; (2) HS only; (3) HS-excellent; (4) HS-good+excellent; (5) HS-fair+good+excellent
df_all <-
  df |> 
  group_by(datetime) |> 
  summarize(n_stic = sum(!is.na(wetdry)),
            prc_wet = sum(wetdry == "wet")/n_stic,
            STICs = "All (HS + LS)")

df_HS <-
  df |> 
  subset(sublocation == "HS") |> 
  group_by(datetime) |> 
  summarize(n_stic = sum(!is.na(wetdry)),
            prc_wet = sum(wetdry == "wet")/n_stic,
            STICs = "HS: All")

df_HS.e <-
  df |> 
  subset(sublocation == "HS" & qual_rating == "excellent") |> 
  group_by(datetime) |> 
  summarize(n_stic = sum(!is.na(wetdry)),
            prc_wet = sum(wetdry == "wet")/n_stic,
            STICs = "HS: Excellent")
df_HS.eg <-
  df |> 
  subset(sublocation == "HS" & qual_rating %in% c("excellent", "good")) |> 
  group_by(datetime) |> 
  summarize(n_stic = sum(!is.na(wetdry)),
            prc_wet = sum(wetdry == "wet")/n_stic,
            STICs = "HS: Excellent+Good")
df_HS.egf <-
  df |> 
  subset(sublocation == "HS" & qual_rating %in% c("excellent", "good", "fair")) |> 
  group_by(datetime) |> 
  summarize(n_stic = sum(!is.na(wetdry)),
            prc_wet = sum(wetdry == "wet")/n_stic,
            STICs = "HS: Excellent+Good+Fair")

# combine and plot
df_stats <-
  df_all |> 
  bind_rows(df_HS) |> 
  bind_rows(df_HS.e) |> 
  bind_rows(df_HS.eg) # |> 
  #bind_rows(df_HS.egf)

p_prcwet <- 
  ggplot(df_stats, aes(x = datetime, y = prc_wet, color = STICs)) +
  geom_line() +
  scale_y_continuous(name = "% Wet STICs", labels = scales::percent) +
  scale_x_datetime(name = "Datetime [15 min data]", expand = c(0,0)) +
  scale_color_viridis_d() +
  labs(title = "Konza SFKC")

p_nstic <- 
  ggplot(df_stats, aes(x = datetime, y = n_stic, color = STICs)) +
  geom_line() +
  scale_y_continuous(name = "# Active STICs") +
  scale_x_datetime(name = "Datetime [15 min data]", expand = c(0,0)) +
  scale_color_viridis_d() +
  labs(title = "Konza SFKC")

# combine and save
p_STICdata <- 
  (p_prcwet + p_nstic) +
  plot_layout(ncol = 1, guides = "collect") &
  theme(legend.position = "bottom")
ggsave(file.path(path_data, "KNZ_AllSTICsCleaned_SummaryPlots.png"),
       p_STICdata, width = 190, height = 190, units = "mm")  

## compare to field measurements
df_field <- read_csv("G:/.shortcut-targets-by-id/1KSx3E1INg4hQNlHWYnygPi41k_ku66fz/Track 2 AIMS/Data [working files]/Core datasets (as defined by implementation plan)/Approach 1_sensors and STICs/GP_STIC_metadata/KNZ_STIC_metadata/KNZ_STIC_QAQC_metadata/KNZ_STIC_QAQC_metadata.csv") |> 
  subset(wet_dry %in% c("wet", "dry")) # remove NAs and ice

# join based on nearest timestamp
df_field$STICreading <- NA
df_field$timediff_min <- NA
for (i in 1:length(df_field$wet_dry)){
  # grab site and timestamp
  s <- df_field$Location[i]
  t <- df_field$datetime[i] |> 
    mdy_hm(tz = "America/Chicago") |> 
    with_tz(tzone = "UTC")
  
  # get observation and time difference
  df_s <- subset(df, siteID == s)
  j_closest <- which.min(abs(df_s$datetime - t))
  t_diff <- as.numeric(difftime(t, df_s$datetime[j_closest], units = "mins"))
  
  # add observation
  if (length(t_diff) > 0){
    df_field$STICreading[i] <- df_s$wetdry[j_closest]
    df_field$timediff_min[i] <- t_diff
  }
  
}

# comparison - get rid of NAs and timediff > 30 min
df_compare <- 
  df_field |> 
  subset(is.finite(timediff_min) & abs(timediff_min) < 30)

# make confusion matrix
df_confusion <-
  df_compare |> 
  group_by(wet_dry, STICreading) |> 
  summarize(count = n())

accuracy <- 
  (df_confusion$count[df_confusion$wet_dry=="wet" & df_confusion$STICreading=="wet"] +
  df_confusion$count[df_confusion$wet_dry=="dry" & df_confusion$STICreading=="dry"])/sum(df_confusion$count)

p_accuracy <- 
  ggplot(df_confusion, aes(x = wet_dry, y = STICreading)) +
  geom_tile(aes(fill = count)) +
  geom_text(aes(label = count), color = "white") +
  scale_x_discrete(name = "Observed", expand = c(0,0)) +
  scale_y_discrete(name = "Classified", expand = c(0,0)) +
  scale_fill_viridis_c(name = "# of Observations") +
  labs(title = "KNZ STIC accuracy assessment", subtitle = paste0("Overall accuracy = ", round(100*accuracy, 1), "%"))
ggsave(file.path(path_data, "KNZ_AllSTICsCleaned_AccuracyPlot.png"),
       p_accuracy, width = 120, height = 95, units = "mm")  
