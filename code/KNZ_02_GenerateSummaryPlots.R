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
