## OKA_02_GenerateSummaryPlots.R

source(file.path("code", "paths+packages.R"))
library(patchwork)

# path to QAQCed data
path_data <- "G:/.shortcut-targets-by-id/1KSx3E1INg4hQNlHWYnygPi41k_ku66fz/Track 2 AIMS/QA QCed Data/GP_STIC_QAQC/"

# load 2021 and 2022 files and combine
df <- read_csv(file.path(path_data, "OKA_AllSTICsCleaned_20220215-20221025.csv"))

# calculate % wet for following scenarios:
# (1) all stics; (2) excellent; (3) good+excellent; (4) HS-fair+good+excellent
df_all <-
  df |> 
  group_by(datetime) |> 
  summarize(n_stic = sum(!is.na(wetdry)),
            prc_wet = sum(wetdry == "wet")/n_stic,
            STICs = "All")

df_e <-
  df |> 
  subset(qual_rating == "excellent") |> 
  group_by(datetime) |> 
  summarize(n_stic = sum(!is.na(wetdry)),
            prc_wet = sum(wetdry == "wet")/n_stic,
            STICs = "Excellent")
df_eg <-
  df |> 
  subset(qual_rating %in% c("excellent", "good")) |> 
  group_by(datetime) |> 
  summarize(n_stic = sum(!is.na(wetdry)),
            prc_wet = sum(wetdry == "wet")/n_stic,
            STICs = "Excellent+Good")
df_egf <-
  df |> 
  subset(qual_rating %in% c("excellent", "good", "fair")) |> 
  group_by(datetime) |> 
  summarize(n_stic = sum(!is.na(wetdry)),
            prc_wet = sum(wetdry == "wet")/n_stic,
            STICs = "Excellent+Good+Fair")

# combine and plot
df_stats <-
  df_all |> 
  bind_rows(df_e) |> 
  bind_rows(df_eg) # |> 
#bind_rows(df_HS.egf)

p_prcwet <- 
  ggplot(df_stats, aes(x = datetime, y = prc_wet, color = STICs)) +
  geom_line() +
  scale_y_continuous(name = "% Wet STICs", labels = scales::percent) +
  scale_x_datetime(name = "Datetime [15 min data]", expand = c(0,0)) +
  scale_color_viridis_d() +
  labs(title = "YMR")

p_nstic <- 
  ggplot(df_stats, aes(x = datetime, y = n_stic, color = STICs)) +
  geom_line() +
  scale_y_continuous(name = "# Active STICs") +
  scale_x_datetime(name = "Datetime [15 min data]", expand = c(0,0)) +
  scale_color_viridis_d() +
  labs(title = "YMR")

# combine and save
p_STICdata <- 
  (p_prcwet + p_nstic) +
  plot_layout(ncol = 1, guides = "collect") &
  theme(legend.position = "bottom")
ggsave(file.path(path_data, "OKA_AllSTICsCleaned_SummaryPlots.png"),
       p_STICdata, width = 190, height = 190, units = "mm")  
