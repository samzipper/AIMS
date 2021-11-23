## Konza_LowDischargeAnalysis.R

source(file.path("code", "paths+packages.R"))

library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(patchwork)

## download data
pCodes = c("00060") # discharge = 00060, stage = 00065
USGS_gage <- "06879650"  # USGS 06879650 KINGS C NR MANHATTAN, KS
daily_raw <- 
  dataRetrieval::readNWISdv(siteNumbers = USGS_gage, 
                            parameterCd = pCodes,
                            statCd = "00003") # daily mean

# subset to water years only
daily_raw$WaterYear <- year(daily_raw$Date + days(92))
daily_raw$WYDOY <- yday(daily_raw$Date + days(92))
colnames(daily_raw)[colnames(daily_raw)=="X_00060_00003"] <- "discharge_cfs"

daily <- 
  daily_raw %>% 
  subset(WaterYear >= 1980 & WaterYear <= 2021)

# set "low flow is impossible" threshold
q_thres <- 0.1 # cfs

daily$toolow <- daily$discharge_cfs < q_thres

# sum to annual
annual <-
  daily %>% 
  group_by(WaterYear) %>% 
  summarize(toolow_days = sum(toolow),
            total_days = sum(is.finite(discharge_cfs)),
            toolow_prc = toolow_days/total_days,
            discharge_cfs_total = sum(discharge_cfs)*86400,
            discharge_cfs_toolow = sum(discharge_cfs[toolow])*86400,
            discharge_toolow_prc = discharge_cfs_toolow/discharge_cfs_total)

# double-check math
sum(daily$discharge_cfs[daily$toolow & daily$WaterYear== 1980])*86400
sum(daily$discharge_cfs[daily$WaterYear== 1980])*86400

# plot of annual total discharge and too-low discharge
annual %>% 
  pivot_longer(-WaterYear) %>% 
  subset(name != "discharge_toolow_prc") %>% 
  ggplot(aes(x = WaterYear, y = value, color = name)) +
  geom_line()

# plot
p_ts_days <-
  ggplot(annual, aes(x = WaterYear, y = toolow_prc)) +
  geom_line(color = col.cat.blu) +
  geom_point(color = col.cat.blu) +
  scale_y_continuous(name = paste0("% of Days with Q < ", q_thres, " cfs"),
                     labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(name = "Water Year")
  
p_ts <- 
  ggplot(annual, aes(x = WaterYear, y = discharge_toolow_prc)) +
  geom_line(color = col.cat.blu) +
  geom_point(color = col.cat.blu) +
  scale_y_continuous(name = paste0("% of Annual Q occurring\nat Q < ", q_thres, " cfs"),
                     labels = scales::percent) +
  scale_x_continuous(name = "Water Year")

p_hist <- 
  ggplot(annual, aes(x = discharge_toolow_prc)) +
  geom_histogram(fill = col.cat.blu, breaks = seq(0, 0.35, 0.05)) +
  scale_x_continuous(name = paste0("% of Annual Q occurring at Q < ", q_thres, " cfs"),
                     labels = scales::percent) +
  scale_y_continuous(name = "# of Water Years", expand = expansion(c(0,0.02)))

p_combo <-
  (p_ts_days + p_ts + p_hist) +
  plot_layout(ncol = 1) +
  plot_annotation(title = "USGS 06879650, Kings Creek nr Manhattan")

ggsave(file.path("plots", "Konza_LowDischargeAnalysis.png"),
       width = 120, height = 180, units = "mm")  
