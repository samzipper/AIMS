## Youngmeyer_NearestUSGSgage.R

library(tidyverse)
library(lubridate)
library(dataRetrieval)

## download data
pCodes = c("00060") # discharge = 00060, stage = 00065
USGS_gage <- "07167500"  # OTTER C AT CLIMAX, KS
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
  subset(WaterYear <= 2021 & WYDOY <= 365) %>% 
  mutate(discharge_cms = discharge_cfs*0.3048^3)

# calculate daily mean
daily_mean <- 
  daily %>% 
  group_by(WYDOY) %>% 
  summarize(discharge_cms_mean = mean(discharge_cms))

# plot
p_hydrograph <-
  ggplot() +
  geom_line(data = daily, aes(x = WYDOY, y = discharge_cms, group = WaterYear), alpha = 0.1) +
  geom_line(data = daily_mean, aes(x = WYDOY, y = discharge_cms_mean), color = "blue", size = 2) +
  scale_x_continuous(name = "Day of Water Year [starting Oct 1]",
                     limits = c(0, 365), expand = c(0,0)) +
  scale_y_log10(name = "Discharge [m^3/s]") +
  theme_bw() +
  labs(title = "USGS 07167500: Otter Creek at Climax KS",
       subtitle = "gray = individual years, blue = mean of all years",
       caption = "129 square miles, ~25 miles NE of Youngmeyer")
ggsave(file.path("results", "Youngmeyer_NearestUSGSgage.png"),
       width = 190, height = 120, units = "mm")
