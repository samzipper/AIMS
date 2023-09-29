## STIC_CalibrationCheck.R
# This script is intended to check the calibration for a batch of STICs.
# The required inputs are a CSV file with three columns:
#  - sn = serial number of STIC
#  - standard = value of standard in us/cm
#  - condUncal = raw data reported by STIC
# The calibration SOP with details is here: https://docs.google.com/document/d/1gTZ5MecE8Xjp6ymhH4rB92V_i1lH93Jv/edit
# An example input dataset is here: https://drive.google.com/drive/folders/1EST3UxKYHGUHdml31SW8nRNXY9cEhC_2

library(tidyverse)

## INPUTS
# path to CSV file with calibration data
path_cal_inputs <- "G:/.shortcut-targets-by-id/1KSx3E1INg4hQNlHWYnygPi41k_ku66fz/Track 2 AIMS/Data [working files]/Core datasets (as defined by implementation plan)/Approach 1_sensors and STICs/GP_STIC_metadata/Calibrations/KNZ_STIC_calibrations_202105_202301.csv"

# path to save calibration plots and results
path_cal_results <- "G:/.shortcut-targets-by-id/1KSx3E1INg4hQNlHWYnygPi41k_ku66fz/Track 2 AIMS/Data [working files]/Core datasets (as defined by implementation plan)/Approach 1_sensors and STICs/GP_STIC_metadata/Calibrations/2023-Oct Calibrations/"

## Load inputs
df_in <- 
  read_csv(path_cal_inputs) |> 
  # if there is an "air" calibration point, remove
  mutate(sn = as.numeric(sn)) |> 
  subset(is.finite(sn))


## get list of SNs
sn_all <- unique(df_in$sn)

## loop - calculate fit table and plots for each
for (s in sn_all){
  # subset to sn
  df_sn <- subset(df_in, sn == s)
  
  # remove "air" point
  
  # calculate fit
  lm_sn <- lm(condUncal ~ standard, data = df_sn)
  
  # extract output
  df_sn_out <- tibble(sn = s,
                      yint = coef(lm_sn)[1],
                      slope = coef(lm_sn)[2],
                      r2 = summary(lm_sn)$r.squared)
  
  # make plot
  p_sn <- 
    ggplot(df_sn, aes(x = standard, y = condUncal)) +
    geom_point() +
    stat_smooth(method = "lm") +
    labs(title = s,
         subtitle = paste0("R2 = ", round(df_sn_out$r2[1], 5))) +
    theme_bw()
  ggsave(file.path(path_cal_results, paste0(s, ".png")), p_sn)
  
  if (s == sn_all[1]){
    df_fit_out <- df_sn_out
  } else {
    df_fit_out <- bind_rows(df_fit_out, df_sn_out)
  }
}

# write fit stats
write_csv(df_fit_out, file.path(path_cal_results, "CalibrationFitStats.csv"))
