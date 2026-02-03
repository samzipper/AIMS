## STICsynthesis_PrepQAQC.R

library(tidyverse)

# create QAQC CSV per instructions
df_qaqc <- tribble(
  ~qaqc_code, ~description, 
  "Excellent", "STIC was (1) calibrated prior to deployment, and (2) stayed operational throughout 95% of the download period, and (3) was not displaced from streambed (i.e., the external electrodes were within 1 cm from stream bed at the time of download indicating minimal erosion/deposition), and (4) data from sensor roughly agree with field observations of wet/dry (i.e., >1000 Lux sensor reading on day of removal corresponds to field observations of water at STIC).",
  "Good", "(1) STIC stayed operational throughout the entire download period, and (2) the external electrodes were within 1 cm from stream bed at the time of download, and (3) data from sensor roughly agree with field observations of wet/dry, but (4) the STIC was not calibrated prior to deployment.",
  "Fair", "(1) STIC stayed operational throughout 75% or more of the download period, and (2) data roughly agree with field observations, and/or (3) the external electrodes were between 1-3 cm from streambed at the time of download.",
  "Poor", "(1) STIC stayed operational throughout less than 75% of the download period, and/or (2) the external electrodes were >3 cm from streambed at the time of download, and/or (3) data does NOT agree with field observations",
  "[blank]", "No automated flags, STIC passes automated checks",
  "C", "estimated calibrated SpC was negative and was corrected to 0",
  "O", "calibrated SpC was outside the range of cailbration standards used",
  "D", "classified data point is a short-term deviation (i.e., a wet reading surrounded on both sides by dry readings). Deviations were flagged if there were a maximum of 4 consecutive anomalous readings (1 hour) within a window of 96 readings (1 day) of consistent readings.",
  "M", "wetdry reading was manually edited based on other data available from site"
) |> 
  mutate(data_type = "STIC")

# save one per watershed
path_out <- file.path("C:/Users", "s947z036", "OneDrive - University of Kansas", "Research", "AIMS", "Warix_STICsynthesis")

site_names <- c("SFKC", "YMR", "SHN", "N04D", "N01B", "N02B")
for (s in site_names){
  write_csv(df_qaqc, file.path(path_out, paste0("QAQC_", s, ".csv")))
}
