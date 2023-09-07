## KNZ_SFM01_StagePlotly.R
# Goal: Make an interactive graph with the following things plotted:
#   - stage through time (SW PT)
#   - manual stage measurements (from staff gage)
# https://plotly.com/r/line-and-scatter/ 

## load packages
library(tidyverse)
library(lubridate)
library(plotly)
library(htmlwidgets)

## path to stage data - from repo AIMS-Project/EXO_QAQC
path_data <- file.path("..", "EXO_QAQC", "01_data", "04_postQaqc", "KMZ")

## load and combine data
# get list of all CSV files
data_files <- list.files(path_data, pattern = "*.csv")

# load files, select columns, combine
for (d in data_files){
  df_d <- read_csv(file.path(path_data, d)) |> 
    subset(parameterName %in% c("SW_Pressure_psi", "SW_Temp_PT_C", "SW_Level_ft", 
                                "GW_Pressure_psi", "GW_Temp_PT_C", "GW_Level_ft")) |> 
    dplyr::select(timestamp, parameterName, value)
  
  # deal with inconsistent date string
  if (d %in% c("KMZ_2023-05_QAQCed.csv", "KMZ_2023-06_QAQCed.csv", "KMZ_2023-07_QAQCed.csv", "KMZ_2023-08_QAQCed.csv")){
    # for 2023-05 and 2023-06, conductivity timestamp is in different format than rest of parametesr
    df_d$timestamp <- ymd_hms(df_d$timestamp)
  }
  
  df_d_wide <- 
    df_d |> 
    pivot_wider(id_cols = "timestamp", names_from = "parameterName", values_from = "value")
  
  # combine output
  if (d == data_files[1]){
    df_all <- df_d_wide
  } else {
    df_all <- bind_rows(df_all, df_d_wide)
  }
}

#ggplot(df_all, aes(x = timestamp)) +
#  geom_line(aes(y = SW_Level_ft), color = "blue") +
#  geom_line(aes(y = GW_Level_ft), color = "red")

# load manual stage measurements - exported from 
# https://docs.google.com/spreadsheets/d/1DkF2Ugr6tEgnZHOGJ_UY2kW9JWH4ce8xbyzSyyA6-CI/edit#gid=677133672 
df_manual <- read_csv(file.path("data", "SFM01_ManualStage.csv")) |> 
  mutate(timestamp = mdy_hms(Datetime))

# make plotly
fig <- 
  plot_ly(data = df_all, 
          x = ~timestamp, 
          y = ~SW_Level_ft*0.3048, 
          name = 'Stream Stage [m]', 
          type = 'scatter', 
          mode = 'lines') |> 
  add_markers(data = df_manual, x = ~timestamp, y = ~StageManual_m, 
              name = 'Manual Stage', mode = 'markers') |> 
  layout(
    title = "Surface water level",
    xaxis = list(rangeslider = list(type = "date")),
    yaxis = list(title = "Stage [m]")
    )
fig

# push to web
htmlwidgets::saveWidget(fig, 'docs//KNZ_SFM01_StagePlotly.html')
# see at: https://samzipper.github.io/AIMS/docs/KNZ_SFM01_StagePlotly.html