## SHN_StageDygraph.R
# Goal: Make a dygraph with the following things plotted:
#   - stage through time (SW and GW PT)
#   - manual stage measurements (from staff gage)
# Following example from Nate: https://gist.github.com/FloodHydrology/7eaa0bd9ddd420a95d5055a4f369a590
# Dygraph website: https://dygraphs.com/
# Dygraph for R: https://rstudio.github.io/dygraphs/

## load packages
library(tidyverse)
library(lubridate)
library(dygraphs)
library(xts)
library(dataRetrieval)
library(htmlwidgets)

## path to stage data - from repo AIMS-Project/EXO_QAQC
path_data <- file.path("..", "EXO_QAQC", "01_data", "04_postQaqc", "SHN")

## load and combine data
# get list of all CSV files
data_files <- list.files(path_data, pattern = "*.csv")

# load files, select columns, combine
for (d in data_files){
  df_d <- read_csv(file.path(path_data, d))
  
  # deal with inconsistent date string
  #if (d %in% c("KMZ_2023-05_QAQCed.csv", "KMZ_2023-06_QAQCed.csv", "KMZ_2023-07_QAQCed.csv", "KMZ_2023-08_QAQCed.csv")){
  #  # for 2023-05 and 2023-06, conductivity timestamp is in different format than rest of parametesr
  #  df_d$timestamp <- ymd_hms(df_d$timestamp)
  #}
  
  df_d_wide <- 
    df_d |> 
    subset(parameterName %in% c("SW_Pressure_psi", "SW_Temp_PT_C", "SW_Level_ft", 
                                "GW_Pressure_psi", "GW_Temp_PT_C", "GW_Level_ft")) |> 
    dplyr::select(timestamp, parameterName, value) |> 
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

# make dygraph function
dygraph_ts_fun<-function(df, title, y_label = "Stage [ft]"){
  
  #format data
  df_xts<-df #%>% na.omit() 
  df_xts<-xts(df_xts, order.by=df_xts$timestamp)
  df_xts<-df_xts[,-1]
  
  #Plot
  dygraph(df_xts,
          main = title) %>%
    dyRangeSelector() %>%
    dyLegend() %>%
    dyOptions(strokeWidth = 1.5) %>%
    dyOptions(labelsUTC = TRUE) %>%
    dyHighlight(highlightCircleSize = 5,
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = FALSE) %>%
    dyAxis("y", label = y_label)
}

# plot dygraph
df_all |> 
  dplyr::select(timestamp, SW_Level_ft) |>  # select timestamp + any variables you want to plot
  dygraph_ts_fun(title = "SHN", y_label = "Stage [ft]")

# push to web
m <- df_all |> 
  dplyr::select(timestamp, SW_Level_ft) |>  # select timestamp + any variables you want to plot
  dygraph_ts_fun(title = "SHN", y_label = "Stage [ft]")

htmlwidgets::saveWidget(m, 'docs//SHN_StageDygraph.html')
# see at: https://samzipper.github.io/AIMS/docs/SHN_StageDygraph.html