## YMR_ENM01_StageDygraph.R
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
path_data <- file.path("..", "EXO_QAQC", "01_data", "04_postQaqc", "YMR")
sitename <- "YMR_ENM01"

## load and combine data
# get list of all CSV files
data_files <- list.files(path_data, pattern = "*.csv")

# load files, select columns, combine
for (d in data_files){
  df_d <- read_csv(file.path(path_data, d)) |> 
    subset(parameterName %in% c("SW_Pressure_psi", "SW_Temp_PT_C", "SW_Level_ft", 
                                "GW_Pressure_psi", "GW_Temp_PT_C", "GW_Level_ft")) |> 
    dplyr::select(timestamp, parameterName, value)
  
  df_d$datetime <- parse_date_time(df_d$timestamp, orders = c("ymd_HMS", "mdy_HM"))

  # check if anything duplicated (in 2022-11, looks like DST hours are repeated)
  which_dups <- which(duplicated(df_d[,c("timestamp", "parameterName")]))
  if (length(which_dups) > 0){
    df_d <- df_d[-which_dups, ]
  }
  
  df_d_wide <- 
    df_d |> 
    pivot_wider(id_cols = "datetime", names_from = "parameterName", values_from = "value")
  
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
dygraph_ts_fun<-function(df, title = sitename, y_label = "Stage [ft]"){
  
  #format data
  df_xts<-df #%>% na.omit() 
  df_xts<-xts(df_xts, order.by=df_xts$timestamp)
  # not sure why, but for YMR must convert to numeric
  df_xts$
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

# for YMR, rename "datetime" to "timestamp"
names(df_all)[names(df_all) == "datetime"] <- "timestamp"

#format data
df_xts<-df_all |> 
  dplyr::select(timestamp, SW_Level_ft)
df_xts<-xts(df_xts, order.by=df_xts$timestamp)
df_xts$SW_Level_ft <- as.numeric(df_xts$SW_Level_ft)
df_xts<-df_xts[,-1]

#Plot
dygraph(df_xts,
        main = sitename) %>%
  dyRangeSelector() %>%
  dyLegend() %>%
  dyOptions(strokeWidth = 1.5) %>%
  dyOptions(labelsUTC = TRUE) %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE) %>%
  dyAxis("y", label = "Stage [ft]")

# push to web
m <- dygraph(df_xts,
             main = sitename) %>%
  dyRangeSelector() %>%
  dyLegend() %>%
  dyOptions(strokeWidth = 1.5) %>%
  dyOptions(labelsUTC = TRUE) %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE) %>%
  dyAxis("y", label = "Stage [ft]")

htmlwidgets::saveWidget(m, paste0('docs//', sitename, '_StageDygraph.html'))
# see at: https://samzipper.github.io/AIMS/docs/[SITENAME]_StageDygraph.html