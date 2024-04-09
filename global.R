library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(readr)
library(lubridate)
library(reshape2)
library(readxl)
library(xtable)
library(shinyWidgets)
library(colourpicker)
library(reshape2)
library(hrbrthemes)
library(viridis)
library(ggthemes)
# data prep

# source: https://www.kaggle.com/datasets/muhammadkhalid/most-popular-programming-languages-since-2004
# we want to add a new column that is usable numerically for the date. That is why we use lubridate
# df <- read_csv("./data/Most Popular Programming Languages from 2004 to 2022.csv") %>%
#   dplyr::mutate(timestamp = lubridate::my(Date)) %>%
#   dplyr::select(-Date)

# Enter in the exact path to the excel sheet you would like to use


# Doing the same thing
# df_long <- melt(df,id.vars = "timestamp",variable.name = "language",value.name = "popularity")
# df_long <- df %>%
#   pivot_longer(cols = !timestamp, names_to = "language", values_to = "popularity")
# 
# # Prepare the vecotr for all the different languages, the unique means that we only have the languages once
# language_choices <- df_long$language %>% unique()
# 
# # External limits of the dates
# date_start <- df_long$timestamp %>% min()
# date_end <- df_long$timestamp %>% max()
