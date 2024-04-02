# Load required library
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(ggthemes)
library(gridExtra)
library(reshape2)
library(installr)
# Cleaning
# Clearing the environment of previous variables
rm(list=ls())
# Clearing the console of previous items
shell("cls")
# Setting the working directory to the location of the current script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Pulling in the function
# source("./ScatterplotFunc.R")


# Example datasets
set.seed(123)
data1 <- data.frame(
  x = rnorm(50),
  y = rnorm(50)
)

data2 <- data.frame(
  x = rnorm(50),
  y = rnorm(50)
)

# Enter in the exact path to the excel sheet you would like to use
excel_path <- "C:/Users/hughm/Dropbox/Cochlear_IIR_Project/Analysis/Scoring/Cochlear IIR scoring log.xlsx"

# You can search for the column name you want by starting to type in the name after the $ below
# df <- read_excel(excel_path)
# df$Talker_Discrim_Quiet_Score


# Write the names in a string for the column variables you want to use for the
# x axis and y axis. If you want to plot multiple for correlation, just type them 
# one after the other with commas in between 
variables_1 <- c("Word_Rec_Quiet_Reaction")
variables_2 <- c("Word_Rec_Quiet_Reaction")

# If you want to put two scatterplots on top of each other, change the "dual" variable to TRUE. 
variables_3 <- c("Accent_Discrimination_Score_Total_List_A")
variables_4 <- c("Accent_Discrimination_Score_Total_List_B")
# variables_3 <- c("Accent_Discrimination_Rating_Total_List_A")
# variables_4 <- c("Accent_Discrimination_Rating_Total_List_B")



# Facet_wrap, stacking the figures, ggarrange, ggpubr


# Running the function

# Reading the excel file
df <- read_excel(excel_path)

test_data <- data.frame(
  x <- df[,"Accent_Discrimination_Score_Total_List_A"],
  y <- df[,"Word_Rec_Quiet_Reaction"]
)

test_data <- test_data %>% 
  rename(x = Accent_Discrimination_Score_Total_List_A) %>%
  na.omit()

test_data2 <- data.frame(
  x <- df[,"Accent_Discrimination_Score_Total_List_B"],
  y <- df[,"Word_Rec_Quiet_Reaction"]
)

test_data2 <- test_data2 %>% 
  rename(x = Accent_Discrimination_Score_Total_List_B) %>%
  na.omit()

test_data$type <- "test_data"
test_data2$type <- "test_data2"

# Combine datasets
combined_data <- rbind(test_data, test_data2)


# Plot
ggplot(combined_data, aes(x = x, y = Word_Rec_Quiet_Reaction, color = type)) +
  geom_point() +
  scale_color_manual(values = c("red", "blue")) +
  theme_minimal()
