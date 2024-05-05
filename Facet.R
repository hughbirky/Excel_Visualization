# View()

# Gapminder
library(ggplot2); library(tidyverse); library(readxl)

# Clearing the console of previous junk
shell("cls")

# Clearing the environment of previous variables
rm(list=ls()) 


# Reading in the scoring log
Data1 <- read_excel("C:/Users/hughm/Dropbox/Cochlear_IIR_Project/Analysis/Figures/Shiny Plots Accent Discrimination/FA Summary.xlsx")

condition1 = "AO"
condition2 = "AT"

y_data1 = "Z_withinpart_AO"
y1name = "RTs (z-scores, across participants)"
y_data2 = "Z_withinpart_AT"
y2name = "RTs (z-scores, across participants)"
x_data1 ="Sentence_Rec_Quiet_Score"
x1name = "Sentence Recognition Accuracy (%)"
x_data2 = "WordRec_ACC"
x2name = "Word Recognition Accuracy (%)"
x_data3 = "WordRec_RT_Zpart"
x3name = "Word Recognition RTs (z-scores)"










# Creating the first data frame with all necessary information
data1_filtered <- data.frame(
  # Data for the y axis (I Know it's mislabeled, I suffer)
  x_data = Data1[,y_data1],
  # Data fro the x axis
  y_data = Data1[,x_data1],
  type = condition1,
  metric = x1name
  
) %>% 
  # Getting rid of the NAs
  na.omit() %>%
  rename(x_data = y_data1) %>%
  rename(y_data = x_data1)





# See above
data2_filtered <- data.frame(
  x_data = Data1[,y_data1],
  # Data fro the x axis
  y_data = Data1[,x_data2],
  type = condition1,
  metric = x2name
) %>% 
  na.omit() %>%
  rename(x_data = y_data1) %>%
  rename(y_data = x_data2)







# See above
data3_filtered <- data.frame(
  x_data = Data1[,y_data1],
  # Data fro the x axis
  y_data = Data1[,x_data3],
  type = condition1,
  metric = x3name
) %>% 
  na.omit() %>%
  rename(x_data = y_data1) %>%
  rename(y_data = x_data3)







# See above
data4_filtered <- data.frame(
  x_data = Data1[,y_data2],
  # Data fro the x axis
  y_data = Data1[,x_data1],
  type = condition2,
  metric = x1name
  
) %>% 
  na.omit() %>%
  rename(x_data = y_data2) %>%
  rename(y_data = x_data1)






# See above
data5_filtered <- data.frame(
  x_data = Data1[,y_data2],
  # Data fro the x axis
  y_data = Data1[,x_data2],
  type = condition2,
  metric = x2name
) %>% 
  na.omit() %>%
  rename(x_data = y_data2) %>%
  rename(y_data = x_data2)







# See above
data6_filtered <- data.frame(
  x_data = Data1[,y_data2],
  # Data fro the x axis
  y_data = Data1[,x_data3],
  type = condition2,
  metric = x3name
) %>% 
  na.omit() %>%
  rename(x_data = y_data2) %>%
  rename(y_data = x_data3)







# Combining the data frames
combined_data <- rbind(data1_filtered,data2_filtered,data3_filtered,data4_filtered,data5_filtered,data6_filtered)






# Creating fake data in order to have the proper limits
fake_data1 <- data.frame(
  y_data = c(0,100),
  x_data = c(-1,1),
  type = condition1,
  metric = x1name
  
) 
fake_data2 <- data.frame(
  # Adding in LOTS of data to see if it changes the regression
  # y_data = c(0,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,100),
  # x_data = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),
  y_data = c(0,100),
  x_data = c(-1,1),
  type = condition1,
  metric = x2name
  
) 
fake_data3 <- data.frame(
  y_data = c(-3,3),
  x_data = c(-1,1),
  type = condition1,
  metric = x3name
) 
fake_data4 <- data.frame(
  y_data = c(0,100),
  x_data = c(-1,1),
  type = condition2,
  metric = x1name
  
) 
fake_data5 <- data.frame(
  y_data = c(0,100),
  x_data = c(-1,1),
  type = condition2,
  metric = x2name
  
) 
fake_data6 <- data.frame(
  y_data = c(-3,3),
  x_data = c(-1,1),
  type = condition2,
  metric = x3name
  
) 

# Combining the fake data
combined_fake_data <- rbind(fake_data1,fake_data2,fake_data3,fake_data4,fake_data5,fake_data6)


# Plotting data
ggplot(data = combined_data,aes(x = y_data, y = x_data, lty = type)) +
  # Plotting actual wanted data as shapes
  geom_point(size = 2,aes(shape = type)) +
  # Adding a regression line to this first data
  geom_smooth(method = lm, se = F) +
  # Adding in invisible points after the regression
  geom_point(data = combined_fake_data, alpha = 0) +
  # Making them all into a facet grid
  facet_grid(~ metric, scales = "free_x") +
  # Changing the labels
  labs(y = y1name, x = "", shape = "Orthographic Condition", lty = "Orthographic Condition") +
  theme(legend.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        text = element_text(family = "Arial"),
        # panel.border = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill  = NA),
        plot.title = element_text(size=20),
        axis.title.x = element_text(angle = 0, hjust = 0.5,size = 10),
        axis.title.y = element_text(angle = 90, vjust = 0.5,size = 13),
        axis.text.x = element_text(size = 10),  # Increase size of x-axis numbers
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


  
