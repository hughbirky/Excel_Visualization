# View()

# Gapminder
library(ggplot2); library(tidyverse); library(readxl)



# Reading in the scoring log
Data1 <- read_excel("C:/Users/hughm/Dropbox/Cochlear_IIR_Project/Analysis/Figures/Shiny Plots Accent Discrimination/1ID analyses.xlsx")



# Creating the first data frame with all necessary information
data1_filtered <- data.frame(
  # Data for the y axis (I Know it's mislabeled, I suffer)
  x_data = Data1$aprimeAT,
  # Data fro the x axis
  y_data = Data1$WordrecQuietScore,
  type = "AT",
  metric = "Word_Rec_Quiet_Score"
  
) %>% 
  # Getting rid of the NAs
  na.omit()


# See above
data2_filtered <- data.frame(
  x_data = Data1$aprimeAT,
  y_data = Data1$WordrecQuietZScore,
  type = "AT",
  metric = "Word_Rec_Quiet_Reaction_Time"
) %>% 
  na.omit()



# See above
data3_filtered <- data.frame(
  x_data = Data1$aprimeAT,
  y_data = Data1$Sentence_Rec_Quiet_Score,
  type = "AT",
  metric = "Sentence_Rec_Quiet_Score"
) %>% 
  na.omit()



# See above
data4_filtered <- data.frame(
  x_data = Data1$aprimeAO,
  y_data = Data1$WordrecQuietScore,
  type = "AO",
  metric = "Word_Rec_Quiet_Score"
  
) %>% 
  na.omit()


# See above
data5_filtered <- data.frame(
  x_data = Data1$aprimeAO,
  y_data = Data1$WordrecQuietZScore,
  type = "AO",
  metric = "Word_Rec_Quiet_Reaction_Time"
) %>% 
  na.omit()



# See above
data6_filtered <- data.frame(
  x_data = Data1$aprimeAO,
  y_data = Data1$Sentence_Rec_Quiet_Score,
  type = "AO",
  metric = "Sentence_Rec_Quiet_Score"
) %>% 
  na.omit()



# Combining the data frames
combined_data <- rbind(data1_filtered,data2_filtered,data3_filtered,data4_filtered,data5_filtered,data6_filtered)






# Creating fake data in order to have the proper limits
fake_data1 <- data.frame(
  y_data = c(0,100),
  x_data = c(0,1),
  type = "AT",
  metric = "Word_Rec_Quiet_Score"
  
) 
fake_data2 <- data.frame(
  # Adding in LOTS of data to see if it changes the regression
  y_data = c(0,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,100),
  x_data = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),
  # y_data = c(0,100),
  # x_data = c(0,1),
  type = "AT",
  metric = "Sentence_Rec_Quiet_Score"
  
) 
fake_data3 <- data.frame(
  y_data = c(-5,5),
  x_data = c(0,1),
  type = "AO",
  metric = "Word_Rec_Quiet_Reaction_Time"
) 
fake_data4 <- data.frame(
  y_data = c(0,100),
  x_data = c(0,1),
  type = "AO",
  metric = "Word_Rec_Quiet_Score"
  
) 
fake_data5 <- data.frame(
  y_data = c(0,100),
  x_data = c(0,1),
  type = "AO",
  metric = "Sentence_Rec_Quiet_Score"
  
) 
fake_data6 <- data.frame(
  y_data = c(-5,5),
  x_data = c(0,1),
  type = "AO",
  metric = "Word_Rec_Quiet_Reaction_Time"
  
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
  labs(y = "Sensitivity (A')", x = "", shape = "Orthographic Condition", lty = "Orthographic Condition") 


  
