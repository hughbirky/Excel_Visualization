# install.packages("RInno")
# install.packages("Rcpp")

library(RInno)

# Replace these with your details
app_name <- "Data Visualization"  # Name of your app
app_dir <- "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Analysis/Code/Data Visualization App/Shiny_Gui"  # Full path to your Shiny app folder

# Create the RInno app
create_app(app_name = app_name, app_dir = app_dir, R_version = "4.4.2",R_path = "https://cran.r-project.org/bin/windows/base/R-4.4.2-win.exe")
# compile_iss()
