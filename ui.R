shinyUI(fluidPage(
  
  # Application title
  titlePanel("Interactive Graphs"),
  
  # Sidebar with a file input and select input
  sidebarLayout(
    sidebarPanel(
      
      # Tabset panel for distinguishing between different sections
      tabsetPanel(
        
        # Tab1 : Plot Settings
        tabPanel("Data Import",
                 # Input: Select a file
                 fileInput("file1", "Choose xlsx File", accept = c(".xlsx")),
                  
                 # Selecting which graph type you want
                 selectInput("plotType", label = "Graph Type",
                              choices = c("Scatterplot","Boxplot")),
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 ###############################################################
                 # Normal Scatterplot
                 # Checkbox for color addition or not
                 checkboxInput("color_boolean", "Color Data", value = FALSE),
                 
                 # Checkbox for regression or not
                 checkboxInput("regression_boolean", "Regression", value = FALSE),
                 
                 # Panel for Scatterplot Settings
                 conditionalPanel(
                   condition = "input.plotType == 'Scatterplot'",
                   # Selecting which item you want for each column
                   selectInput("x_column", label = "X Data",
                               choices = list("")),
                   selectInput("y_column", label = "Y Data",
                               choices = list("")),
                   # Conditional panel for using color
                   conditionalPanel(
                     condition = "input.color_boolean",
                     selectInput("color_data", label = "Color Data",
                                 choices = list("")),
                   )
                 ), 
                # Panel for Regression Settings
                conditionalPanel(
                  condition = "input.regression_boolean",
                  # Selecting which item you want for each column
                  selectInput("regression_method", label = "Regression Type",
                              choices = list("lm","glm","gam","loess","rlm")),
                  # Deciding whether or not to choose the confidence interval
                  checkboxInput("regression_se", "Confidence Interval", value = FALSE),
                ),
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                ################################################################
                # Dual Scatterplot
                 
                  
                  
        ),
        
        # Tab 2: Exporting Data
        tabPanel("Plot Settings",
                 # Select inputs for the points on the plot
                 # Select point size
                 sliderInput("point_size", "Point Size", value = 1, min = 0.01, max = 5),
                 # Select point color
                 conditionalPanel(
                   condition = "!input.color_boolean",
                   colourInput("point_color", "Point Color", "black")
                 ),
                 
                 colourInput("background_color", "Background Color", "lightgrey"),
                 colourInput("panel_color", "Panel Color", "grey"),
                 # colourInput("border_color", "Border Color", "white"),
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 ###############################################################
                 # Normal Scatterplot
                 # Panel for Scatterplot Settings
                 conditionalPanel(
                   condition = "input.plotType == 'Scatterplot'",
                   
                   # Checkbox for gridlines
                   checkboxInput("gridlines", "Show Major Gridlines", value = TRUE),
                   checkboxInput("minor_gridlines", "Show Minor Gridlines", value = TRUE),
                   
                   # Color picker for gridline color
                   # Conditional panel for showing the color option for the gridlines
                   conditionalPanel(
                      condition = "input.gridlines",
                      colourInput("major_gridline_color", "Major Gridline Color", value = "white"),
                   ),
                   conditionalPanel(
                     condition = "input.minor_gridlines",
                     colourInput("minor_gridline_color", "Minor Gridline Color", value = "white"))
                  ),
                   # Conditional panel for showing the color option for the gridlines
                   conditionalPanel(
                     condition = "input.color_boolean",
                     colourInput("data_color1", "Color Gradient 1", value = "green"),
                     colourInput("data_color2", "Color Gradient 2", value = "red"),
                   ),
                   # Conditional panel for showing the color option for the regression line
                   conditionalPanel(
                     condition = "input.regression_boolean",
                     colourInput("regression_color", "Regression Color", value = "blue"),
                   ),
                 # Panel for the axes hiding
                 conditionalPanel(
                   condition = "input.color_boolean",
                   colourInput("legend_background", "Legend Background Color", value = "white")
                 ),
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                  
          
        ),
        
        tabPanel("Axes/Labels",
                 
                 # Allowing them to override the plotting
                 checkboxInput("override_axes", label = "Override Axes", value = FALSE),
                 
                 
                 
                 
                 # Only having these pop up if the box is checked
                 conditionalPanel(
                   condition = "input.override_axes",
                   # Input for x_axis
                   sliderInput("override_x", "X Axis Range", min = 0, max = 7000, value = c(0,6000)),
                   # Input for y_axis
                   sliderInput("override_y", "Y Axis Range", min = 0, max = 7000, value = c(0,6000)),
                 ),
                 
                 
                 # Input for axes and title labels
                 textInput("plot_title", label = "Plot Title", value = "Plot"),
                 textInput("x_title", label = "X Axis Title", value = ""),
                 textInput("y_title", label = "Y Axis Title", value = ""),
                 
                 # Panel for the axes hiding
                 conditionalPanel(
                   condition = "input.color_boolean",
                   textInput("legend_title", label = "Legend Title", value = ""),
                 ),
                 
                 
                 # Select axes size
                 sliderInput("axes_size", "Axes Size", value = 15, min = 1, max = 30),
                 # Select numbers size
                 sliderInput("num_size", "Number Size", value = 10, min = 1, max = 30),
                 
          
        ),
        tabPanel("Export",
                 # Input for plot width
                 numericInput("plot_width", "Plot Export Width", value = 8),
                 
                 # Input for plot height
                 numericInput("plot_height", "Plot Export Height", value = 6),
                 
                 downloadButton("save_graph","Save Graph")
          
        )
        
      )
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      plotOutput("plot")
    )
    )
  )
)

