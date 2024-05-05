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
                              choices = c("Scatterplot","Multiple Scatterplot","Boxplot", "Facet Grid"),
                             selected = "Multiple Scatterplot"),
                 
                 conditionalPanel(
                   condition = "input.plotType == 'Scatterplot' || input.plotType == 'Multiple Scatterplot' || ipnut.plotType == 'Facet Grid'",
                   # Selecting which item you want for each column
                   selectInput("y_column", label = "Y Data",
                               choices = list("")),
                 ),
                 
                 selectInput("x_column", label = "X Data",
                             choices = list("")),
                
                 conditionalPanel(
                   condition = "input.plotType == 'Multiple Scatterplot' || input.plotType == 'Boxplot' || ipnut.plotType == 'Facet Grid'",
                   # Selecting which item you want for each column of the second set
                   selectInput("x_column2", label = "X Data (Second Set)",
                               choices = list("")),
                   
                   selectInput("multiple_color", label = "Color or Shapes",
                               choices = list("Color","Shapes"),
                               selected = "Shapes"),
                 ),
                 
                 
                 

                 ###############################################################
                 # Scatterplots
                 # Checkbox for whether or not it is a scatterplot
                 conditionalPanel(
                   condition = "input.plotType == 'Scatterplot' || input.plotType == 'Multiple Scatterplot' || ipnut.plotType == 'Facet Grid'",
                   
                   # Checkbox for regression or not
                   checkboxInput("regression_boolean", "Regression", value = FALSE),
        
                   
                   ###############################################################
                   # Scatterplot specific
                   conditionalPanel(
                     # Doing color options if they are in the scatterplot condition
                     condition = "input.plotType == 'Scatterplot'",
                     checkboxInput("color_boolean", "Color Data", value = FALSE),
                     # Conditional panel for using color
                     conditionalPanel(
                       condition = "input.color_boolean",
                       selectInput("color_data", label = "Color Data",
                                   choices = list("")),
                     ),
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
                 ),
                 
                 

                 
                 #################################################################
                 # Boxplot
                 conditionalPanel(
                   condition = "input.plotType == 'Boxplot'",
                 
                   checkboxInput("boxplot_individual_points_bool", "Individual Points", value = FALSE),
                   checkboxInput("boxplot_mean_bool", "Mean", value = FALSE),
                   
                  
                 )
        ),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        # Tab 2: Plot Settings
        tabPanel("Plot Settings",
                 # Select inputs for the points on the plot
                 # Select point size
                 sliderInput("point_size", "Point Size", value = 1, min = 0.01, max = 5),
                 
                 # Checkbox for gridlines
                 checkboxInput("gridlines", "Show Major Gridlines", value = TRUE),
                 checkboxInput("minor_gridlines", "Show Minor Gridlines", value = TRUE),
                 checkboxInput("outline_boolean", "Show Plot Outline", value = TRUE),
                 
                 
                 # Overriding Axes
                 checkboxInput("override_axes", label = "Axes Slider Adjust", value = FALSE),
                 # Allowing the participant to crop the graph if they want to
                 # conditionalPanel(
                 #   condition = "input.override_axes",
                 #   # Input for x_axis
                 #   sliderInput("override_x", "X Axis Range", min = 0, max = 7000, value = c(0,100),step = 0.01),
                 #   # Input for y_axis
                 #   sliderInput("override_y", "Y Axis Range", min = 0, max = 7000, value = c(0,100), step = 0.01),
                 # ),
                 

                 # Allowing for integer inputs for the range of the inputs
                 numericInput("y_axis_min", "Y Min",value = 0),
                 numericInput("y_axis_max", "Y Max",value = 0),
                 numericInput("x_axis_min", "X Min",value = 0),
                 numericInput("x_axis_max", "X Max",value = 0),
                 

                 # Selecting which item you want for each column of the second set
                 numericInput("x1_axis_min", "X1 Min",value = 0),
                 numericInput("x1_axis_max", "X1 Max",value = 0),
            
                 
                 
                 

                 
                 # Select axes size
                 sliderInput("axes_size", "Axes Size", value = 15, min = 1, max = 30),
                 # Select numbers size
                 sliderInput("num_size", "Number Size", value = 10, min = 1, max = 30),
                 
                 
        ),
        
        
        
        
        
        
        
        
        tabPanel("Color",
                 
                 
                 # Select point color
                 conditionalPanel(
                   condition = "!input.color_boolean && input.plotType == 'Scatterplot'",
                   colourInput("point_color", "Point Color", "black")
                 ),
                 
                 colourInput("background_color", "Background Color", "gray"),
                 colourInput("panel_color", "Panel Color", "lightgray"),
                 # colourInput("border_color", "Border Color", "white"),
                 
                 
                 
                 
                 
                 ###############################################################
                 # Normal Scatterplot
                 # Panel for Scatterplot Settings
                 conditionalPanel(
                   condition = "input.plotType == 'Scatterplot' || input.plotType == 'Multiple Scatterplot' || input.plotType == 'Boxplot' " ,
                   
                   
                   
                   # Color picker for gridline color
                   # Conditional panel for showing the color option for the gridlines
                   conditionalPanel(
                     condition = "input.gridlines",
                     colourInput("major_gridline_color", "Major Gridline Color", value = "black"),
                   ),
                   conditionalPanel(
                     condition = "input.minor_gridlines",
                     colourInput("minor_gridline_color", "Minor Gridline Color", value = "black"),
                   ),
                   
                   
                   # Select inputs for outline color and width
                   colourInput("outline_color", "Outline Color", "black"),
                   
                   # Conditional panel for showing the color option for the gridlines
                   conditionalPanel(
                     condition = "input.color_boolean && input.plotType == 'Scatterplot'",
                     colourInput("data_color1", "Color Gradient 1", value = "green"),
                     colourInput("data_color2", "Color Gradient 2", value = "red"),
                   ),
                   
                   # Conditional panel for showing the color option for the gridlines
                   conditionalPanel(
                     condition = "input.plotType == 'Multiple Scatterplot' || input.plotType == 'Boxplot'",
                     colourInput("point_color1", "Set Color 1", value = "blue"),
                     colourInput("point_color2", "Set Color 2", value = "red"),
                   ),
                   
                   # Conditional panel for showing the color option for the regression line
                   conditionalPanel(
                     condition = "input.regression_boolean && input.plotType == 'Scatterplot'",
                     colourInput("regression_color", "Regression Color", value = "blue"),
                     
                     conditionalPanel(
                       condition = "input.plotType = 'Multiple Scatterplot'",
                       colourInput("regression_color_multiple", "Regression Color (Set 2)", value = "red"),
                     )
                   ),
                   # Panel for the axes hiding
                   conditionalPanel(
                     condition = "input.color_boolean || input.plotType = 'Multiple Scatterplot' || ipnut.plotType = 'Boxplot'",
                     colourInput("legend_background", "Legend Background Color", value = "white")
                   ),
                 )
        ),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        tabPanel("Labels",

                 ##############################################################
                 # Normal Scatterplot
                 # Allowing them to override the plotting
                 conditionalPanel(
                   condition = "input.plotType == 'Scatterplot' || input.plotType == 'Multiple Scatterplot' || input.plotType == 'Boxplot'",
                   
                   selectInput("Font", label = "Font",
                               choices = list("Arial", "Times New Roman"), selected = "Arial"),
                   
                   # Input for axes and title labels
                   textInput("plot_title", label = "Plot Title", value = "Plot"),
                   textInput("x_title", label = "X Axis Title", value = ""),
                   textInput("y_title", label = "Y Axis Title", value = ""),

                   # Panel for the axes hiding
                   conditionalPanel(
                     condition = "input.color_boolean && input.plotType == 'Scatterplot' || input.plotType == 'Multiple Scatterplot' || input.plotType == 'Boxplot'",
                     textInput("legend_title", label = "Legend Title", value = "Legend"),
                   ),

                   # Panel for changing the names of the conditions
                   conditionalPanel(
                     condition = "input.plotType == 'Multiple Scatterplot' || input.plotType == 'Boxplot'",
                     textInput("multiple_condition_title1", label = "Condition 1", value = ""),
                     textInput("multiple_condition_title2", label = "Condition 2", value = ""),
                   ),


                   
                 )
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

