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
                             selected = "Boxplot"),
                 
                 conditionalPanel(
                   condition = "input.plotType == 'Scatterplot' || input.plotType == 'Multiple Scatterplot'",
                   # Selecting which item you want for each column
                   selectInput("y_column", label = "Y Data",
                               choices = list(""))
                 ),
                 
                 
                 conditionalPanel(
                   condition = "input.plotType == 'Scatterplot' || input.plotType == 'Multiple Scatterplot' || input.plotType == 'Boxplot'",
                   # Selecting which item you want for each column
                   selectInput("x_column", label = "X Data",
                               choices = list(""))
                 ),
                 
                
                 conditionalPanel(
                   condition = "input.plotType == 'Multiple Scatterplot' || input.plotType == 'Boxplot'",
                   # Selecting which item you want for each column of the second set
                   selectInput("x_column2", label = "X Data (Second Set)",
                               choices = list("")),
                   
                   conditionalPanel(
                     condition = "input.plotType == 'Multiple Scatterplot'",
                     selectInput("multiple_color", label = "Color or Shapes",
                                 choices = list("Color","Shapes"),
                                 selected = "Shapes"),
                     conditionalPanel(
                       condition = "input.multiple_color == 'Shapes'",
                       selectInput("shapes1", label = "X1 Shape",
                                   choices = list("Square","Circle","Triangle Point Up","Plus","Cross","Diamond","Triangle Point Down","Square Cross",
                                                  "Star","Diamond Plus","Circle Plus","Triangles Up and Down","Square Plus","Circle Cross",
                                                  "Square and Triangle Down","Filled Square","Filled Circle","Filled Triangle Point-Up","Filled Diamond",
                                                  "Solid Circle","Bullet","Filled Circle Blue","Filled Square Blue","Filled Diamond Blue","Filled Triangle Point-Up Blue",
                                                  "Filled Triangle Point-Down Blue"),
                                   selected = ("Circle")),
                       selectInput("shapes2", label = "X2 Shape",
                                   choices = list("Square","Circle","Triangle Point Up","Plus","Cross","Diamond","Triangle Point Down","Square Cross",
                                                  "Star","Diamond Plus","Circle Plus","Triangles Up and Down","Square Plus","Circle Cross",
                                                  "Square and Triangle Down","Filled Square","Filled Circle","Filled Triangle Point-Up","Filled Diamond",
                                                  "Solid Circle","Bullet","Filled Circle Blue","Filled Square Blue","Filled Diamond Blue","Filled Triangle Point-Up Blue",
                                                  "Filled Triangle Point-Down Blue"),
                                   selected = ("Triangle Point Up"))
                     ),
                   ),
                 ),
                 
                 
                 
                 conditionalPanel(
                   condition = "input.plotType == 'Facet Grid'",
                   
                   # Making the select nodes for Facet Grid
                   selectInput("y_condition_1", label = "Y Data (First Set)",
                               choices = list("")),
                   selectInput("y_condition_2", label = "Y Data (Second Set)",
                               choices = list("")),
                   
                   selectInput("x_condition_1", label = "X Data (First Set)",
                               choices = list("")),
                   selectInput("x_condition_2", label = "X Data (Second Set)",
                               choices = list(""))
                 ),
                 
                
                 ###############################################################
                 # Scatterplots
                 # Checkbox for whether or not it is a scatterplot
                 conditionalPanel(
                   condition = "input.plotType == 'Scatterplot' || input.plotType == 'Multiple Scatterplot' || input.plotType == 'Facet Grid'",
                   
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
                                   choices = list(""))
                     )
                   ),
                   

                   # Panel for Regression Settings
                   conditionalPanel(
                     condition = "input.regression_boolean",
                     # Selecting which item you want for each column
                     selectInput("regression_method", label = "Regression Type",
                                 choices = list("lm","glm","gam","loess")),
                     # Deciding whether or not to choose the confidence interval
                     checkboxInput("regression_se", "Confidence Interval", value = FALSE)
                   )
                 ),
                 
                 
                 #################################################################
                 # Boxplot
                 conditionalPanel(
                   condition = "input.plotType == 'Boxplot'",
                 
                   checkboxInput("boxplot_individual_points_bool", "Individual Points", value = T),
                   checkboxInput("boxplot_mean_bool", "Mean", value = T)
                 )
        ),
        
        
        # Tab 2: Plot Settings
        tabPanel("Plot Settings",
                 # Select inputs for the points on the plot
                 # Select point size
                 sliderInput("point_size", "Point Size", value = 1, min = 0.01, max = 5),
                 
                 # Checkbox for gridlines
                 checkboxInput("gridlines", "Show Major Gridlines", value = F),
                 checkboxInput("minor_gridlines", "Show Minor Gridlines", value = F),
                 checkboxInput("outline_boolean", "Show Plot Outline", value = T),
                 
                 selectInput("legend_position", label = "Legend Position",
                             choices = list("normal","none","top","bottom"), selected = "none"),
                 

                 # Allowing for integer inputs for the range of the inputs
                 numericInput("y_axis_min", "Y Min",value = 0),
                 numericInput("y_axis_max", "Y Max",value = 0),
                 numericInput("x_axis_min", "X Min",value = 0),
                 numericInput("x_axis_max", "X Max",value = 0),
                 

                 # Selecting which item you want for each column of the second set
                 conditionalPanel(
                   condition = "input.plotType != 'Scatterplot'",
                   numericInput("x1_axis_min", "X1 Min",value = 0),
                   numericInput("x1_axis_max", "X1 Max",value = 0)
                 ),
                 
            
                 
                 # Select axes size
                 sliderInput("axes_size", "Axes Size", value = 10, min = 1, max = 30),
                 # Select numbers size
                 sliderInput("num_size", "Number Size", value = 10, min = 1, max = 30),
                 
                 
        ),
        

        tabPanel("Color",
                 
                 
                 # Select point color
                 conditionalPanel(
                   condition = "!input.color_boolean && input.plotType == 'Scatterplot'",
                   colourInput("point_color", "Point Color", "black")
                 ),
                 
                 colourInput("background_color", "Background Color", "white"),
                 colourInput("panel_color", "Panel Color", "white"),
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
                     colourInput("point_color1", "Set Color 1", value = "white"),
                     colourInput("point_color2", "Set Color 2", value = "grey"),
                   ),
                   
                   # Conditional panel for showing the color option for the regression line
                   conditionalPanel(
                     condition = "input.regression_boolean && input.plotType == 'Scatterplot'",
                     colourInput("regression_color", "Regression Color", value = "black")
                     
                     
                   ),
                   
                   # Conditional panel for showing the color option for the regression line
                   conditionalPanel(
                     condition = "input.regression_boolean && input.plotType != 'Scatterplot'",
                     
                     conditionalPanel(
                       condition = "input.plotType = 'Multiple Scatterplot'",
                       colourInput("regression_color_multiple", "Regression Color (Set 2)", value = "red"),
                     )
                   ),
                   # Panel for the axes hiding
                   conditionalPanel(
                     condition = "input.color_boolean || input.plotType = 'Multiple Scatterplot' || input.plotType = 'Boxplot'",
                     colourInput("legend_background", "Legend Background Color", value = "white")
                   ),
                 )
        ),
        
        

        
        tabPanel("Labels",
                 selectInput("Font", label = "Font",
                             choices = list("Arial", "Times New Roman"), selected = "Arial"),
                 
                 # Input for axes and title labels
                 textInput("plot_title", label = "Plot Title", value = ""),
                 textInput("x_title", label = "X Axis Title", value = ""),
                 textInput("y_title", label = "Y Axis Title", value = ""),
                 
                 # Panel for the axes hiding
                 conditionalPanel(
                   condition = "input.color_boolean == TRUE && input.plotType == 'Scatterplot' || input.plotType == 'Multiple Scatterplot' || input.plotType == 'Boxplot'",
                   textInput("legend_title", label = "Legend Title", value = "")
                 ),
                 
                 # Panel for changing the names of the conditions
                 conditionalPanel(
                   condition = "input.plotType == 'Multiple Scatterplot' || input.plotType == 'Boxplot'",
                   textInput("multiple_condition_title1", label = "Condition 1", value = ""),
                   textInput("multiple_condition_title2", label = "Condition 2", value = "")
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

