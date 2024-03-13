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
                 
                # Panel for Scatterplot Settings
                 conditionalPanel(
                   condition = "input.plotType == 'Scatterplot'",
                   # Selecting which item you want for each column
                   selectInput("x_column", label = "X Data",
                               choices = list("")),
                   selectInput("y_column", label = "Y Data",
                               choices = list(""))
                 ), 
                  
                 
                 
                  
                  
        ),
        
        # Tab 2: Exporting Data
        tabPanel("Plot Settings",
                 # Select inputs for the points on the plot
                 # Select point size
                 sliderInput("point_size", "Point Size", value = 1, min = 0.01, max = 5),
                 # Select point color
                 colourInput("point_color", "Point Color", "black"),
                 colourInput("background_color", "Background Color", "lightgrey"),
                 colourInput("panel_color", "Panel Color", "grey"),

                 
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
))
