shinyUI(fluidPage(
  
  # Application title
  titlePanel("Interactive Graphs"),
  
  # Sidebar with a file input and select input
  sidebarLayout(
    sidebarPanel(
      # Input: Select a file
      fileInput("file1", h3("Choose xlsx File"), accept = c(".xlsx")),
      
      # # Selecting which item you want
      # selectInput("plotType", label = h3("Graph Type"),
      #             choices = list("Scatterplot" = 1, "Box and Whisker" = 2)),
      
      # Selecting which item you want for each column
      selectInput("x_column", label = h3("X Data"),
                  choices = list("")),
      selectInput("y_column", label = h3("Y Data"),
                  choices = list("")),
      
      # Select inputs for the points on the plot
      # Select point size
      sliderInput("point_size", "Point Size", value = 1, min = 0.01, max = 5),
      
      # Select point color
      colourInput("point_color", "Point Color", "blue")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      plotOutput("plot")
    )
  )
))
