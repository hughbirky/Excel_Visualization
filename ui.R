shinyUI(fluidPage(

    # Application title
    titlePanel("Read Excel"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          # Input: Select a file ----
          fileInput("file1", h3("Choose xlsx File"),
                    accept = c(".xlsx"),
          
                    ),
          
          # Selecting which item you want
          selectInput("plotType", label = h3("Graph Type"),
                      choices = list("Scatterplot" = 1,"Box and Whisker" = 2,
                                     "Correlation" = 3)
                      ),
          uiOutput("inputPanel") # Render input panel dynamically
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
          
          plotOutput("plot")
          
        )
    ),
        

        # Show a plot of the generated distribution
        mainPanel(
            # Prepare placeholders
          # plotlyOutput(outputId = "plot_popularity"),
          # plotlyOutput(outputId = "plot_average_popularity")
        )
    )
)
