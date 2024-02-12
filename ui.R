shinyUI(fluidPage(

    # Application title
    titlePanel("Most Popular Programming Languages"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            # We are putting inour input widgets, dropdown input
            # Some of these are the ones that we created in the global R file
            # Selected is the stuff that you start with
            selectInput(inputId = "language", 
                        label = "Language", 
                        choices = language_choices, 
                        selected = c("Python","R"),
                        multiple = T),
          
            dateRangeInput(inputId = "date_range", 
                           label = "Date Range",
                           start = date_start, 
                           end = date_end)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            # Prepare placeholders
          plotlyOutput(outputId = "plot_popularity"),
          plotlyOutput(outputId = "plot_average_popularity")
        )
    )
))
