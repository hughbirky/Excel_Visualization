shinyServer(function(input, output, session) {
  
  observe({
    print("File uploaded")
    print(input$file1)
  })
  
  observeEvent(input$file1, {
    print("File uploaded event triggered")
    # Read the uploaded file
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    df <- read_excel(inFile$datapath, sheet = 1)
    
    # Get column names with numeric values
    numeric_cols <- sapply(df, is.numeric)
    
    # Print numeric column names
    print(names(df)[numeric_cols])
    
    # Update select input with numeric column names
    updateSelectInput(session, "numericColumn", choices = names(df)[numeric_cols])
  })
  
  observeEvent(input$plotType, {
    print("Plot type changed")
    plotType <- input$plotType
    print(plotType)
    if(plotType == "Scatterplot") {
      print("Updating scatterplot select inputs")
      updateSelectInput(session, "x_column", choices = input$numericColumn)
      updateSelectInput(session, "y_column", choices = input$numericColumn)
    } else if (plotType == "Box and Whisker") {
      print("Updating boxplot select input")
      updateSelectInput(session, "boxplot_column", choices = input$numericColumn)
    }
  })
  
  # Dynamic rendering of inputPanel
  output$inputPanel <- renderUI({
    plotType <- input$plotType
    if(plotType == "Scatterplot") {
      # Define the inputs for the scatterplot
      tagList(
        numericInput("n_points", "Number of Points", value = 100),
        sliderInput("x_range", "X Range", min = 0, max = 10, value = c(0, 10)),
        sliderInput("y_range", "Y Range", min = 0, max = 10, value = c(0, 10)),
        selectInput("x_column", "X Column", choices = NULL),
        selectInput("y_column", "Y Column", choices = NULL)
      )
    } else if (plotType == "Box and Whisker") {
      # Define inputs for Box and Whisker
      tagList(
        numericInput("n_bins", "Number of Bins", value = 30),
        sliderInput("data_range", "Data Range", min = 0, max = 100, value = c(0, 100)),
        selectInput("boxplot_column", "Boxplot Column", choices = NULL)
      )
    }
  })
  
  # Creating an output plot
  output$plot <- renderPlot({
    plotType <- input$plotType
    if (plotType == "Scatterplot") {
      # Generate scatterplot
      ggplot(data.frame(x = rnorm(input$n_points), y = rnorm(input$n_points)), aes(x, y)) +
        geom_point() +
        xlim(input$x_range) +
        ylim(input$y_range)
    } else if (plotType == "Box and Whisker") {
      # Generate boxplot
      ggplot(data.frame(x = rnorm(100)), aes(x = 1, y = x)) +
        geom_boxplot()
    }
  })
  
})
