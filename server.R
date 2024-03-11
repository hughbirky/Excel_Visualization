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
    # print(names(df)[numeric_cols])
    
    # Update select input with numeric column names
    updateSelectInput(session, "x_column", choices = names(df)[numeric_cols])
    updateSelectInput(session, "y_column", choices = names(df)[numeric_cols])
  })
  
  observeEvent(input$plotType, {
    print("Plot type changed")
    plotType <- input$plotType
    print(plotType)
    if(plotType == "1") {
      print("Updating scatterplot select inputs")
    } else if (plotType == "2") {
      print("Updating boxplot select input")
      # updateSelectInput(session, "boxplot_column", choices = input$numericColumn)
    }
  })
  
  # Dynamic rendering of inputPanel
  output$inputPanel <- renderUI({
    plotType <- input$plotType
    if(plotType == "1") {
      # Define the inputs for the scatterplot
      tagList(
        numericInput("n_points", "Number of Points", value = 100),
        sliderInput("x_range", "X Range", min = 0, max = 10, value = c(0, 10)),
        sliderInput("y_range", "Y Range", min = 0, max = 10, value = c(0, 10))
      )
    } else if (plotType == "2") {
      # Define inputs for Box and Whisker
      tagList(
        numericInput("n_bins", "Number of Bins", value = 30),
        sliderInput("data_range", "Data Range", min = 0, max = 100, value = c(0, 100)),
      )
    }
  })
  
  # Creating an output plot
  output$plot <- renderPlot({
    plotType <- input$plotType
    if(!is.null(input$file1)){
      if (plotType == "1") {
        # Generate scatterplot
        print(is.character(input$x_column))
        ggplot(aes_string(x = input$x_column,y = input$y_column)) +
          geom_point() +
          xlim(input$x_range) +
          ylim(input$y_range)
      } else if (plotType == "2") {
        # Generate boxplot
        ggplot(data.frame(x = rnorm(100)), aes(x = 1, y = x)) +
          geom_boxplot()
      }
    }
  })
  
})
