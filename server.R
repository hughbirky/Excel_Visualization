shinyServer(function(input, output) {

  # Collapsed table rendering
  # # Outputting the contents of a table? Rendertable allows you to make a table
  # output$contents <- renderTable({
  #   # This is reading the file1 from the input that comes from the UI
  #   inFile <- input$file1
  #   
  #   # Makes sure that there is a file
  #   req(input$file1)
  #   
  #   # Reads in the file from the input
  #   inFile <- input$file1
  #   
  #   # Reads the file
  #   read_excel(inFile$datapath,1)
  # })
  
  # This is creating the output parameter we are trying to make for the different drop downs
  output$inputPanel <- renderUI({
    
    # Getting what plot they selected from the UI
    plotType <- input$plotType
    
    if(plotType == "Scatterplot") {
      
      # Define the inputs for the scatterplot
      tagList(
        numericInput("n_points", "Number of Points", value = 100),
        sliderInput("x_range", "X Range", min = 0, max = 10, value = c(0, 10)),
        sliderInput("y_range", "Y Range", min = 0, max = 10, value = c(0, 10))
      )
    } else if (plotType == "Box and Whisker") {
      # Define inputs for Box and Whisker
      tagList(
        numericInput("n_bins", "Number of Bins", value = 30),
        sliderInput("data_range", "Data Range", min = 0, max = 100, value = c(0, 100))
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
      # Generate histogram
      ggplot(data.frame(x = rnorm(100)), aes(x)) +
        geom_histogram(binwidth = diff(input$data_range) / input$n_bins) +
        xlim(input$data_range)
    }
  })

})
