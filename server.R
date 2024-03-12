shinyServer(function(input, output, session) {
  
  
  # We want a reactive expression here in order to return the data from the spreadsheet
  data1 <- reactiveValues(data = NULL)
  
  # This observe event watches for when the file is uploaded and executes
  observeEvent(input$file1, {
    # This requires the file to exist before executing
    req(input$file1)
    # Assigns the input data to our reactive value
    data1$data <- read_excel(input$file1$datapath)
    # Filters the columns that only contain numeric values
    data1$data <- data1$data %>% select(where(is.numeric))
    
    # Updating the input nodes on the screen to be the names of the columns
    updateSelectInput(session, "x_column", choices = names(data1$data))
    updateSelectInput(session, "y_column", choices = names(data1$data))
    
  })

  # This actually outputs the plot
  output$plot <- renderPlot({
    
    # Requires the data, x column, and y column data before running
    req(data1$data)
    req(input$y_column)
    req(input$x_column)
    
    # Filters the data fram to only have the columns we want from the select input
    data_filtered <- data1$data %>%
      select(input$x_column,input$y_column)
    # Getting rid of NA values
    data_filtered <-na.omit(data_filtered)
    
    # Plots the data on a scatterplot
    ggplot(data_filtered, aes_string(x = input$x_column,y = input$y_column)) +
      geom_point(size = input$point_size, color = input$point_color) +
      labs(x = input$x_column, y = input$y_column, title = "Scatterplot")
    
  })
  
})
