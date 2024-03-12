shinyServer(function(input, output, session) {
  
  # This is solely to check what we are downloading/adding
  observe({
    print("File uploaded")
    print(input$file1)
  })
  
  # We want a reactive expression here in order to return the data from the spreadsheet
  data1 <- reactiveValues(data = NULL)
  x_col <- reactiveValues(name = NULL)
  y_col <- reactiveValues(name = NULL)
  print(data1)
  
  
  observeEvent(input$file1, {
    req(input$file1)
    data1$data <- read_excel(input$file1$datapath)
    data1$data <- data1$data %>% select(where(is.numeric))
    
    updateSelectInput(session, "x_column", choices = names(data1$data))
    updateSelectInput(session, "y_column", choices = names(data1$data))
    
    x_col <- input$x_column[1]
    y_col <- input$y_column[1]
  })
  
  observeEvent(input$x_column, {
    print("X_column Changed")
    print("Original Data")
    print(data1$data)
    print("Filtered Data")
    
    if(!is.null(data1$data)){
      data_filtered <- data1$data %>%
        select(input$x_column,input$y_column)
      # Getting rid of different data points
      data_filtered <-na.omit(data_filtered)
      print(data_filtered)
    }
    
    
    
    
    
  })
  observeEvent(input$y_column, {
    print("y_column Changed")
    if(!is.null(data1$data)){
      data_filtered <- data1$data %>%
        select(input$x_column,input$y_column)
      # Getting rid of different data points
      data_filtered <-na.omit(data_filtered)
      print(data_filtered)
    }
    
    
  })
  
  
  
})
