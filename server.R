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

  
  
  # Function for generating the plot
  generatePlot <- function(data_filtered, input) {
    # Filters the data fram to only have the columns we want from the select input
    data_filtered <- data1$data %>%
      select(input$x_column,input$y_column)
    # Getting rid of NA values
    data_filtered <-na.omit(data_filtered)
    
    
    
    # Changing the label of the x and y axis
    if(input$x_title == "X Axis"){
      x = input$x_column
    } else {
      x = input$x_title
    }
    if(input$y_title == "Y Axis"){
      y = input$y_column
    } else {
      y = input$y_title
    }
    
    
    
    
    # Plotting Data
    plot <- ggplot(data_filtered, aes_string(x = input$x_column, y = input$y_column)) +
      geom_point(size = input$point_size, color = input$point_color) +
      labs(x = x, y = y, title = input$plot_title) +
      theme(
        legend.position = "none",
        plot.title = element_text(size=20),
        axis.title.x = element_text(angle = 0, hjust = 0.5,size = 30),
        axis.title.y = element_text(angle = 90, vjust = 0.5,size = 30),
        axis.text.x = element_text(size = 25),  # Increase size of x-axis numbers
        axis.text.y = element_text(size = 25),  # Increase size of y-axis numbers
        text = element_text(family = "Arial")
      )
    
    
    # Plotting in case there are gridlines
    if (input$gridlines && input$minor_gridlines) {
       plot +
        theme(plot.background = element_rect(fill = input$background_color),
              panel.background = element_rect(fill = input$background_color),
              panel.grid.major = element_line(color = input$major_gridline_color),
              panel.grid.minor = element_line(color = input$minor_gridline_color)
              )
      # Condition where you only want minor gridlines
    } else if(!input$gridlines && input$minor_gridlines) {
      plot +
        labs(x = input$x_column, y = input$y_column, title = "Scatterplot") +
        theme(plot.background = element_rect(fill = input$background_color),
              panel.background = element_rect(fill = input$background_color),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_line(color = input$minor_gridline_color)
              )
      # Condition where you only want major gridlines
    } else if(input$gridlines && !input$minor_gridlines) {
      plot +
        theme(plot.background = element_rect(fill = input$background_color),
              panel.background = element_rect(fill = input$background_color),
              panel.grid.major = element_line(color = input$major_gridline_color),
              panel.grid.minor = element_blank())
      # Condition where you want neither
    } else if(!input$gridlines && !input$minor_gridlines) {
      plot +
        theme(plot.background = element_rect(fill = input$background_color),
              panel.background = element_rect(fill = input$background_color),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }
  }

  
  # This actually outputs the plot
  output$plot <- renderPlot({
    
    # Requires the data, x column, and y column data before running
    req(data1$data)
    req(input$y_column)
    req(input$x_column)
    
    # Calling the function to generate the plot
    generatePlot(data_filtered, input)
    
    
  })
  
  
  
  # Downloading the plot
  output$save_graph <- downloadHandler(
    # Generating generic file name for the initial spot
    filename = function(){
      paste("plot", Sys.Date(), ".jpg", sep = "")
    },

    # Generating the content needed to save the file
    content = function(file) {
      req(data1$data)
      req(input$y_column)
      req(input$x_column)
      
      plot <- generatePlot(data_filtered, input)
      ggsave(file, plot,scale = 1, width = input$plot_width,height = input$plot_height)
    }
  )
  
})
