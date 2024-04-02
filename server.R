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
    updateSelectInput(session, "x_column2", choices = names(data1$data))
    
  })
  
  
  
  
  observeEvent(input$color_boolean, {
    # This requires the file to exist before executing
    req(input$file1)
    # Assigns the input data to our reactive value
    data1$data <- read_excel(input$file1$datapath)
    # Filters the columns that only contain numeric values
    data1$data <- data1$data %>% select(where(is.numeric))
    
    # Updating the input nodes on the screen to be the names of the columns
    updateSelectInput(session, "color_data", choices = names(data1$data))
  })
  
  
  # Updating min and max
  observe({
    if(input$override_axes){
      req(data1$data)  # Ensure data is available
      req(input$x_column, input$y_column)  # Ensure x_column and y_column are selected
      x_max1 <- vector(length = 2)
      x_min1 <- vector(length = 2)
      
      # Finding the min and max of the x and y columns
      x_max1[1] <- max(data1$data[[input$x_column]], na.rm = TRUE)
      x_max1[2] <- max(data1$data[[input$x_column2]], na.rm = TRUE)
      x_max <- max(x_max1)
      x_min1[1] <- min(data1$data[[input$x_column]], na.rm = TRUE)
      x_min1[2] <- min(data1$data[[input$x_column2]], na.rm = TRUE)
      x_min <- min(x_min1)
      y_max <- max(data1$data[[input$y_column]], na.rm = TRUE)
      y_min <- min(data1$data[[input$y_column]], na.rm = TRUE)
      
      
      # Update the sliderInput with new max and min values
      updateSliderInput(session, "override_x", max = x_max, min = x_min, value = c(x_min, x_max))
      updateSliderInput(session, "override_y", max = y_max, min = y_min, value = c(y_min, y_max))
    }
  })
  

  
  
  
  
  
  
  
  
  
  
  #########################################################################
  #########################################################################
  # Scatterplot function
  # Function for generating the plot that is called later in the script
  generateScatterplot <- function(data_filtered, input) {
    
    
    # Changing the label of the x and y axis
    # If there is no input in the text box, make the title the name of the column
    if(input$x_title == ""){
      x = input$x_column
    } else {
      # Otherwise, make it the text entered
      x = input$x_title
    }
    # If there is no input in the text box, make the title the name of the column
    if(input$y_title == ""){
      y = input$y_column
    } else {
      # Otherwise, make it the text entered
      y = input$y_title
    }
    # If there is no input in the text box, make the title the name of the column
    if(input$legend_title == ""){
      legend = input$color_data
    } else {
      # Otherwise, make it the text entered
      legend = input$legend_title
    }
    
    print(input$plotType)
    
    # Checking to see if the person is plotting with color or not
    if(!input$color_boolean){
      # Filters the data fram to only have the columns we want from the select input
      data_filtered <- data1$data %>%
        select(input$x_column,input$y_column)
      # Getting rid of NA values
      data_filtered <-na.omit(data_filtered)
      # Plotting when no color included
      plot <- ggplot(data_filtered, aes_string(x = input$x_column, y = input$y_column)) +
        geom_point(size = input$point_size, color = input$point_color) +
        labs(x = x, y = y, title = input$plot_title)
    } else {
      # Filtering to include color
      # Filters the data fram to only have the columns we want from the select input
      req(input$color_data)
      data_filtered <- data1$data %>%
        select(input$x_column,input$y_column,input$color_data)
      # Getting rid of NA values
      data_filtered <-na.omit(data_filtered)
      
      # Plotting for color included
      plot <- ggplot(data_filtered, aes_string(x = input$x_column, y = input$y_column, color = input$color_data))+
        scale_color_continuous(low = input$data_color1, high = input$data_color2)+
        geom_point(size = input$point_size) +
        labs(x = x, y = y, title = input$plot_title) +
        theme(legend.background = element_rect(fill = input$legend_background))
    }
    
    
    
    
    
    
    # Adding a regression line
    if(input$regression_boolean){
      plot <- plot + geom_smooth(method = input$regression_method, se = input$regression_se, color = input$regression_color, fill = input$regression_color)
    }
    
    
    # Overriding axes bound
    if(input$override_axes){
      plot <- plot + ylim(input$override_y[1],input$override_y[2]) + xlim(input$override_x[1],input$override_x[2])
    }
    
    
    
    # Plotting Data
    plot <- plot  +
      labs(x = x, y = y, title = input$plot_title, colour = legend) +
      theme(
        # legend.position = "none",
        plot.title = element_text(size=20),
        axis.title.x = element_text(angle = 0, hjust = 0.5,size = input$axes_size),
        axis.title.y = element_text(angle = 90, vjust = 0.5,size = input$axes_size),
        axis.text.x = element_text(size = input$num_size),  # Increase size of x-axis numbers
        axis.text.y = element_text(size = input$num_size),  # Increase size of y-axis numbers
        text = element_text(family = "Arial"),
        # legend.title = element_text(input$legend_title)
        # plot.border = element_rect(color = "black",size = 1)  # Border around the plot
      ) 
    
    
    
    
    
    
    
    
    # Plotting in case there are gridlines
    if (input$gridlines && input$minor_gridlines) {
       plot +
        theme(plot.background = element_rect(fill = input$background_color),
              panel.background = element_rect(fill = input$panel_color),
              panel.grid.major = element_line(color = input$major_gridline_color),
              panel.grid.minor = element_line(color = input$minor_gridline_color)
              )
      # Condition where you only want minor gridlines
    } else if(!input$gridlines && input$minor_gridlines) {
      plot +
        labs(x = input$x_column, y = input$y_column, title = "Scatterplot") +
        theme(plot.background = element_rect(fill = input$background_color),
              panel.background = element_rect(fill = input$panel_color),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_line(color = input$minor_gridline_color)
              )
      # Condition where you only want major gridlines
    } else if(input$gridlines && !input$minor_gridlines) {
      plot +
        theme(plot.background = element_rect(fill = input$background_color),
              panel.background = element_rect(fill = input$panel_color),
              panel.grid.major = element_line(color = input$major_gridline_color),
              panel.grid.minor = element_blank())
      # Condition where you want neither
    } else if(!input$gridlines && !input$minor_gridlines) {
      plot +
        theme(plot.background = element_rect(fill = input$background_color),
              panel.background = element_rect(fill = input$panel_color),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }
  }

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  #########################################################################
  #########################################################################
  # Multiple Scatterplot function
  # Function for generating the plot that is called later in the script
  generateMultipleScatterplot <- function(data_filtered, input) {
    
    print(paste0("Generate multiple x column: ", input$x_column))
    # Changing the label of the x and y axis
    # If there is no input in the text box, make the title the name of the column
    if(input$x_title == ""){
      x = input$x_column
    } else {
      # Otherwise, make it the text entered
      x = input$x_title
    }
    # If there is no input in the text box, make the title the name of the column
    if(input$y_title == ""){
      y = input$y_column
    } else {
      # Otherwise, make it the text entered
      y = input$y_title
    }
    # If there is no input in the text box, make the title the name of the column
    if(input$legend_title == ""){
      legend = input$x_column
    } else {
      # Otherwise, make it the text entered
      legend = input$legend_title
    }
    
    
    
    # Making the first data frame to hold the info from the first item
    data1_filtered <- data.frame(
      x_data <- data1$data[,input$x_column],
      y_data <- data1$data[,input$y_column]
      )
    

    
    
    
    
    # Doesn't like when the columns are named the same thing
    if(input$x_column == input$y_column){
      # Renaming the x column and getting rid of NAs
      data1_filtered <- data1_filtered %>%
        rename(x_data = input$x_column) %>%
        rename(y_data = paste0(input$y_column,".1")) 
    } else{
      # Renaming the x column and getting rid of NAs
      data1_filtered <- data1_filtered %>%
        rename(x_data = input$x_column) %>%
        rename(y_data = paste0(input$y_column))
    }
    
    
    # Making the second data frame
    data2_filtered <- data.frame(
      x_data <- data1$data[,input$x_column2],
      y_data <- data1$data[,input$y_column]
    )
    
    
    
    
    
    
    
    
    
    
    # Doesn't like when the columns are named the same thing
    if(input$x_column2 == input$y_column){
      # Renaming the x column and getting rid of NAs
      data2_filtered <- data2_filtered %>%
        rename(x_data = input$x_column2) %>%
        rename(y_data = paste0(input$y_column,".1")) 
    } else{
      # Renaming the x column and getting rid of NAs
      data2_filtered <- data2_filtered %>%
        rename(x_data = input$x_column2) %>%
        rename(y_data = paste0(input$y_column))
    }
    
  
    # Adding types for the titles
    if(input$multiple_condition_title1 == ""){
      data1_filtered$type <- input$x_column
    } else{
      data1_filtered$type <- input$multiple_condition_title1
    }
    if(input$multiple_condition_title2 == ""){
      data2_filtered$type <- input$x_column2
    } else{
      data2_filtered$type <- input$multiple_condition_title2
    }

    

    
    
  
    
    # Combining the data frames
    combined_data <- rbind(data1_filtered,data2_filtered)
    
    
    # Baseline plot for all the stuff needed for all conditions
    plot <- ggplot(combined_data, aes(x = x_data, y = y_data, color = type)) +
      geom_point(size = input$point_size) +
      labs(x = x, y = y, title = input$plot_title) +
      scale_color_manual(values = c(input$point_color1, input$point_color2), name = input$legend_title) +
      theme(legend.background = element_rect(fill = input$legend_background))
    
    # Adding a regression line
    if(input$regression_boolean){
      plot <- plot + geom_smooth(method = input$regression_method, aes(color = type), se = input$regression_se)
    }
      
    # Overriding axes bound
    if(input$override_axes){
      plot <- plot + ylim(input$override_y[1],input$override_y[2]) + xlim(input$override_x[1],input$override_x[2])
    }
    
    
    # Plotting Data
    plot <- plot  +
      labs(x = x, y = y, title = input$plot_title, colour = legend) +
      theme(
        # legend.position = "none",
        plot.title = element_text(size=20),
        axis.title.x = element_text(angle = 0, hjust = 0.5,size = input$axes_size),
        axis.title.y = element_text(angle = 90, vjust = 0.5,size = input$axes_size),
        axis.text.x = element_text(size = input$num_size),  # Increase size of x-axis numbers
        axis.text.y = element_text(size = input$num_size),  # Increase size of y-axis numbers
        text = element_text(family = "Arial"),
        # legend.title = element_text(input$legend_title)
        # plot.border = element_rect(color = "black",size = 1)  # Border around the plot
      ) 
    
    
    
    
    
    
    
    # Plotting in case there are gridlines
    if (input$gridlines && input$minor_gridlines) {
       plot +
        theme(plot.background = element_rect(fill = input$background_color),
              panel.background = element_rect(fill = input$panel_color),
              panel.grid.major = element_line(color = input$major_gridline_color),
              panel.grid.minor = element_line(color = input$minor_gridline_color)
              )
      # Condition where you only want minor gridlines
    } else if(!input$gridlines && input$minor_gridlines) {
      plot +
        labs(x = input$x_column, y = input$y_column, title = "Scatterplot") +
        theme(plot.background = element_rect(fill = input$background_color),
              panel.background = element_rect(fill = input$panel_color),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_line(color = input$minor_gridline_color)
              )
      # Condition where you only want major gridlines
    } else if(input$gridlines && !input$minor_gridlines) {
      plot +
        theme(plot.background = element_rect(fill = input$background_color),
              panel.background = element_rect(fill = input$panel_color),
              panel.grid.major = element_line(color = input$major_gridline_color),
              panel.grid.minor = element_blank())
      # Condition where you want neither
    } else if(!input$gridlines && !input$minor_gridlines) {
      plot +
        theme(plot.background = element_rect(fill = input$background_color),
              panel.background = element_rect(fill = input$panel_color),
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
    
    # Observing for when they change
    observe({
      input$x_column
      input$y_column
      input$x_column2
      input$color_data
    })
    
    
    print(paste0("X_Column: ",input$x_column))
    # Calling the function to generate the plot
    if(input$plotType == "Scatterplot"){
      generateScatterplot(data_filtered, input)
    } else if(input$plotType == "Multiple Scatterplot"){
      req(input$x_column2)
      generateMultipleScatterplot(data_filtered, input)
    }
    
    
    
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
      
      plot <- generateScatterplot(data_filtered, input)
      ggsave(file, plot,scale = 1, width = input$plot_width,height = input$plot_height)
    }
  )
  
})
