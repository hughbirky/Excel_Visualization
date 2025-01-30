shinyServer(function(input, output, session) {
  
  
  # We want a reactive expression here in order to return the data from the spreadsheet
  data1 <- reactiveValues(data = NULL)
  
  # Function to read and process data
  process_data <- function(skip = 0, sheet = NULL) {
    req(input$file1)
    data <- read_excel(input$file1$datapath, sheet = sheet, skip = skip) %>% select(where(is.numeric))
    colnames(data) <- gsub(" ", "_", colnames(data))  # Replace spaces with underscores
    colnames(data) <- gsub("\\(|\\)|\\%", "", colnames(data))
    # print(colnames(data))
    return(data)
  }
  
  # Function to update UI Components from sheet
  update_ui_components <- function() {
    updateSelectInput(session, "x_column", choices = names(data1$data), selected = names(data1$data)[1])
    updateSelectInput(session, "y_column", choices = names(data1$data), selected = names(data1$data)[2])
    updateSelectInput(session, "x_column2", choices = names(data1$data), selected = names(data1$data)[3])
  }
  
  # Function for making data long form
  long_form <- function(){
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
    
    return(combined_data)
    
  }

  
  # Function for setting other plot elements
  set_plot_elements <- function(plot) {
    # Changing the label of the x and y axis
    # If there is no input in the text box, make the title the name of the column
    if(input$x_title == ""){
      x = input$x_column
    } else { x = input$x_title } # Otherwise, make it the text entered 
    # If there is no input in the text box, make the title the name of the column
    if(input$multiple_condition_title1 == ""){
      con1 = input$x_column
    } else { con1 = input$multiple_condition_title1 } # Otherwise, make it the text entered 
    # If there is no input in the text box, make the title the name of the column
    if(input$multiple_condition_title2 == ""){
      con2 = input$x_column2
    } else { con2 = input$multiple_condition_title2 } # Otherwise, make it the text entered 

    
    # If there is no input in the text box, make the title the name of the column
    if(input$y_title == ""){
      y = input$y_column
    } else { y = input$y_title }
    
    # If there is no input in the text box, make the title the name of the column
    if(input$legend_title == ""){
      legend = as.character(input$color_data)
    } else { legend = input$legend_title }
    
    print(paste0(con1, " - ", con2))
    
    
    # print("We got before")
    # Plotting Data
    plot <- plot + labs(x = x, y = y, title = input$plot_title, colour = legend) +
      theme(
        legend.background = element_rect(fill = input$legend_background),
        text = element_text(family = input$Font),
        plot.title = element_text(size=20),
        axis.title.x = element_text(angle = 0, hjust = 0.5,size = input$axes_size),
        axis.title.y = element_text(angle = 90, vjust = 0.5,size = input$axes_size),
        axis.text.x = element_text(size = input$num_size),  # Increase size of x-axis numbers
        axis.text.y = element_text(size = input$num_size),  # Increase size of y-axis numbers
        plot.background = element_rect(fill = input$background_color),
        panel.background = element_rect(fill = input$panel_color),
      ) 
      
      if(input$plotType != "Boxplot"){
        plot <- plot + ylim(input$y_axis_min,input$y_axis_max) + xlim(input$x_axis_min,input$x_axis_max) 
      } else{
        plot <- plot + scale_fill_manual(values = c(input$point_color1, input$point_color2), name = legend) + ylim(input$y_axis_min,input$y_axis_max) +
          scale_x_discrete(labels = c(con1,con2)) +
          labs(x = "")
      }
    
    # Adding an outline or not
    if(input$outline_boolean){plot <- plot + theme(panel.border = element_rect(color = input$outline_color,fill = NA))} 
    
    # Plotting in case there are gridlines
    if (input$gridlines && input$minor_gridlines) {
      plot <- plot +
        theme(panel.grid.major = element_line(color = input$major_gridline_color),
              panel.grid.minor = element_line(color = input$minor_gridline_color))
      # Condition where you only want minor gridlines
    } else if(!input$gridlines && input$minor_gridlines) {
      plot <- plot +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_line(color = input$minor_gridline_color))
      # Condition where you only want major gridlines
    } else if(input$gridlines && !input$minor_gridlines) {
      plot <- plot +
        theme(panel.grid.major = element_line(color = input$major_gridline_color),
              panel.grid.minor = element_blank())
      # Condition where you want neither
    } else if(!input$gridlines && !input$minor_gridlines) {
      plot <- plot +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }
  
    return(plot)
  }
  
  # This observe event watches for when the file is uploaded and executes
  observeEvent(input$file1, {
    # Process data
    data1$data <- process_data(input$skip)
    # Get all sheet names in the uploaded file
    sheet_names <- excel_sheets(input$file1$datapath)
    # Update the selectInput with sheet names
    updateSelectInput(session, "sheet", choices = sheet_names, selected = sheet_names[1])
    update_ui_components()
  })
  
  # Changing the sheet if need be
  observeEvent(input$sheet, {
    data1$data <- process_data(skip = input$skip,sheet = input$sheet)
    # Updating the input nodes on the screen to be the names of the columns
    update_ui_components()
  })
  
  # Changing the sheet if need be
  observeEvent(input$skip, {
    data1$data <- process_data(skip = input$skip,sheet = input$sheet)
    # Updating the input nodes on the screen to be the names of the columns
    update_ui_components()
  })
  
  # Check if the color boolean changes
  observeEvent(input$color_boolean, {
    data1$data <- process_data(skip = input$skip,sheet = input$sheet)
    # Updating the input nodes on the screen to be the names of the columns
    updateSelectInput(session, "color_data", choices = names(data1$data))
    
  })
  
  # Checking to see if any of the values have changed
  toListenX <- reactive({ list(input$x_column,input$x_column2) })
  toListenY <- reactive({ list(input$y_column) })
  
  # Update ranges every time these change for the x axis
  observeEvent(toListenX(), {

    req(data1$data,input$x_column, input$y_column)  # Ensure x_column and y_column are selected
    # Finding the min and max of the x and y columns
    if(input$plotType == "Multiple Scatterplot"){
      # print("Multiple Scatterplot")
      data1 <- na.omit(data1$data[,names(data1$data) %in% c(input$x_column,input$x_column2,input$y_column)])
      x_min <- min(c(data1[[input$x_column]],data1[[input$x_column2]]), na.rm = TRUE)
      x_max <- max(c(data1[[input$x_column]],data1[[input$x_column2]]), na.rm = TRUE)
    } else if(input$plotType == "Boxplot"){
      # print("Boxplot")
      # print("Before")
      data1 <- na.omit(data1$data[,names(data1$data) %in% c(input$x_column,input$x_column2)])
      x_min <- min(c(data1[[input$x_column]],data1[[input$x_column2]]), na.rm = TRUE)
      x_max <- max(c(data1[[input$x_column]],data1[[input$x_column2]]), na.rm = TRUE)
      # print("After")
      updateNumericInput(session = session, inputId = "y_axis_min", value = x_min)
      updateNumericInput(session = session, inputId = "y_axis_max", value = x_max)
    } else if(input$plotType == "Scatterplot"){
      # print("Scatterplot")
      data1 <- na.omit(data1$data[,names(data1$data) %in% c(input$x_column,input$y_column)])
      x_min <- min(data1[[input$x_column]], na.rm = TRUE)
      x_max <- max(data1[[input$x_column]], na.rm = TRUE)
      # print(paste0((x_min), " - ", x_max))
    }
    # Update x axis
    updateNumericInput(session = session, inputId = "x_axis_min", value = x_min)
    updateNumericInput(session = session, inputId = "x_axis_max", value = x_max)
})


  # Update ranges everytime these change for the y axis
  observeEvent(toListenY(), {
      req(data1$data)  # Ensure data is available
      req(input$x_column, input$y_column)  # Ensure x_column and y_column are selected

      # Filtering Data
      if(input$plotType == "Multiple Scatterplot"){
        data1 <- na.omit(data1$data[,names(data1$data) %in% c(input$x_column,input$x_column2,input$y_column)])
      } else if(input$plotType == "Boxplot"){
        data1 <- na.omit(data1$data[,names(data1$data) %in% c(input$x_column,input$x_column2)])
      } else{
        data1 <- na.omit(data1$data[,names(data1$data) %in% c(input$x_column,input$y_column)])
      }

      # Finding the min and max of the x and y column
      y_max <- max(data1[[input$y_column]], na.rm = TRUE)
      y_min <- min(data1[[input$y_column]], na.rm = TRUE)
      # Updating input
      updateNumericInput(session = session, inputId = "y_axis_min", value = y_min)
      updateNumericInput(session = session, inputId = "y_axis_max", value = y_max)
  })
  
  
  #########################################################################
  #########################################################################
  # Scatterplot function
  # Function for generating the plot that is called later in the script
  generateScatterplot <- function(data_filtered, input) {
    # Checking to see if the person is plotting with color or not
    if(!input$color_boolean){
      # Filters the data fram to only have the columns we want from the select input
      data_filtered <- data1$data %>%
        select(input$x_column,input$y_column)
      # Getting rid of NA values
      data_filtered <-na.omit(data_filtered)
      
      
      # Changing column names
      # Plotting when no color included
      plot <- ggplot(data_filtered, aes_string(x = input$x_column, y = input$y_column)) +
        geom_point(size = input$point_size, color = input$point_color)
        
        
    } else {
      # Filters the data fram to only have the columns we want from the select input
      req(input$color_data)
      data_filtered <- data1$data %>%
        select(input$x_column,input$y_column,input$color_data)
      # Getting rid of NA values
      data_filtered <-na.omit(data_filtered)
      # Plotting for color included
      plot <- ggplot(data_filtered, aes_string(x = input$x_column, y = input$y_column, color = input$color_data))+
        geom_point(size = input$point_size) +
        scale_color_continuous(low = input$data_color1, high = input$data_color2)
      
      # Adjusting the position of the legend
      if(input$legend_position != "normal"){
        plot <- plot + theme(legend.position = input$legend_position)
      }
    }
    
    # Adding a regression line
    if(input$regression_boolean){ plot <- plot + geom_smooth(method = input$regression_method, se = input$regression_se, color = input$regression_color, fill = input$regression_color) }
    
    # Adding general plots
    plot <- set_plot_elements(plot)
    
    return(plot)
  }

 
  #########################################################################
  #########################################################################
  # Multiple Scatterplot function
  # Function for generating the plot that is called later in the script
  generateMultipleScatterplot <- function(data_filtered, input) {
    # Combining the data frames
    combined_data <- long_form()
    
    # Baseline plot for all the stuff needed for all conditions SHAPE ONLY
    if(input$multiple_color == "Shapes"){
      # Getting the index number of the shape selected
      shape_index1 = which(c("Square","Circle","Triangle Point Up","Plus","Cross","Diamond","Triangle Point Down","Square Cross",
                                         "Star","Diamond Plus","Circle Plus","Triangles Up and Down","Square Plus","Circle Cross",
                                         "Square and Triangle Down","Filled Square","Filled Circle","Filled Triangle Point-Up","Filled Diamond",
                                         "Solid Circle","Bullet","Filled Circle Blue","Filled Square Blue","Filled Diamond Blue","Filled Triangle Point-Up Blue",
                                         "Filled Triangle Point-Down Blue") == input$shapes1) - 1
      shape_index2 = which(c("Square","Circle","Triangle Point Up","Plus","Cross","Diamond","Triangle Point Down","Square Cross",
                                         "Star","Diamond Plus","Circle Plus","Triangles Up and Down","Square Plus","Circle Cross",
                                         "Square and Triangle Down","Filled Square","Filled Circle","Filled Triangle Point-Up","Filled Diamond",
                                         "Solid Circle","Bullet","Filled Circle Blue","Filled Square Blue","Filled Diamond Blue","Filled Triangle Point-Up Blue",
                                         "Filled Triangle Point-Down Blue") == input$shapes2) - 1
      
      # Making actual plot
      plot <- ggplot(combined_data, aes(x = x_data, y = y_data, lty = type)) +
        geom_point(size = input$point_size, aes(shape = type)) +
        scale_shape_manual(values = c(shape_index1,shape_index2))
    }
    
    # Baseline plot for all the stuff needed for all conditions COLOR ONLY
    if(input$multiple_color == "Color"){
      
      plot <- ggplot(combined_data, aes(x = x_data, y = y_data, color = type)) +
        geom_point(size = input$point_size) +
        scale_color_manual(values = c(input$point_color1, input$point_color2), name = input$legend_title) 
    }
    
    # Adjusting the position of the legend
    if(input$legend_position != "normal"){
      plot <- plot + theme(legend.position = input$legend_position)
    }
    
    # Adding a regression line
    if(input$regression_boolean){
      if(input$multiple_color == "Color"){
        plot <- plot + geom_smooth(method = input$regression_method,se = input$regression_se, show.legend = FALSE)
      } else {
        plot <- plot + geom_smooth(method = input$regression_method,se = input$regression_se,color = input$regression_color_multiple,fill = input$regression_color_multiple,show.legend = FALSE)
      }
    }
      
    # Adding general plots
    plot <- set_plot_elements(plot)
    
    return(plot)
  }
  

  #########################################################################
  #########################################################################
  # Boxplot function
  # Function for generating the plot that is called later in the script
  generateBoxplot <- function(data_filtered, input) {
    # Getting the data in long form
    combined_data <- long_form()
    
    if(input$boxplot_individual_points_bool){
      boxplot_outlier_shape = NA
    } else{
      boxplot_outlier_shape = 1
    }

    
    # Baseline plot for all the stuff needed for all conditions
    plot <- ggplot(combined_data, aes(x = type, y = x_data, fill = type)) +
      geom_boxplot(outlier.shape = boxplot_outlier_shape) 
      
    # Adding individual points
    if(input$boxplot_individual_points_bool){
      # Add outline to the plot
      set.seed(input$seed)
      plot <- plot + geom_jitter(color="black", size=2.5, alpha=0.9,height = 0,width = 0.25,shape = 1,stroke = 1)
    } 
    
    # Adjusting the position of the legend
    if(input$legend_position != "normal"){
      plot <- plot + theme(legend.position = input$legend_position)
    }
    
    # Adding a mean point
    if(input$boxplot_mean_bool){
      # Add outline to the plot
      plot <- plot + stat_summary(fun = mean, geom="point", shape=8, size=4, color="black", fill="black",stroke = 2)  # Add star for mean
    } 
    
    # Adding general plots
    plot <- set_plot_elements(plot)
    return(plot)
    
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
    
    
    # Calling the function to generate the plot
    if(input$plotType == "Scatterplot"){
      generateScatterplot(data_filtered, input)
    } else if(input$plotType == "Multiple Scatterplot"){
      req(input$x_column2)
      generateMultipleScatterplot(data_filtered, input)
    } else if(input$plotType == "Boxplot"){
      req(input$x_column2)
      generateBoxplot(data_filtered, input)
    } else if(input$plotType == "Facet Grid"){
      req(input$x_column2)
      generateFacetGrid(data_filtered, input)
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
      req(data1$data,input$y_column,input$x_column)
      
      plot <- NULL
      if(input$plotType == "Scatterplot"){
        plot <- generateScatterplot(data1$data, input)
      } else if(input$plotType == "Multiple Scatterplot"){
        req(input$x_column2)
        plot <- generateMultipleScatterplot(data1$data, input)
      } else if(input$plotType == "Boxplot"){
        req(input$x_column2)
        plot <- generateBoxplot(data1$data, input)
      }
      ggsave(file, plot,scale = 1, width = input$plot_width,height = input$plot_height)
    }
  )
})
