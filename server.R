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
    updateSelectInput(session, "x_column", choices = names(data1$data),selected = names(data1$data)[1])
    updateSelectInput(session, "y_column", choices = names(data1$data), selected = names(data1$data)[2])
    updateSelectInput(session, "x_column2", choices = names(data1$data), selected = names(data1$data)[3])


    # ######################################################################################################
    # ######################################################################################################
    # # In case you want to make multiple that quickly update between and don't have to change every time
    # updateTextInput(session, "multiple_condition_title1", value = "Auditory only")
    # updateTextInput(session, "multiple_condition_title2", value = "Auditory + text")
    # updateTextInput(session, "x_title", value = "Orthographic condition")
    # updateTextInput(session, "x_title", value = " ")
    # 
    # 
    # updateSelectInput(session, "x_column", choices = names(data1$data), selected = "Z_withinpart_AO")
    # updateSelectInput(session, "x_column2", choices = names(data1$data), selected = "Z_withinpart_AT")
    # updateTextInput(session, "y_title", value = "Reaction times (z-scores, wintin participant)")
    # updateNumericInput(session, "y_axis_min", value = -1)
    # updateNumericInput(session, "y_axis_max", value = 1)
    # 
    
    # updateSelectInput(session, "x_column", choices = names(data1$data), selected = "aprime_AO")
    # updateSelectInput(session, "x_column2", choices = names(data1$data), selected = "aprime_AT")
    # updateTextInput(session, "y_title", value = "Sensitivity (A’)")
    # updateNumericInput(session, "y_axis_min", value = 0)
    # updateNumericInput(session, "y_axis_max", value = 1)
    
    
    # updateSelectInput(session, "x_column", choices = names(data1$data), selected = "CR_AO")
    # updateSelectInput(session, "x_column2", choices = names(data1$data), selected = "CR_AT")
    # updateTextInput(session, "y_title", value = "Confidence ratings")
    # updateNumericInput(session, "y_axis_min", value = 0)
    # updateNumericInput(session, "y_axis_max", value = 100)
    
    
    
    
    
    
    # updateSelectInput(session, "x_column", choices = names(data1$data), selected = "aprimeAO")
    # updateSelectInput(session, "x_column2", choices = names(data1$data), selected = "aprimeAT")
    # updateTextInput(session, "y_title", value = "Sensitivity (A’)")
    # updateNumericInput(session, "y_axis_min", value = 0)
    # updateNumericInput(session, "y_axis_max", value = 1)
    
    
    # updateSelectInput(session, "x_column", choices = names(data1$data), selected = "FA_AT_Rat")
    # updateSelectInput(session, "x_column2", choices = names(data1$data), selected = "FA_AT_Rat1_A")
    # updateTextInput(session, "y_title", value = "Confidence ratings")
    # updateNumericInput(session, "y_axis_min", value = 0)
    # updateNumericInput(session, "y_axis_max", value = 100)
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
  
  
  # Checking to see if any of the values have changed
  toListenX <- reactive({
    list(input$x_column,input$x_column2)
  })
  toListenY <- reactive({
    list(input$y_column)
  })
  
  
  # Update ranges every time these change for the x axis
  observeEvent(toListenX(), {

      req(data1$data)  # Ensure data is available
      req(input$x_column, input$y_column)  # Ensure x_column and y_column are selected
      x_max1 <- vector(length = 2)
      x_min1 <- vector(length = 2)

      # Filtering Data
      data1 <- na.omit(data1$data[,names(data1$data) %in% c(input$x_column,input$x_column2,input$y_column)])


      # Finding the min and max of the x and y columns
      x_max1[1] <- max(data1[[input$x_column]], na.rm = TRUE)
      x_max1[2] <- max(data1[[input$x_column2]], na.rm = TRUE)
      x_max <- max(x_max1)
      x_min1[1] <- min(data1[[input$x_column]], na.rm = TRUE)
      x_min1[2] <- min(data1[[input$x_column2]], na.rm = TRUE)
      x_min <- min(x_min1)



      updateNumericInput(session = session, inputId = "x_axis_min", value = x_min)
      updateNumericInput(session = session, inputId = "x_axis_max", value = x_max)
      updateSliderInput(session, "override_x", max = x_max, min = x_min, value = c(x_min, x_max))
  })


  # Update ranges everytime these change for the y axis
  observeEvent(toListenY(), {

      req(data1$data)  # Ensure data is available
      req(input$x_column, input$y_column)  # Ensure x_column and y_column are selected

      # Filtering Data
      data1 <- na.omit(data1$data[,names(data1$data) %in% c(input$x_column,input$x_column2,input$y_column)])


      # Finding the min and max of the x and y column
      y_max <- max(data1[[input$y_column]], na.rm = TRUE)
      y_min <- min(data1[[input$y_column]], na.rm = TRUE)



      updateNumericInput(session = session, inputId = "y_axis_min", value = y_min)
      updateNumericInput(session = session, inputId = "y_axis_max", value = y_max)
      updateSliderInput(session, "override_y", max = y_max, min = y_min, value = c(y_min, y_max))
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
    

    # Checking to see if the person is plotting with color or not
    if(!input$color_boolean){
      # Filters the data fram to only have the columns we want from the select input
      data_filtered <- data1$data %>%
        select(input$x_column,input$y_column)
      # Getting rid of NA values
      data_filtered <-na.omit(data_filtered)
      # Plotting when no color included
      plot <- ggplot(data_filtered, aes_string(x = input$x_column, y = input$y_column)) +
        # Adding in the scatterplot with the color and size variable
        geom_point(size = input$point_size, color = input$point_color) +
        theme(legend.background = element_rect(fill = input$legend_background),
              text = element_text(family = input$Font)) +
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
        # This sets the colors used for the continuous color scale
        scale_color_continuous(low = input$data_color1, high = input$data_color2)+
        # Actually plotting the scatterplot
        geom_point(size = input$point_size) +
        # Labels the axes and title
        labs(x = x, y = y, title = input$plot_title) +
        theme(legend.background = element_rect(fill = input$legend_background),
              text = element_text(family = input$Font),
              ) 
      
      # Adjusting the position of the legend
      if(input$legend_position != "normal"){
        plot <- plot + theme(legend.position = input$legend_position)
      }
    }
    
    
    # Adding a regression line
    if(input$regression_boolean){
      plot <- plot + geom_smooth(method = input$regression_method, se = input$regression_se, color = input$regression_color, fill = input$regression_color)
    }
    
    # Overriding axes bound
    # if(input$override_axes){
    #   plot <- plot + ylim(input$override_y[1],input$override_y[2]) + xlim(input$override_x[1],input$override_x[2])
    # } else {
    plot <- plot + ylim(input$y_axis_min,input$y_axis_max) + xlim(input$x_axis_min,input$x_axis_max)
    # print(paste0(input$y_axis_min,input$y_axis_max))
    # }
    
    
    
    # Adding an outline or not
    if(input$outline_boolean){
      # Add outline to the plot
      plot <- plot + theme(panel.border = element_rect(color = input$outline_color,fill = NA))
    } 
    
    # Plotting Data
    plot <- plot  +
      labs(x = x, y = y, title = input$plot_title, colour = legend) +
      theme(
        plot.title = element_text(size=20),
        axis.title.x = element_text(angle = 0, hjust = 0.5,size = input$axes_size),
        axis.title.y = element_text(angle = 90, vjust = 0.5,size = input$axes_size),
        axis.text.x = element_text(size = input$num_size),  # Increase size of x-axis numbers
        axis.text.y = element_text(size = input$num_size),  # Increase size of y-axis numbers
        plot.background = element_rect(fill = input$background_color),
        panel.background = element_rect(fill = input$panel_color),
      ) 
    
    # Plotting in case there are gridlines
    if (input$gridlines && input$minor_gridlines) {
       plot +
        theme(panel.grid.major = element_line(color = input$major_gridline_color),
              panel.grid.minor = element_line(color = input$minor_gridline_color))
      # Condition where you only want minor gridlines
    } else if(!input$gridlines && input$minor_gridlines) {
      plot +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_line(color = input$minor_gridline_color))
      # Condition where you only want major gridlines
    } else if(input$gridlines && !input$minor_gridlines) {
      plot +
        theme(panel.grid.major = element_line(color = input$major_gridline_color),
              panel.grid.minor = element_blank())
      # Condition where you want neither
    } else if(!input$gridlines && !input$minor_gridlines) {
      plot +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }
    
    
    
  }

 
  #########################################################################
  #########################################################################
  # Multiple Scatterplot function
  # Function for generating the plot that is called later in the script
  generateMultipleScatterplot <- function(data_filtered, input) {
    
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
      

      
      
      
      
      plot <- ggplot(combined_data, aes(x = x_data, y = y_data, lty = type)) +
        geom_point(size = input$point_size, aes(shape = type)) +
        scale_shape_manual(values = c(shape_index1,shape_index2))+
        labs(x = x, y = y, title = input$plot_title) +
        theme(legend.background = element_rect(fill = input$legend_background),
              text = element_text(family = input$Font),
              panel.border = element_rect(color = input$outline_color),
              plot.background = element_rect(fill = input$background_color),
              panel.background = element_rect(fill = input$panel_color))
    }
    

    # Baseline plot for all the stuff needed for all conditions COLOR ONLY
    if(input$multiple_color == "Color"){
        plot <- ggplot(combined_data, aes(x = x_data, y = y_data, color = type)) +
        geom_point(size = input$point_size) +
        labs(x = x, y = y, title = input$plot_title) +
        scale_color_manual(values = c(input$point_color1, input$point_color2), name = input$legend_title) +
        theme(legend.background = element_rect(fill = input$legend_background),
              text = element_text(family = input$Font),
              panel.border = element_rect(color = input$outline_color),
              plot.background = element_rect(fill = input$background_color),
              panel.background = element_rect(fill = input$panel_color))
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
      
    
    # Overriding axes bound
    # if(input$override_axes){
    #   plot <- plot + ylim(input$override_y[1],input$override_y[2]) + xlim(input$override_x[1],input$override_x[2])
    # } else {
      plot <- plot + ylim(input$y_axis_min,input$y_axis_max) + xlim(input$x_axis_min,input$x_axis_max)
      # print(paste0(input$y_axis_min,input$y_axis_max))
    # }
    
    
    # Adding an outline or not
    if(input$outline_boolean){
      # Add outline to the plot
      plot <- plot + theme(panel.border = element_rect(color = input$outline_color,fill = NA))
    } 
    
    
    # Plotting Data
    plot <- plot  +
      labs(x = x, y = y, title = input$plot_title, colour = legend, shape = legend) +
      theme(
        # legend.position = "none",
        plot.title = element_text(size=20),
        axis.title.x = element_text(angle = 0, hjust = 0.5,size = input$axes_size),
        axis.title.y = element_text(angle = 90, vjust = 0.5,size = input$axes_size),
        axis.text.x = element_text(size = input$num_size, face = "bold"),  # Increase size of x-axis numbers
        axis.text.y = element_text(size = input$num_size, face = "bold"),  # Increase size of y-axis numbers
      ) 
    
  
    # Plotting in case there are gridlines
    if (input$gridlines && input$minor_gridlines) {
       plot +
        theme(panel.grid.major = element_line(color = input$major_gridline_color),
              panel.grid.minor = element_line(color = input$minor_gridline_color)
              )
      # Condition where you only want minor gridlines
    } else if(!input$gridlines && input$minor_gridlines) {
      plot +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_line(color = input$minor_gridline_color)
              )
      # Condition where you only want major gridlines
    } else if(input$gridlines && !input$minor_gridlines) {
      plot +
        theme(panel.grid.major = element_line(color = input$major_gridline_color),
              panel.grid.minor = element_blank())
      # Condition where you want neither
    } else if(!input$gridlines && !input$minor_gridlines) {
      plot +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }
  }
  

  #########################################################################
  #########################################################################
  # Boxplot function
  # Function for generating the plot that is called later in the script
  generateBoxplot <- function(data_filtered, input) {
    
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
    
    if(input$boxplot_individual_points_bool){
      boxplot_outlier_shape = NA
    } else{
      boxplot_outlier_shape = 1
    }
    
    # Baseline plot for all the stuff needed for all conditions
    plot <- ggplot(combined_data, aes(x = type, y = x_data, fill = type)) +
      geom_boxplot(outlier.shape = boxplot_outlier_shape) +
      scale_fill_manual(values = c(input$point_color1, input$point_color2), name = legend) +
      theme(legend.background = element_rect(fill = input$legend_background),
            text = element_text(family = input$Font),
            plot.background = element_rect(fill = input$background_color),
            panel.background = element_rect(fill = input$panel_color),
            axis.title.x = element_text(angle = 0, hjust = 0.5,size = 10), # FIX THIS BECAUSE I PUT IT HERE FROM THE LOWER SECTION THAT DOESN"T WORK
            axis.title.y = element_text(angle = 90, vjust = 0.5,size = 10),
            axis.text.x = element_text(size = 10,face = "bold"),  # Increase size of x-axis numbers
            axis.text.y = element_text(size = 10)) +
      labs(x = x, y = y) + coord_cartesian(ylim = c(input$y_axis_min,input$y_axis_max))
      

    
    # Adding individual points
    if(input$boxplot_individual_points_bool){
      # Add outline to the plot
      set.seed(input$seed)
      plot <- plot + geom_jitter(color="black", size=2.5, alpha=0.9,height = 0,width = 0.25,shape = 1,stroke = 1)
    } 
    
    
    # Adding an outline or not
    if(input$outline_boolean){
      # Add outline to the plot
      plot <- plot + theme(panel.border = element_rect(color = input$outline_color,fill = NA))
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

    
    # Plotting Data
    plot +
      theme(plot.title = element_text(size=20),
        axis.title.x = element_text(angle = 0, hjust = 0.5,size = 50),
        axis.title.y = element_text(angle = 90, vjust = 0.5,size = 50),
        axis.text.x = element_text(size = 50),  # Increase size of x-axis numbers
        axis.text.y = element_text(size = 50),  # Increase size of y-axis numbers
        # axis.title.x = element_text(angle = 0, hjust = 0.5,size = input$axes_size),
        # axis.title.y = element_text(angle = 90, vjust = 0.5,size = input$axes_size),
        # axis.text.x = element_text(size = input$num_size),  # Increase size of x-axis numbers
        # axis.text.y = element_text(size = input$num_size),  # Increase size of y-axis numbers
        text = element_text(family = input$Font),
        # legend.title = element_text(input$legend_title)
        # plot.border = element_rect(color = "black",size = 1)  # Border around the plot
      ) +
      xlab(x) +
      ylab(y) + 
      labs(legend = legend)+
      scale_x_discrete(labels = c("A","B")) 
    
    
    # Plotting in case there are gridlines
    if (input$gridlines && input$minor_gridlines) {
      plot +
        theme(panel.grid.major = element_line(color = input$major_gridline_color),
              panel.grid.minor = element_line(color = input$minor_gridline_color)
        )
      # Condition where you only want minor gridlines
    } else if(!input$gridlines && input$minor_gridlines) {
      plot +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_line(color = input$minor_gridline_color)
        )
      # Condition where you only want major gridlines
    } else if(input$gridlines && !input$minor_gridlines) {
      plot +
        theme(panel.grid.major = element_line(color = input$major_gridline_color),
              panel.grid.minor = element_blank())
      # Condition where you want neither
    } else if(!input$gridlines && !input$minor_gridlines) {
      plot +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }
  }
  
  
  # #########################################################################
  # #########################################################################
  # # Facet Grid function
  # # Function for generating the plot that is called later in the script

  
  
  
  
  
  
  
  
  
  
  
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
      req(data1$data)
      req(input$y_column)
      req(input$x_column)
      
      if(input$plotType == "Scatterplot"){
        plot <- generateScatterplot(data_filtered, input)
      } else if(input$plotType == "Multiple Scatterplot"){
        req(input$x_column2)
        plot <- generateMultipleScatterplot(data_filtered, input)
      } else if(input$plotType == "Boxplot"){
        req(input$x_column2)
        plot <- generateBoxplot(data_filtered, input)
      }
      ggsave(file, plot,scale = 1, width = input$plot_width,height = input$plot_height)
    }
  )
})
