shinyServer(function(input, output) {

  output$plot_popularity<- renderPlotly({
    
    selected_start <- as.Date(input$date_range[1])
    selected_end <- as.Date(input$date_range[2])
    selected_language <- input$language
    # Server logic of what should be plotted. We want to see the interactivity
    df_long %>%
      dplyr::filter(language %in% selected_language,
                    timestamp >= selected_start, # Filtering the language column by the languages that are input
                    timestamp <= selected_end) %>% # Filtering the language column by the languages that are input
      ggplot(data = ., aes(x = timestamp, y  = popularity,col = language)) +
      geom_line()
  })  

  
  # Next graph
  output$plot_average_popularity <- renderPlotly({
    selected_start <- as.Date(input$date_range[1])
    selected_end <- as.Date(input$date_range[2])
    selected_language <- input$language
    # Server logic of what should be plotted. We want to see the interactivity
    df_long %>%
      dplyr::filter(language %in% selected_language,
                    timestamp >= selected_start, # Filtering the language column by the languages that are input
                    timestamp <= selected_end) %>% # Filtering the language column by the languages that are input
      group_by(language) %>%
      summarise(ave_pop = mean(popularity))%>%
      ggplot(data=., aes(x = language, y = ave_pop)) +
      geom_col(fill = "blue")
  })
})
