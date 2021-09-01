shinyServer(function(input, output) {
  output$plot <- renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$tbl_ed_grd <- renderDT({
    ds_ed %>% 
      mutate(
        Dataset = Title,
        ERDDAP  = glue("<a href='{griddap}' target='_blank'>{Dataset.ID}</a>")) %>% 
      select(ERDDAP, Dataset) %>% 
      arrange(ERDDAP, Dataset)
  }, escape = F)
})
