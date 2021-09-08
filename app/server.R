shinyServer(function(input, output) {
  output$plot <- renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$tbl_ed_grd <- renderDT({
    ds_ed %>% 
      select(Server = server, ERDDAP, Dataset = Title)
  }, escape = F, selection = 'single')
  
  output$map <- renderLeaflet({
    req(input$tbl_ed_grd_rows_selected)
    # message(glue("Dataset.ID: {id_ed}"))
    
    map_ed(
      id = ds_ed %>% 
        slice(input$tbl_ed_grd_rows_selected) %>% 
        pull(Dataset.ID))
  })
})
