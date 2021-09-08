shinyServer(function(input, output, session) {
  output$plot <- renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  # get_ed_id() ----
  get_ed_id <- reactive({
    req(input$tbl_ed_grd_rows_selected)
    
    ds_ed %>% 
      slice(input$tbl_ed_grd_rows_selected) %>% 
      pull(Dataset.ID)
  })
  
  # get_ed_vars() ----
  get_ed_vars <- reactive({
    id <- get_ed_id()
    
    url = ds_ed %>% 
      filter(Dataset.ID == id) %>% 
      pull(url)
    
    d_info <- rerddap::info(ed_id, url = ed_url)
    
    d_info$variables$variable_name
  })
  
  # update(sel_ed_var) ----
  observe({
    updateSelectInput(
      session,
      "sel_ed_var",
      choices = get_ed_vars())
  })
  
  # tbl_ed_grd ----
  output$tbl_ed_grd <- renderDT({
    ds_ed %>% 
      select(Server = server, ERDDAP, Dataset = Title)
  }, escape = F, selection = 'single')
  
  # map ----
  output$map <- renderLeaflet({
    req(input$tbl_ed_grd_rows_selected)
    req(input$sel_ed_var)

    map_ed(
      id = get_ed_id(), var = input$sel_ed_var)
  })
})
