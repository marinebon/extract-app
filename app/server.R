shinyServer(function(input, output, session) {
  
  vals <- reactiveValues(
    plys         = NULL,
    summary_data = NULL)
  
  # plot ----
  output$plot <- renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  # mapedit ----
  map_ns    <- NS("map_edit")
  map_edits <- callModule(
    editMod, 
    "map_edit", 
    map_init, # "ply",
    editorOptions = list(
      polylineOptions = F, markerOptions = F, circleMarkerOptions = F,
      singleFeature = T))
  
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
    
    d_info <- rerddap::info(id, url = url)
    
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
  
  # observe, map wms ----
  observe({
    req(input$tbl_ed_grd_rows_selected)
    req(input$sel_ed_var)
    
    id  <- get_ed_id()
    var <- input$sel_ed_var
    
    # id = "jplMURSST41mday"; var = "sst"
    wms <- ds_ed %>% 
      filter(Dataset.ID == id) %>% 
      pull(wms)
    
    leafletProxy(map_ns("map")) %>%
      removeTiles("ed_wms") %>% 
      # leaflet() %>% 
      addWMSTiles(
        baseUrl = glue("{wms}?"),
        layerId = "ed_wms",
        layers  = glue("{id}:{var}"),
        options = WMSTileOptions(
          version = "1.3.0", format = "image/png", transparent = T, opacity = 0.7)) # styles = ""
  })
  
  # observe, set vals$plys ----
  observe({
    vals$plys <- map_edits()$finished
  })
  
  # observe, set vals$summary_data ----
  observe({
    req(input$tbl_ed_grd_rows_selected)
    req(input$sel_ed_var)
    req(vals$plys)
    
    # vars: dir_extract, dir_plys
    id  <- get_ed_id()
    var <- input$sel_ed_var
    url <- ds_ed %>% 
      filter(Dataset.ID == id) %>% 
      pull(url)

    dir_grd <- path(dir_extract, "grids", id)
    ed_info <- info(id, url)
    
    #browser()
    # dir.create(dir_plys, showWarnings = F)
    # test_ply_json <- path(dir_plys, "test.geojson")
    # vals$plys %>% sf::write_sf(test_ply_json)
    # vals <- list(
    #   plys = sf::read_sf(test_ply_json))
    # ply       = vals$plys
    # ed_info   = ed_info
    # ed_var    = var
    # dir_tif   = dir_grd
    
    grds <- get_ed_grds(
      ply       = vals$plys, 
      ed_info   = ed_info, 
      ed_var    = var,
      dir_tif   = dir_grd,
      verbose   = T)
    

  })
  
  # map ----
  output$map <- renderLeaflet({
    req(input$tbl_ed_grd_rows_selected)
    req(input$sel_ed_var)

    map_ed(
      id = get_ed_id(), var = input$sel_ed_var)
  })
})
