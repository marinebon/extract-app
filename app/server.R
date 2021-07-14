shinyServer(function(input, output) {
  output$plot <- renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$tbl_ed_grd <- renderDT({
    # input <- list(sel_ed = "https://coastwatch.pfeg.noaa.gov/erddap")
    
    #rerd
    input$sel_ed
    
  })
})
