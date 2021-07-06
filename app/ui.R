library(shiny)

shinyUI(fluidPage(
  titlePanel("Extraction"),
  fluidRow(
    column(width = 12,
      tabsetPanel(
        id = "tabs_main", type = "tabs",
        tabPanel(
          "Dataset", 
          helpText("Choose the Dataset and subset of variables, such as time, depth, species, etc, for feeding into summarization Method."),
          tabsetPanel(
            id = "tabs_dataset", type = "tabs",
            tabPanel(
              "ERDDAP",
              tabsetPanel(
                id = "tabs_erddap", type = "tabs",
                tabPanel(
                  "Gridded"),
                tabPanel(
                  "Tabular")))),
            tabPanel(
              "OBIS"),
            tabPanel(
              "DataOne")),
        tabPanel(
          "Place", "Choose the **Place** by selecting from a _MarineRegion_ (MarineRegion.org served by mregions R package) or draw your own area of interest."),
        tabPanel(
          "Method", "Choose the available summarization method such as `mean`, `mean-min-max`, or `mean-sd`."),
        tabPanel(
          "Results", "Download the results. ."))
    ))
))
