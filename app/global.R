# libraries ----
if (!require("librarian")){
  install.packages("librarian")
  library(librarian)
}
shelf(dplyr, DT, fs, glue, here, markdown, purrr, readr, rerddap, shiny, tidyr)
options(readr.show_col_types = F)

# paths & variables ----
dir_data         <- here("data")
ed_datasets_csv  <- path(dir_data, "ed_datasets.csv")

ed_servers <- c(
  "CoastWatch" = "https://coastwatch.pfeg.noaa.gov/erddap/",
  "OceanView"  = "https://oceanview.pfeg.noaa.gov/erddap/",
  "GEO-IDE"    = "https://upwell.pfeg.noaa.gov/erddap/")

if (!dir.exists(dir_data)) dir.create(dir_data)

# functions ----
get_ed_datasets <- function(
  servers = ed_servers,
  ds_csv  = ed_datasets_csv, 
  type    = "griddap", # or "tabular"
  redo    = F){

  if (!file.exists(ds_csv) | redo){
    ds <- tibble(
      server = names(servers),
      url    = servers[server]) %>% 
      mutate(
        ds = map(ed_url, ~ rerddap::ed_datasets(url = .x, which = type))) %>% 
      unnest(ds)
    
    write_csv(ds, ds_csv)
  }
  
  read_csv(ds_csv)
}

# get data ----
ds_ed <- get_ed_datasets()
