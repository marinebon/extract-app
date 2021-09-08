# libraries ----
if (!require("librarian")){
  install.packages("librarian")
  library(librarian)
}
shelf(dplyr, DT, fs, glue, here, 
      leaflet, markdown, purrr, readr, rerddap, 
      shiny, stringr, tidyr)
options(readr.show_col_types = F)

# paths & variables ----
dir_data         <- here("data")
ed_datasets_csv  <- path(dir_data, "ed_datasets.csv")

ed_servers <- c(
  #"CoastWatch" = "https://coastwatch.pfeg.noaa.gov/erddap/",
  "OceanView"  = "https://oceanview.pfeg.noaa.gov/erddap/",
  "GEO-IDE"    = "https://upwell.pfeg.noaa.gov/erddap/")

if (!dir.exists(dir_data)) dir.create(dir_data)

# functions ----
get_ed_datasets <- function(
  servers = ed_servers,
  ds_csv  = ed_datasets_csv, 
  type    = "griddap", # or "tabular"
  redo    = F){
  # servers = ed_servers
  # ds_csv  = ed_datasets_csv
  # type    = "griddap" # or "tabular"
  # redo    = F

  if (!file.exists(ds_csv) | redo){
    ds <- tibble(
      server = names(servers),
      url    = servers[server]) %>% 
      mutate(
        ds = map(url, ~ rerddap::ed_datasets(url = .x, which = type))) %>% 
      unnest(ds) %>% 
    
    # nrow(ds): 9,722 # View(ds)
    #ds %>% 
      mutate(
        Summary = Summary %>% 
          str_replace_all("\n\n", "\n") %>% 
          str_replace_all("\n", ". ")) %>% 
      write_csv(ds_csv) # , quote = "all") # , escape="backslash", eol="\r\n")
  }
  
  # ds3_csv  <- path_ext_set(ds_csv, "_test-info.csv")
  # ds3_json <- path_ext_set(ds_csv, "_test-info.json")
  # ds3 <- read_csv(ds_csv) %>% 
  #   slice(1:3) %>% 
  #   mutate(
  #     info = map2(Dataset.ID, url, ~ rerddap::info(.x, url = .y)))
 
  #ds3 %>% select(ds3$Dataset.ID[1], url)
  
  # y_info <- rerddap::info(ds3$Dataset.ID[1], url = ds3$url[1])
  # y_info$alldata %>% names() tibble()
  
  # write_csv(ds3, ds3_csv)
  # shelf(jsonlite)
  # write_json(ds3, ds3_json)
  # file_exists(ds3_json)
  # ds3r <- jsonlite::read_json(ds3_json)
  # ds3 <- read_csv(ds3_csv)
  # 
  #     d_info$variables %>% 
  #       filter(data_type  == "float") %>% 
  #       pull(variable_name) %>% 
  #       .[[1]]
  #   )
  # nrow(ds2)
  
  read_csv(ds_csv) %>% 
    mutate(
      ERDDAP  = glue("<a href='{griddap}' target='_blank'>{Dataset.ID}</a>")) %>% 
    arrange(desc(server), ERDDAP) %>% 
    filter(
      !duplicated(Dataset.ID)) %>% 
    arrange(server, ERDDAP) # nrow: 11,448 -> 10,430
}

map_ed <- function(id, var, datasets = ds_ed){
  # id = "jplMURSST41mday"; var = "sst"
  d <- datasets %>% filter(Dataset.ID == id)
  
  leaflet(
    options = leafletOptions(
      crs = leafletCRS(crsClass = "L.CRS.EPSG4326"))) %>%
    # basemap from GBIF in 4326
    addTiles("//tile.gbif.org/4326/omt/{z}/{x}/{y}@1x.png?style=gbif-geyser") %>%
    # ERDDAP WMS
    addWMSTiles(
      baseUrl = glue("{d$wms}?"),
      layers = glue("{id}:{var}"),
      options = WMSTileOptions(
        version = "1.3.0", format = "image/png", transparent = T, opacity = 0.7)) # styles = ""
        # time = '2021-07-16T00:00:00Z')) # format(date_end,"%Y-%m-%dT00:00:00Z"))) %>% # defaults to most recent
    # TODO: add legend
    # leaflet::addLegend(
    #   position="bottomright",
    #   title = paste0("SST (°C)"),
    #   colorNumeric("Spectral", c(0,32), reverse=T), seq(0,32))
}


# get data ----
ds_ed <- get_ed_datasets()
