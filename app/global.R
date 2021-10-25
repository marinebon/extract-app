# libraries ----
if (!require("librarian")){
  install.packages("librarian")
  library(librarian)
}
shelf(dplyr, DT, fs, glue, here, 
      leaflet, lubridate, mapedit, 
      marinebon/seascapeR,
      markdown, purrr, readr, rerddap, 
      shiny, stringr, tidyr)
options(readr.show_col_types = F)

# paths & variables ----
dir_data         <- here("data")
ed_datasets_csv  <- path(dir_data, "ed_datasets.csv")
dir_extract      <- path(dir_data, "extract")
dir_plys         <- path(dir_extract, "plys")

ed_servers <- c(
  #"CoastWatch" = "https://coastwatch.pfeg.noaa.gov/erddap/",
  "OceanView"  = "https://oceanview.pfeg.noaa.gov/erddap/",
  "GEO-IDE"    = "https://upwell.pfeg.noaa.gov/erddap/")
  #"CoastWatch Caribbean/Gulf of Mexico" = "https://cwcgom.aoml.noaa.gov/erddap/")

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
      mutate(
        Summary = Summary %>% 
          str_replace_all("\n\n", "\n") %>% 
          str_replace_all("\n", ". ")) %>% 
      write_csv(ds_csv) # , quote = "all") # , escape="backslash", eol="\r\n")
  }
  
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

# map_edit ----
map_init <-   leaflet(
  options = leafletOptions(
    crs                = leafletCRS(crsClass = "L.CRS.EPSG4326"),
    zoomControl        = T,
    attributionControl = F)) %>%
  # basemap from GBIF in 4326
  addTiles("//tile.gbif.org/4326/omt/{z}/{x}/{y}@1x.png?style=gbif-geyser") %>%
  setView(-93.4, 37.4, 2)
  
# get data ----
ds_ed <- get_ed_datasets()

get_ed_dates <- function(ed_info){
  
  ed_info$alldata$time %>%
    filter(attribute_name=="actual_range") %>%
    pull(value) %>%
    str_split(", ", simplify = T) %>%
    as.numeric() %>%
    as.POSIXct(origin = "1970-01-01", tz = "GMT") %>%
    as.Date()
}

get_ed_dates_all <- function(ed_info, date_beg, date_end){
  # dates = get_ed_dates(ed_info())
  # dates = c(as.Date("2003-01-01"), as.Date("2005-01-20"))
  
  ed_dataset = attr(ed_info, "datasetid")
  
  t_csv <- glue("{ed_info$base_url}/griddap/{ed_dataset}.csvp?time[({date_beg}T12:00:00Z):1:({date_end}T12:00:00Z)]")
  d_t <- try(read_csv(t_csv, col_types = cols()))
  if ("try-error" %in% class(d_t))
    stop(glue("Problem fetching dates from ERDDAP with: {t_csv}"))
  
  d_t %>%
    pull() %>%
    as.Date()
}


get_ed_grds <- function(
  ply,
  ed_info,
  ed_var,
  date_beg  = min(get_ed_dates(ed_info)),
  date_end  = max(get_ed_dates(ed_info)),
  dir_tif   = NULL,
  del_cache = F,
  verbose   = F){
  
  select = dplyr::select
  
  if (del_cache)
    rerddap::cache_delete_all(force = T)
  
  s_dates <- get_ed_dates(ed_info)
  
  date_beg <- as.Date(date_beg)
  date_end <- as.Date(date_end)
  
  if (!int_overlaps(
    interval(  date_beg, date_end  ),
    interval(s_dates[1], s_dates[2]))){
    stop(glue("Date range requested ({date_beg} to {date_end}) does not overlap with Seascapes ({s_dates[1]} to {s_dates[2]})."))
  }
  
  if (date_end > s_dates[2]){
    warning(glue("The date_end {date_end} > Seascapes end ({s_dates[2]}) so decreasing to {s_dates[2]}."))
    date_end <- s_dates[2]
  }
  
  if (date_beg < s_dates[1]){
    warning(glue("The date_beg {date_beg} < Seascape begin ({s_dates[1]}) so increasing to {s_dates[1]}."))
    date_beg <- s_dates[1]
  }
  
  # dates on ERDDAP
  dates_all <- get_ed_dates_all(ed_info, date_beg, date_end)
  
  
  if (verbose)
    message(glue("Found {length(dates_all)} dates between {date_beg} and {date_end}."))
  
  # TODO: handle error with Papahānaumokuākea
  #   ply = get_url_ply("pmnm); ed_info <- get_ed_info();
  #   get_ed_grds(ed_info, ply, date_beg = "2019-01-01", date_end = "2020-01-01")
  #     ERROR: One or both longitude values (-180, 180) outside data range (-179.975, 179.975)
  
  # ed_info   = get_ed_info()
  # ply       = get_url_ply("mbnms")
  # ed_var    = "CLASS"
  # date_beg  = "2019-01-01"; date_end  = "2021-01-01"
  # dir_tif   = here("data_ed/mbnms_global_monthly_2020-01-01_to_2021-01-01")
  # write_tif = T
  
  if (!is.null(dir_tif)){
    dir_create(dir_tif)
    
    tifs <- tibble(
      tif = list.files(
        path = dir_tif,
        pattern=glue('grd_{ed_var}_.*tif$'),
        recursive = T, full.names=T)) %>%
      mutate(
        date_chr = map_chr(tif, function(x){
          basename(x) %>%
            str_replace(glue('grd_{ed_var}_(.*)\\.tif$'), "\\1") %>%
            str_replace_all(fixed("."), "-")}),
        date = as.Date(date_chr))
    
    tifs_match <- tifs %>%
      filter(date %in% dates_all)
    
    if (verbose)
      message(glue("Found {nrow(tifs_match)} matching tifs of {length(dates_all)} dates."))
    
    if (nrow(tifs_match) > 0)
      tbl_tifs <- tifs_match %>%
      mutate(
        raster = map(
          tif, raster::raster))
    
    if (all(dates_all %in% tifs$date)){
      if (verbose)
        message(
          glue("Reading existing grids ([dir_tif]/grd_{ed_var}_[date].tif) vs fetching fresh data via ERDDAP."))
      
      if (nrow(tifs_match) == 1)
        grd <- tbl_tifs$raster[[1]]
      if (nrow(tifs_match) > 1)
        grd <- raster::stack(tbl_tifs$raster)
      
      names(grd) <- names(grd) %>% str_replace("^grd_", "")
      return(grd)
    } else {
      dates_get <- setdiff(dates_all, tifs$date) %>% as.Date(origin="1970-01-01")
    }
  } else {
    dates_get <- dates_all
  }
  
  if (verbose)
    message(glue("Proceeding to fetch {length(dates_get)} from ERDDAP of {length(dates_all)} dates."))
  
  bb <- sf::st_bbox(ply)
  # TODO: check bb's crs==4326 and within range of dataset product
  
  get_ed_raster <- function(date){
    if (verbose)
      message(glue("  griddap({date}, ...)"))
    
    nc <- try(griddap(
      x         = attr(ed_info, "datasetid"),
      fields    = ed_var,
      url       = ed_info$base_url,
      longitude = c(bb["xmin"], bb["xmax"]),
      latitude  = c(bb["ymin"], bb["ymax"]),
      time      = c(date, date),
      fmt       = "nc"))
    
    if ("try-error" %in% nc){
      stop(glue("
        Problem fetching data from ERDDAP server using:
          rerddap::griddap(
            x         = '{attr(ed_info, 'datasetid')}',
            fields    = '{ed_var}',
            url       = '{ed_info$base_url}',
            longitude = c({bb['xmin']}, {bb['xmax']}),
            latitude  = c({bb['ymin']}, {bb['ymax']}),
            time      = c('{date}', '{date}'))"))}
    
    x <- tibble(nc$data) %>%
      mutate(
        # round b/c of uneven intervals
        #   unique(tbl$lon) %>% sort() %>% diff() %>% unique() %>% as.character()
        #     0.0499954223632812 0.0500030517578125
        #   TODO: inform Maria/Joaquin about uneven intervals
        lon  = round(lon, 3),
        lat  = round(lat, 3),
        date = as.Date(time, "%Y-%m-%dT12:00:00Z")) %>%
      select(-time)
    sp::coordinates(x) <- ~ lon + lat
    sp::gridded(x) <- T
    r <- raster::raster(x)
    raster::crs(r) <- 4326
    r
  }
  
  # verbose = T
  
  tbl <- tibble(
    date = dates_get) %>%
    mutate(
      raster = map(date, get_ed_raster))
  
  if (!is.null(dir_tif) && nrow(tifs_match) > 0){
    if (verbose)
      message(glue("Binding {nrow(tbl)} grids from ERDDAP with {nrow(tbl_tifs)} grids from dir_tif."))
    
    tbl <- tbl %>%
      bind_rows(
        tbl_tifs) %>%
      arrange(date)
  }
  
  is_stack <- nrow(tbl) > 1
  
  
  if (verbose)
    message(glue("Reading, masking, naming and writing (if dir_tif) to grd."))
  
  if (!is_stack){
    if (verbose)
      message(glue("  from single raster"))
    
    grd <- tbl$raster[[1]]
    grd <- raster::mask(grd, sf::as_Spatial(ply))
    names(grd) <- glue("{ed_var}_{tbl$date[[1]]}")
    
    if (!is.null(dir_tif))
      raster::writeRaster(
        grd, glue("{dir_tif}/grd_{names(grd)}.tif"), overwrite = T)
    
  } else {
    if (verbose)
      message(glue("  from raster stack"))
    
    grd <- raster::stack(tbl$raster)
    grd <- raster::mask(grd, sf::as_Spatial(ply))
    names(grd) <- glue("{ed_var}_{tbl$date}")
    
    if (!is.null(dir_tif))
      raster::writeRaster(
        grd, paste0(dir_tif,"/grd"), names(grd),
        bylayer=T, format='GTiff', overwrite = T)
  }
  
  # raster::plot(grd); plot(ply, add = T, col = scales::alpha("blue", 0.3))
  grd
}
