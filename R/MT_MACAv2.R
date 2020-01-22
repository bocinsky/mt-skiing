library(magrittr)

maca_vars <-
  thredds::tds_ncss_list_vars("https://cida.usgs.gov/thredds/ncss/macav2metdata_monthly_future/dataset.html") %$%
  name %>%
  stringr::str_subset("pr_|tasmax_|tasmin_") %>%
  stringr::str_subset("_rcp85") %>%
  sort()

data_out <- "data/data-raw/macav2_monthly/midcentury/"
dir.create(data_out,
           recursive = TRUE,
           showWarnings = FALSE)

maca_vars %>%
  purrr::map_chr(function(var){
    thredds::tds_ncss_download(ncss_url = "https://cida.usgs.gov/thredds/ncss/macav2metdata_monthly_future/dataset.html",
                               out_file = stringr::str_c(data_out,var,".nc"),
                               bbox = mcor::mt_state_simple %>%
                                 sf::st_bbox() %>%
                                 sf::st_as_sfc() %>%
                                 lwgeom::st_transform_proj(4326) %>%
                                 # magrittr::add(c(360,0)) %>%
                                 sf::st_bbox(),
                               vars = var,
                               ncss_args = list(time_start = "2040-01-01",
                                                time_end = "2069-12-31"),
                               overwrite = FALSE)
  }) %>%
  purrr::compact() %>%
  magrittr::set_names(.,.) %>%
  purrr::map(raster::brick) %>%
  purrr::map(function(x){
    midcentury <- x %>%
      raster::readAll() %>%
      raster::stackApply(indices = x %>%
                           names() %>%
                           gsub("X","",.) %>%
                           as.Date(format = "%Y.%m.%d") %>%
                           lubridate::month(),
                         fun = median) %>%
      magrittr::set_names(month.abb)
    midcentury %>%
      raster::readAll()
  }) %>%
  readr::write_rds("data/data-derived/maca_midcentury_medians.Rds",
                   compress = "xz")

data_out <- "data/data-raw/macav2_monthly/normals/"
dir.create(data_out,
           recursive = TRUE,
           showWarnings = FALSE)
maca_vars <-
  thredds::tds_ncss_list_vars("https://cida.usgs.gov/thredds/ncss/macav2metdata_monthly_historical/dataset.html") %$%
  name %>%
  stringr::str_subset("pr_|tasmax_|tasmin_") %>%
  sort()

maca_vars %>%
  purrr::map_chr(function(var){
    thredds::tds_ncss_download(ncss_url = "https://cida.usgs.gov/thredds/ncss/macav2metdata_monthly_historical/dataset.html",
                               out_file = stringr::str_c(data_out,var,".nc"),
                               bbox = mcor::mt_state_simple %>%
                                 sf::st_bbox() %>%
                                 sf::st_as_sfc() %>%
                                 lwgeom::st_transform_proj(4326) %>%
                                 # magrittr::add(c(360,0)) %>%
                                 sf::st_bbox(),
                               vars = var,
                               ncss_args = list(time_start = "1980-01-01",
                                                time_end = "2009-12-31"),
                               overwrite = FALSE)
  }) %>%
  purrr::compact() %>%
  magrittr::set_names(.,.) %>%
  purrr::map(raster::brick) %>%
  purrr::map(function(x){
    midcentury <- x %>%
      raster::readAll() %>%
      raster::stackApply(indices = x %>%
                           names() %>%
                           gsub("X","",.) %>%
                           as.Date(format = "%Y.%m.%d") %>%
                           lubridate::month(),
                         fun = median) %>%
      magrittr::set_names(month.abb)
    midcentury %>%
      raster::readAll()
  }) %>%
  readr::write_rds("data/data-derived/maca_normal_medians.Rds",
                   compress = "xz")

