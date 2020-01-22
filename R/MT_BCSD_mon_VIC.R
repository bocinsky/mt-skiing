library(magrittr)

BCSD_swe_vars <- thredds::tds_ncss_list_vars("https://cida.usgs.gov/thredds/ncss/BCSD_mon_VIC/dataset.html") %$%
  name %>%
  stringr::str_subset("swe") %>%
  stringr::str_subset("_rcp85_") %>%
  sort()

data_out <- "data/data-raw/BCSD_mon_VIC/midcentury/"
dir.create(data_out,
           recursive = TRUE,
           showWarnings = FALSE)

BCSD_swe_vars %>%
  purrr::map_chr(function(var){
    thredds::tds_ncss_download(ncss_url = "https://cida.usgs.gov/thredds/ncss/BCSD_mon_VIC/dataset.html",
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
      raster::subset(
        x %>%
          names() %>%
          gsub("X","",.) %>%
          as.Date(format = "%Y.%m.%d") %>%
          lubridate::year() %in%
          c(2040:2069) %>%
          which()
      )

    midcentury %>%
      raster::stackApply(indices = midcentury %>%
                           names() %>%
                           gsub("X","",.) %>%
                           as.Date(format = "%Y.%m.%d") %>%
                           lubridate::month(),
                         fun = mean) %>%
      magrittr::set_names(month.abb) %>%
      raster::readAll()
  }) %>%
  readr::write_rds("data/data-derived/bcsd_swe_midcentury_means.Rds",
                   compress = "xz")

data_out <- "data/data-raw/BCSD_mon_VIC/normals/"
dir.create(data_out,
           recursive = TRUE,
           showWarnings = FALSE)

BCSD_swe_vars %>%
  purrr::map_chr(function(var){
    thredds::tds_ncss_download(ncss_url = "https://cida.usgs.gov/thredds/ncss/BCSD_mon_VIC/dataset.html",
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
      raster::subset(
        x %>%
          names() %>%
          gsub("X","",.) %>%
          as.Date(format = "%Y.%m.%d") %>%
          lubridate::year() %in%
          c(1980:2009) %>%
          which()
      )

    midcentury %>%
      raster::stackApply(indices = midcentury %>%
                           names() %>%
                           gsub("X","",.) %>%
                           as.Date(format = "%Y.%m.%d") %>%
                           lubridate::month(),
                         fun = mean) %>%
      magrittr::set_names(month.abb) %>%
      raster::readAll()
  }) %>%
  readr::write_rds("data/data-derived/bcsd_swe_normal_means.Rds",
                   compress = "xz")

