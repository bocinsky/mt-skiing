library(magrittr)

list(readr::read_rds("./data/data-derived/maca_midcentury_medians.Rds"),
       readr::read_rds("./data/data-derived/maca_normal_medians.Rds")) %>%
  purrr::map(function(x){
    names(x) %<>%
      basename() %>%
      tools::file_path_sans_ext()
    x
  }) %>%
  unlist() %>%
  tibble::tibble(Model = names(.),Data = .) %>%
  tidyr::separate(Model,into = c("Measurement","Model","Run","Scenario"), sep = "_") %>%
  tidyr::pivot_wider(names_from = Measurement, values_from = Data) %>%
  dplyr::mutate(pr = pr %>%
                  purrr::map(function(x){
                    x[] %<>%
                      # magrittr::divide_by(10) %>%
                      units::set_units("mm") %>%
                      units::set_units("in") %>%
                      units::drop_units()
                    x %>%
                      raster::crop(mcor::mt_state_simple %>%
                                     sf::st_transform(4326),
                                   snap = "out") %>%
                      raster::mask(mcor::mt_state_simple %>%
                                     sf::st_transform(4326))
                  }),
                tasmin = tasmin %>%
                  purrr::map(function(x){
                    x[] %<>%
                      # magrittr::divide_by(10) %>%
                      units::set_units("degK") %>%
                      units::set_units("degF") %>%
                      units::drop_units()
                    x %>%
                      raster::crop(mcor::mt_state_simple %>%
                                     sf::st_transform(4326),
                                   snap = "out") %>%
                      raster::mask(mcor::mt_state_simple %>%
                                     sf::st_transform(4326))
                  }),
                tasmax = tasmax %>%
                  purrr::map(function(x){
                    x[] %<>%
                      # magrittr::divide_by(10) %>%
                      units::set_units("degK") %>%
                      units::set_units("degF") %>%
                      units::drop_units()
                    x %>%
                      raster::crop(mcor::mt_state_simple %>%
                                     sf::st_transform(4326),
                                   snap = "out") %>%
                      raster::mask(mcor::mt_state_simple %>%
                                     sf::st_transform(4326))
                  })) %>%
  tidyr::pivot_longer(cols = pr:tasmin,
                      names_to = "Measurement",
                      values_to = "Data") %>%
  tidyr::pivot_wider(names_from = Scenario, values_from = Data) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(Difference = list(rcp85 - historical)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_longer(cols = rcp85:Difference,
                      names_to = "Scenario",
                      values_to = "Data") %>%
  dplyr::mutate(Data = Data %>%
                  purrr::map(raster::unstack) %>%
                  purrr::map(function(x){
                    tibble::tibble(Month = month.abb,
                                   Data = x)})) %>%
  tidyr::unnest(Data) %>%
  dplyr::group_by(Measurement,Scenario,Month) %>%
  dplyr::summarise(Data = list(raster::brick(Data))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Data = Data %>%
                  purrr::map(
                    function(x){
                      raster::calc(x,median,na.rm = TRUE)
                    }
                  )) %>%
  dplyr::mutate(Measurement = factor(Measurement,
                                     levels = c("pr","tasmin","tasmax"),
                                     ordered = T),
                Scenario = factor(Scenario,
                                  levels = c("historical",
                                             "rcp85",
                                             "Difference"),
                                  ordered = T),
                Month = factor(Month,
                               levels = month.abb,
                               ordered = T)

  ) %>%
  dplyr::arrange(Measurement,
                 Scenario,
                 Month) %>%
  readr::write_rds("data/data-derived/maca_data.Rds",
                   compress = "xz")
