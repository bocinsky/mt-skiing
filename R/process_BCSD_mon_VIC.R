library(magrittr)

list(Midcentury = readr::read_rds("./data/data-derived/bcsd_swe_midcentury_means.Rds"),
       Normals = readr::read_rds("./data/data-derived/bcsd_swe_normal_means.Rds")) %>%
  purrr::map(function(x){
    names(x) %<>%
      basename() %>%
      tools::file_path_sans_ext()
    x
  }) %>%
  purrr::transpose() %>%
  purrr::map(function(x){
    raster::unstack(x$Midcentury - x$Normals)
  }) %>%
  purrr::transpose() %>%
  purrr::map(raster::brick) %>%
  purrr::map(raster::calc,
             function(x){
               quantile(x,
                        probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                        na.rm = TRUE)
             }
  ) %>%
  purrr::map(function(x){
    x[] %<>%
      units::set_units("mm") %>%
      units::set_units("in") %>%
      units::drop_units()
    x %>%
      raster::crop(mcor::mt_state_simple %>%
                     sf::st_transform(4326),
                   snap = "out") %>%
      raster::mask(mcor::mt_state_simple %>%
                     sf::st_transform(4326))
  }) %>%
  readr::write_rds("data/data-derived/BCSD_mon_VIC_data.Rds",
                   compress = "xz")
