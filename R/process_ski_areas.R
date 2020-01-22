library(magrittr)

ski_areas_rockies <-
  sf::read_sf("data/data-derived/ski_areas_rockies_clean.csv",
              crs = 4326) %>%
  dplyr::select(Name = name,
                Website = official_website,
                `Opening Year` = opening_year,
                `Lift Count` = lift_count,
                `Run Count` = run_count,
                `Longest Run` = longest_run,
                `Skiable Acreage` = skiable_acreage,
                `Top Elevation` = top_elevation,
                `Bottom Elevation` = bottom_elevation,
                `Hourly Lift Capacity` = hourly_lift_capacity,
                `State` = state

  ) %>%
  dplyr::mutate_at(.vars = dplyr::vars(`Opening Year`:`Hourly Lift Capacity`),
                   .funs = as.integer)

ski_areas_rockies_all <-
  readr::read_rds("./data/data-derived/ski_areas_rockies_all.rds")

ski_areas_rockies %>%
  dplyr::left_join(
    ski_areas_rockies_all %>%
      dplyr::mutate_at(.vars = dplyr::vars(Precipitation:`Snow Water Equivalent`), units::drop_units) %>%
      tidyr::pivot_longer(cols = Precipitation:`Snow Water Equivalent`,
                          names_to = "Measurement",
                          values_to = "Value") %>%
      dplyr::filter(Year %in% c(1980:2009,2040:2069)) %>%
      dplyr::mutate(Scenario = ifelse(Year %in% c(1980:2009),"historical","rcp85")) %>%
      dplyr::group_by(Name,Month,Model,Scenario,Measurement) %>%
      dplyr::summarise(Value = median(Value)) %>%
      tidyr::pivot_wider(names_from = Scenario, values_from = Value) %>%
      dplyr::mutate(Difference = rcp85 - historical) %>%
      tidyr::pivot_longer(cols = historical:Difference,
                          names_to = "Scenario",
                          values_to = "Value") %>%
      dplyr::group_by(Name,Month,Scenario,Measurement) %>%
      dplyr::summarise(Value = median(Value)) %>%
      tidyr::pivot_wider(names_from = Measurement, values_from = Value) %>%
      dplyr::group_by(Name) %>%
      tidyr::nest(),
    by = "Name"
  ) %>%
  readr::write_rds("./data/data-derived/ski_areas_data.rds",
                   compress = "bz")

ski_area_quants <-
  ski_areas_rockies_all %>%
  dplyr::filter(Year >= 1980,
                Year < 2070) %>%
  dplyr::mutate_at(.vars = dplyr::vars(Precipitation:`Snow Water Equivalent`), units::drop_units) %>%
  tidyr::pivot_longer(cols = Precipitation:`Snow Water Equivalent`,
                      names_to = "Measurement",
                      values_to = "Value") %>%
  dplyr::group_by(Name,Year,Month,Measurement) %>%
  dplyr::summarise(Value =
                     Value %>%
                     list()) %>%
  dplyr::mutate(Value =
                  Value %>%
                  purrr::map(median) %>%
                  unlist()) %>%
  tidyr::pivot_wider(names_from = Measurement, values_from = Value)

ski_area_quants %>%
  readr::write_rds("./data/data-derived/ski_areas_quants.rds",
                   compress = "bz")

library(magrittr)

ski_area_quants <-
  readr::read_rds("./data/data-derived/ski_areas_quants.rds")

df <- ski_area_quants %>%
  dplyr::ungroup() %>%
  dplyr::left_join(ski_areas_rockies %>%
                     dplyr::select(Name,State) %>%
                     sf::st_drop_geometry()
  ) %>%
  dplyr::filter(Month %in% c(10:12,1:4)) %>%
  dplyr::mutate(State = factor(State,
                               levels = c("Montana",
                                          "Arizona",
                                          "Colorado",
                                          "Idaho",
                                          "New Mexico",
                                          "Utah",
                                          "Wyoming"),
                               ordered = TRUE)) %>%
  dplyr::arrange(-State) %>%
  dplyr::mutate(Month = month.name[Month] %>%
                  factor(levels = month.name[c(10:12,1:4)],
                         ordered = TRUE)) %>%
  dplyr::arrange(State, Name, Year, Month)

library(grid)
library(ggplot2)
library(gganimate)
library(transformr)


g <-
  ggplot(df,
         aes(x = Year,
             y = `Minimum Temperature`,
             color = State,
             group = Name)) +
  # annotation_custom(rasterGrob("#e9521620",
  #                              width=unit(1,"npc"),
  #                              height = unit(1,"npc")),
  #                   xmin=-Inf, xmax=Inf, ymin=28, ymax=Inf) +
  # annotation_custom(rasterGrob("#0043fa20",
  #                              width=unit(1,"npc"),
  #                              height = unit(1,"npc")),
  #                   xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=28) +
  geom_smooth(se = FALSE) +
  geom_smooth(data = df %>%
                dplyr::filter(State == "Montana"),
              size = 2,
              se = FALSE) +
  scale_color_manual(values = "black" %>%
                       c(RColorBrewer::brewer.pal(6,"Set2")) %>%
                       magrittr::set_names(c("Montana",
                                             "Arizona",
                                             "Colorado",
                                             "Idaho",
                                             "New Mexico",
                                             "Utah",
                                             "Wyoming"))
  ) +
  geom_hline(yintercept = 28,
             linetype = 2,
             size = 1.5,
             color = "gray50") +
  ggthemes::theme_few(18) +
  # Here comes the gganimate specific bits
  labs(title = 'Month: {closest_state}') +
  transition_states(Month, transition_length = 1, state_length = 1) +
  # transition_time(Month) +
  ease_aes('cubic-in-out')

anim_save("test.gif",
          animation = g,
          width = 10.24,
          height = 7.68,
          device = "svg",
          rewind = TRUE,
          end_pause = 10,
          fps = 50,
          nframes = 700)

ggsave("test.svg",
       plot = g,
       device = 'svg',
       width = 10.24,
       height = 7.68)





