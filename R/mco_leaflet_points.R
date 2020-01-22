mco_leaflet_points <-
  function(x,
         col,
         pal =  "RdBu",
         legend_title = "",
         # image_query_title = "",
         midpoint = 0,
         digits = 1,
         reverse = FALSE,
         attribution = "Climate data by <a href='http://www.climatologylab.org/gridmet.html' target='_blank'>gridMET</a> via <a href='https://earthengine.google.com/' target='_blank'>Google Earth Engine</a>",
         ...){

    # if(reverse)
    #   pal <- paste0("-",pal)

  tm_out <- (x %>%
               tmap::tm_shape() +
               tmap::tm_symbols(col = col,
                                shape = 19,
                                pal =  pal,
                                style= "cont",
                                alpha = 1,
                                midpoint = midpoint,
                                legend.col.reverse = reverse,
                                ...) +
               tmap::tm_layout(title = legend_title) +
               tmap::tm_view(view.legend.position = c("left","bottom"))) %>%
    tmap::tmap_leaflet()

  if(reverse){
    tm_out$x$calls[[6]]$args[[1]]$labels %<>% rev()
  }

  tm_out$x$calls[[6]]$args[[1]]$title <- legend_title

  mv_out <- mapview::mapview(x)@map

  mv_out$x$calls[[7]]$args[[6]]$color <- tm_out$x$calls[[5]]$args[[6]]$fillColor
  mv_out$x$calls[[7]]$args[[6]]$fillColor <- tm_out$x$calls[[5]]$args[[6]]$fillColor
  mv_out$x$calls[[7]]$args[[6]]$opacity <- 1
  mv_out$x$calls[[7]]$args[[6]]$pane <- "foreground"
  mv_out$x$calls[[11]] <- tm_out$x$calls[[6]]




  # mv_out$x$calls <- mv_out$x$calls[c(-1:-5,-8:-10)]


  # tm_out$x$calls[[4]]$args[[4]]$pane <- "background"
  # tm_out$x$calls[[4]]$args[[4]]$attribution <- ""
  # tm_out$x$calls[[4]]$args[[6]] <- ""
  # out$x$calls[[length(out$x$calls)]]$args[[4]]$pane <- "foreground"


  out <- mcor::mco_leaflet_base(attribution = attribution)

  # out <- out %>%
  #   leaflet::addCircleMarkers(data = x,
  #                             options = leaflet::pathOptions(pane = "foreground",
  #                                                            attribution = ""))

  out$x$calls <- c(out$x$calls,mv_out$x$calls[c(7,11)])
  out$x$limits <- mv_out$x$limits
  out$x$options <- c(mv_out$x$options,out$x$options['fullscreenControl'])

  out$title <- legend_title

  # tm_out$jsHooks$render[[1]]$code %<>%
  #   stringr::str_replace("document.getElementsByClassName","el.getElementsByClassName")

  # out$jsHooks$render <- c(out$jsHooks$render,mv_out$jsHooks$render)

  out$dependencies <- c(out$dependencies,mv_out$dependencies[4])

  out$jsHooks$render %<>%
    purrr::map(function(x){
      x$code %<>%
        stringr::str_remove_all("\\t") %>%
        stringr::str_remove_all("\\n")

      return(x)
    })

  # stars <- out$dependencies %>%
  #   purrr::keep(~ .x$name == "stars") %>%
  #   magrittr::extract2(1) %$%
  #   paste0(src$file,"/",script[[1]]) %>%
  #   readr::read_file() %>%
  #   htmltools::tags$script()
  #
  # out$dependencies %<>%
  #   purrr::discard(~ .x$name == "stars")
  #
  # # out$jsHooks$render <- c(out$jsHooks$render,
  # #                         list(list(code = stars,
  # #                              data = NULL)))
  #
  # out %<>%
  #   htmlwidgets::appendContent(stars) #%>%
  # # leaflet.opacity::addOpacitySlider(layerId = image_query_title)

  out

}
