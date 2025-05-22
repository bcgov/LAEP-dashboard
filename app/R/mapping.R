
## helper functions
stat_transform <- function(x, rev = FALSE) {

  if(!all(is.na(x))) {

    range <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)

    if(rev) {

      if(between(max(x, na.rm = TRUE), 2, 5)) val <- 10^x
      else val <- x

    } else {

      if(range > 10^5) val <- log10(x)
      else val <- x
    }
    return(val)
  } else { NA }
}

add_legend <- function(map, df, pal) {

  if(nrow(df) > 1) {

    if(is.numeric(df$VALUE)) {
      if("MAP_TITLE" %in% names(df)) {
        title <- unique(df$MAP_TITLE)
        lab_formatting <- label_percent(accuracy = 1)

      } else {
        title = unique(df$VARIABLE)
        lab_formatting <- label_comma()
      }

      map <- map |>
        addLegend(data = df,
                  pal = pal,
                  title = HTML("<div style='text-align:left; width: 120px;'>", title, "</div>"), ## wrap in HTML to set width
                  values = ~na.omit(df$VALUE),
                  labFormat = function(type, cuts, p) { lab_formatting(stat_transform(cuts, rev = TRUE))},
                  # need to include type, cuts, p in function -
                  # type: "numeric", "bin", or "quantile"
                  # cuts: vector of bin breaks -- this is what is being formatted by this function
                  # p: palette values (optional)
                  bins = 5
        )

    } else {

      title <- "Dominant income source"
      legend_values <- df |> distinct(VALUE, COLOR)
      legend_labels <- legend_values$VALUE
      legend_colors <- legend_values$COLOR

      map <- map |>
        addLegend(data = df,
                  title = HTML("<div style='text-align:left; width: 120px;'>", title, "</div>"), ## wrap in HTML to set width
                  colors = legend_colors,
                  labels = lapply(paste("<span style='display: inline-block; width: 120px;'>", legend_labels, "</span>"), HTML)
                  )
    }


  }

  map
}

add_colored_layer <- function(map, df, pal) {

  if(is.numeric(df$VALUE)) {
    map |>
      addPolygons(data = df,
                  layerId = ~REGION_NAME,
                  label = ~lapply(MAP_LABEL, HTML),
                  #  weight = 0.75, color = "#9F9D9C",
                  stroke = FALSE,
                  fillColor = ~pal(stat_transform(VALUE)), fillOpacity = 0.5,
                  highlightOptions = highlightOptions(
                    fillOpacity = 0.9,
                    # weight = 5,
                    # color = "#9F9D9C",
                    bringToFront = TRUE))
  } else {
    map |>
      addPolygons(data = df,
                  layerId = ~REGION_NAME,
                  label = ~lapply(MAP_LABEL, HTML),
                  stroke = FALSE,
                  fillColor = ~COLOR, fillOpacity = 0.5,
                  highlightOptions = highlightOptions(
                    fillOpacity = 0.9,
                    # weight = 5,
                    # color = "#9F9D9C",
                    bringToFront = TRUE))

  }



}



## mapping function
make_map <- function(region_type, rd_region, la_region, stat_data) {

  ## set base colour palette
  pal <- colorNumeric("YlGn", domain = NULL)
  light_border<- "#9F9D9C"
  dark_border <- "#252423" #"#353433"


  ## base map
  m <- leaflet(options = leafletOptions(zoomSnap = 0.2, zoomDelta = 1, attributionControl = F)) |>
    addProviderTiles("CartoDB.Voyager") |>
    addPolygons(data = map_bc, layerId = ~REGION_NAME,label = ~REGION_NAME, weight = 0, color = "transparent", fillOpacity = 0) |>
    ## there doesn't seem to be a way to move the leafet attribution so remove and add new one in same style as original
    addControl(
      html = HTML('<div class="leaflet-control-attribution">
                      Leaflet | &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>
                      contributors &copy; <a href="https://carto.com/">CARTO</a>
                    </div>'),
      position = "bottomleft"  )

  if (region_type == "RD") {
    print("regiontype=RD")
    if (!str_detect(rd_region, "All") & str_detect(la_region, "All")) {
      print("region !all, la =all")

      background_map <- map_rds
      outline_map <- map_rds |> filter(REGION_NAME == rd_region) |> mutate(color = dark_border) |>
        bind_rows(map_las |>
                    filter(REGION_NAME %in% (rd_la_lookup |> filter(RD == rd_region) |> pull(LA))) |>
                    mutate(color = light_border)
        )
      centered_map <- map_rds |> filter(REGION_NAME == rd_region)
      colored_map <- map_las |>
        filter(REGION_NAME %in% (rd_la_lookup |> filter(RD == rd_region) |> pull(LA))) |>
        left_join(stat_data, by = "REGION_NAME")

    } else if (!str_detect(rd_region, "All") & !str_detect(la_region, "All")) {
      print("region !all, la !all")

      background_map <- map_rds |> st_difference(map_las |> filter(REGION_NAME == la_region))
      outline_map <- map_las |> filter(REGION_NAME == la_region) |> mutate(color = dark_border)
      centered_map <- map_las |> filter(REGION_NAME == la_region)
      colored_map <- map_las |> filter(REGION_NAME == la_region)|>
        left_join(stat_data, by = "REGION_NAME")

    } else {
      print("region = All")
      background_map <- NULL
      outline_map <- map_rds |> mutate(color = light_border)
      centered_map <- map_rds
      colored_map <- map_rds_clipped |>
        left_join(stat_data, by = "REGION_NAME")
    }

  } else {  # LA
    print("regiontype = LA")
    print(rd_region)
    print(la_region)
    if (!str_detect(rd_region, "All") & str_detect(la_region, "All")) {
      print("LA: region !all, la =all")

      background_map <- map_las |> st_difference(map_las |>
                                                   filter(REGION_NAME %in% (rd_la_lookup |> filter(RD == rd_region) |> pull(LA))))
      outline_map <- map_rds |> filter(REGION_NAME == rd_region) |> mutate(color = dark_border) |>
        bind_rows(map_las |>
                    filter(REGION_NAME %in% (rd_la_lookup |> filter(RD == rd_region) |> pull(LA))) |>
                    mutate(color = light_border)
        )
      centered_map <- map_rds |> filter(REGION_NAME == rd_region)
      colored_map <- map_las |>
        filter(REGION_NAME %in% (rd_la_lookup |> filter(RD == rd_region) |> pull(LA))) |>
        left_join(stat_data, by = "REGION_NAME")

    } else if (!str_detect(rd_region, "All") & !str_detect(la_region, "All")) {
      print("LA: region !all, la !all")
      background_map <- map_las |> st_difference(map_las |> filter(REGION_NAME == la_region))
      outline_map <- map_las |> filter(REGION_NAME == la_region) |> mutate(color = dark_border)
      centered_map <- map_las |> filter(REGION_NAME == la_region)
      colored_map <- map_las |> filter(REGION_NAME == la_region) |>
        left_join(stat_data, by = "REGION_NAME")

    } else {
      print("LA: region = All")
      background_map <- NULL
      outline_map <- map_las |> mutate(color = light_border)
      centered_map <- map_las
      colored_map <- map_las |>
        left_join(stat_data, by = "REGION_NAME")
    }
  }

  bbox <- st_bbox(centered_map)

  print("pre-mapping")
  if(!is.null(background_map)) {
    print("backgroundmap")
    m <- m |>
      addPolygons(data = background_map,
                  layerId = ~REGION_NAME,
                  label = ~REGION_NAME,
                  weight = 1, color = "#9F9D9C", fillOpacity = 0,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#9F9D9C",
                    bringToFront = TRUE)
      )
  }

  m <- m |>
    addPolygons(data = outline_map, weight = 2, color = ~color, fillOpacity = 0) |>
    add_colored_layer(colored_map, pal) |>
    fitBounds(lng1 = bbox$xmin[[1]],
              lat1 = bbox$ymin[[1]],
              lng2 = bbox$xmax[[1]],
              lat2 = bbox$ymax[[1]]) |>
    add_legend(colored_map, pal)

  print("mapped")
  m

}
