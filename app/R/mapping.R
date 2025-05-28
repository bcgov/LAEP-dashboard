
# helper functions ----

## transform to log10 if the range of values is very large
## note: for the legend value, will need to undo transform - use rev = TRUE for this
stat_transform <- function(x, rev = FALSE) {

  if(!all(is.na(x))) {
    range <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)

    if(rev) {
      ## if the max of x is between 2 and 5 - assume values have been log transformed
      ## if max is less than 1, then values are percentages
      ## if max is greater than 5, the values are not transformed
      if(between(max(x, na.rm = TRUE), 2, 5)) val <- 10^x
      else val <- x

    } else {
      ## if the range of x is bigger than 10^5 then log transform
      if(range > 10^5) val <- log10(x)
      else val <- x
    }

    return(val)

  } else { NA } ## if all values are NA, return NA
}

## add a legend to the map
## handles several different cases:
##  only 1 row of data - don't add legend
##  map title provided in MAP_TITLE or VARIABLE columns
##  log transformed data or no transformation
##  numeric or categorical
add_legend <- function(map, df, pal) {

  ## only add legend if more than one row of data
  if(nrow(df) > 1) {

    ## determine map title
    if("MAP_TITLE" %in% names(df)) {
      title <- unique(df$MAP_TITLE)
      lab_formatting <- label_percent(accuracy = 1)

    } else {
      title = unique(df$VARIABLE)
      lab_formatting <- label_comma()
    }

    title <- HTML("<div style='text-align:left; width:135px; line-height:1.125rem;'>",
                  title, "</div>")

    ## numeric legend
    if(is.numeric(df$VALUE)) {

      map <- map |>
        addLegend(data = df,
                  pal = pal,
                  title = title,
                  values = ~na.omit(df$VALUE),
                  ## use custom formatting function to handle log transforms
                  labFormat = function(type, cuts, p) { lab_formatting(stat_transform(cuts, rev = TRUE))},
                  # need to include type, cuts, p in function -
                  # type: "numeric", "bin", or "quantile"
                  # cuts: vector of bin breaks -- this is what is being formatted by this function
                  # p: palette values (optional)
                  bins = 5)

      ## categorical legend
      } else {
        legend_values <- df |> distinct(VALUE, COLOR)
        legend_labels <- legend_values$VALUE
        legend_colors <- legend_values$COLOR

        map <- map |>
          addLegend(data = df,
                    title = title,
                    labels = lapply(paste("<span style='display: inline-block; width: 120px;'>", legend_labels, "</span>"), HTML),
                    colors = legend_colors)
      }
    }

  map
}

## add the colored layer to the map
add_colored_layer <- function(map, df, pal) {

  if(is.numeric(df$VALUE)) {
    df <- df |> mutate(fill = pal(stat_transform(VALUE)))
  } else {
    df <- df |> mutate(fill = COLOR)
  }

  map |>
    addPolygons(data = df,
                layerId = ~REGION_NAME,
                label = ~lapply(MAP_LABEL, HTML),
                stroke = FALSE,
                fillColor = ~fill,
                fillOpacity = 0.5,
                highlightOptions = highlightOptions(
                  fillOpacity = 0.9,
                  bringToFront = TRUE))
}


## mapping function
make_map <- function(region_type, rd_region, la_region, stat_data) {

  ## set base colour palette
  pal <- colorNumeric("YlGn", domain = NULL, na.color = "white")
  light_border<- "#9F9D9C"
  dark_border <- "#252423"


  ## base map
  m <- leaflet(options = leafletOptions(zoomSnap = 0.2, zoomDelta = 1, attributionControl = F)) |>
    addProviderTiles("CartoDB.Voyager") |>
    ## there doesn't seem to be a way to move the leafet attribution so remove and add new one in same style as original
    addControl(
      html = HTML('<div class="leaflet-control-attribution">
                      Leaflet | &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>
                      contributors &copy; <a href="https://carto.com/">CARTO</a>
                    </div>'),
      position = "bottomleft")

  ## add layers:
  ## to make the map navigation work as desired, there are 4 map layers
  ## background_map - null or light outline, no fill, has region name labels, is clickable
  ## outline_map - light or dark outline (depending if background map is used), no fill, no labels, is not clickable
  ## centered_map - zoom reference only, is not mapped
  ## colored_map - no outline, fill based on data, has data labels, is clickable

  if (region_type == "RD") {

    if (!str_detect(rd_region, "All") & str_detect(la_region, "All")) {
      background_map <- map_rds |> filter(REGION_NAME != rd_region)
      outline_map <- map_rds |> filter(REGION_NAME == rd_region) |> mutate(color = dark_border) |>
        bind_rows(map_las |>
                    filter(REGION_NAME %in% (rd_la_lookup |> filter(RD == rd_region) |> pull(LA))) |>
                    mutate(color = light_border))
      centered_map <- map_rds |> filter(REGION_NAME == rd_region)
      colored_map <- map_las |>
        filter(REGION_NAME %in% (rd_la_lookup |> filter(RD == rd_region) |> pull(LA))) |>
        left_join(stat_data, by = "REGION_NAME")

    } else if (!str_detect(rd_region, "All") & !str_detect(la_region, "All")) {
      background_map <- map_rds |> st_difference(map_las |> filter(REGION_NAME == la_region))
      outline_map <- map_las |> filter(REGION_NAME == la_region) |> mutate(color = dark_border)
      centered_map <- map_las |> filter(REGION_NAME == la_region)
      colored_map <- map_las |> filter(REGION_NAME == la_region)|>
        left_join(stat_data, by = "REGION_NAME")

    } else {
      background_map <- NULL
      outline_map <- map_rds |> mutate(color = light_border)
      centered_map <- map_rds
      colored_map <- map_rds_clipped |>
        left_join(stat_data, by = "REGION_NAME")
    }

  } else {  # LA

    if (!str_detect(rd_region, "All") & str_detect(la_region, "All")) {
      background_map <- map_las |>
        st_difference(map_las |> filter(REGION_NAME %in% (rd_la_lookup |> filter(RD == rd_region) |> pull(LA))))
      outline_map <- map_rds |> filter(REGION_NAME == rd_region) |> mutate(color = dark_border) |>
        bind_rows(map_las |>
                    filter(REGION_NAME %in% (rd_la_lookup |> filter(RD == rd_region) |> pull(LA))) |>
                    mutate(color = light_border))
      centered_map <- map_rds |> filter(REGION_NAME == rd_region)
      colored_map <- map_las |>
        filter(REGION_NAME %in% (rd_la_lookup |> filter(RD == rd_region) |> pull(LA))) |>
        left_join(stat_data, by = "REGION_NAME")

    } else if (!str_detect(rd_region, "All") & !str_detect(la_region, "All")) {
      background_map <- map_las |> st_difference(map_las |> filter(REGION_NAME == la_region))
      outline_map <- map_las |> filter(REGION_NAME == la_region) |> mutate(color = dark_border)
      centered_map <- map_las |> filter(REGION_NAME == la_region)
      colored_map <- map_las |> filter(REGION_NAME == la_region) |>
        left_join(stat_data, by = "REGION_NAME")

    } else {
      background_map <- NULL
      outline_map <- map_las |> mutate(color = light_border)
      centered_map <- map_las
      colored_map <- map_las |>
        left_join(stat_data, by = "REGION_NAME")
    }
  }

  bbox <- st_bbox(centered_map)

  if(!is.null(background_map)) { ## add background map if it is not null
    m <- m |>
      addPolygons(data = background_map,
                  layerId = ~REGION_NAME,
                  label = ~REGION_NAME,
                  weight = 1, color = light_border, fillOpacity = 0,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = light_border,
                    bringToFront = TRUE))
    }

  ## add remaining layers
  m <- m |>
    addPolygons(data = outline_map, weight = 2, color = ~color, fillOpacity = 0,
                options = pathOptions(clickable = FALSE)) |>
    add_colored_layer(colored_map, pal) |>
    fitBounds(lng1 = bbox$xmin[[1]],
              lat1 = bbox$ymin[[1]],
              lng2 = bbox$xmax[[1]],
              lat2 = bbox$ymax[[1]]) |>
    add_legend(colored_map, pal)
}
