library(tidyverse)
library(sf)
library(leaflet)
library(bcmaps)
library(mapview)

#source("app_testing/R/functions.R")

FILE_EDA <- "EDA_union.shp"

## load LA (EDA) mapping info (created by R_scripts/merging_geometries.R)
map_las <- read_sf(FILE_EDA) |>
  transmute(REGION_NAME = clean_regions(EDA_name))|>
  mutate(REGION_NAME = ifelse(str_detect(REGION_NAME, "Chilcotin"), "Chilcotin", REGION_NAME)) |>
  group_by(REGION_NAME) |>
  summarize(geometry = st_union(geometry)) |>
  st_transform(3005) |> ## transform to meters to simplify
  st_simplify(dTolerance = 100, preserveTopology = TRUE) |>
  st_transform(4326) ## transform to LAT/LONG for mapping

#LAs <- map_las |> as.data.frame() |> distinct(REGION_NAME) |> pull() |> sort()

## load RD mapping info (from bc data catalogue using the bcmaps package)
map_rds <- bcmaps::regional_districts() |>
  transmute(REGION_NAME = clean_regions(ADMIN_AREA_NAME))|>
  mutate(REGION_NAME = ifelse(REGION_NAME %in% map_las$REGION_NAME, paste(REGION_NAME, "RD"), REGION_NAME)) |>
  ## simplify the boundaries, dTolerance in meters
  st_simplify(dTolerance = 100, preserveTopology = TRUE) |>
  st_transform(4326) |> ## transform to LAT/LONG for mapping
  bind_rows(map_las |> filter(REGION_NAME == "Northern Rockies") |> mutate(REGION_NAME = "Northern Rockies RD"))

#RDs <- map_rds |> as.data.frame() |> distinct(REGION_NAME) |> pull() |> sort()

## load BC mapping info (from bc data catalogue using the bcmaps package)
map_bc <- bcmaps::bc_bound() |>
  ## simplify the boundaries, dTolerance in meters
  st_simplify(dTolerance = 100, preserveTopology = TRUE) |>
  st_transform(4326) |> ## transform to LAT/LONG for mapping
  st_union() |>
  st_as_sf() |>
  transmute(REGION_NAME = "British Columbia")



## mapping function
make_map <- function(region_type, rd_region, la_region, stat_data, stat) {

  ## set base colour palette
  # pal <- colorQuantile(n=9,
  #   palette = "Greens",
  #   domain = stat_data$STATISTIC
  # )
  pal <- colorNumeric(palette = "YlGn", NULL)

  stat_transform <- function(x, rev = FALSE) {

    if(!all(is.na(x))) {

    range <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)

    if(rev) {

      if(max(x, na.rm = TRUE) > 6)   val <- x
      else val <- 10^x

    } else {

      if(range > 10^5) val <- log10(x)
      else val <- x
    }

    return(val)
    } else { NA }
    }

  legend <- function(map, data, x) {

    if(length(x) > 1) {
      lab_formatting <- regional_profile_info |> filter(col_formatted == stat) |> pull(label) |> pluck(1)

      map <- map |>
        addLegend(data = data,
                  pal = pal,
                  title = HTML("<div style='text-align:left; width: 120px;'>", stat, "</div>"), ## wrap in HTML to set width
                  values = ~na.omit(stat_transform(x)),
                  labFormat = function(type, cuts, p) { lab_formatting(stat_transform(cuts, rev = TRUE))},
                  # need to include type, cuts, p in function -
                  # type: "numeric", "bin", or "quantile"
                  # cuts: vector of bin breaks -- this is what is being formatted by this function
                  # p: palette values (optional)
                  bins = 5
        )
    }

    map
  }

  add_coloured_layer <- function(map, data) {

    map |>
      addPolygons(data = data,
                  layerId = ~REGION_NAME,
                  label = ~lapply(paste0("<strong>",REGION_NAME,"</strong><br>", stat, ": ", comma(STATISTIC)), HTML),
                  weight = 2, color = "#9F9D9C",
                  fillColor = ~pal(stat_transform(STATISTIC)), fillOpacity = 0.5,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    bringToFront = TRUE))

  }

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
      position = "bottomleft"
    )

  if(region_type == "RD") {

    if(!str_detect(rd_region, "All") & str_detect(la_region, "All")) {

      background_map <- map_rds
      centered_map <- map_rds |> filter(REGION_NAME == rd_region)
      coloured_map <- map_las |> filter(REGION_NAME %in% (rd_la_lookup |> filter(RD == rd_region) |> pull(LA))) |>
        left_join(stat_data, by = "REGION_NAME")

      bbox <- st_bbox(centered_map)

      m <- m |>
        addPolygons(data = background_map,
                    layerId = ~REGION_NAME,
                    label = ~REGION_NAME,
                    weight = 2, color = "#9F9D9C", fillColor = "#9df99f", fillOpacity = 0,
                    highlightOptions = highlightOptions(
                      weight = 5,
                      bringToFront = TRUE)) |>
        addPolygons(data = centered_map, weight = 2, color = "#353433", fillOpacity = 0) |>
        add_coloured_layer(data = coloured_map) |>
        fitBounds(lng1 = bbox$xmin[[1]],
                  lat1 = bbox$ymin[[1]],
                  lng2 = bbox$xmax[[1]],
                  lat2 = bbox$ymax[[1]])|>
        legend(data = coloured_map, x = coloured_map$STATISTIC)

    } else if(!str_detect(rd_region, "All") & !str_detect(la_region, "All")) {

      background_map <- map_rds
      outline_map <- map_rds |> filter(REGION_NAME == rd_region)
      coloured_map <- map_las |> filter(REGION_NAME %in% (rd_la_lookup |> filter(RD == rd_region) |> pull(LA)))|>
        left_join(stat_data, by = "REGION_NAME")
      centered_map <- map_las |> filter(REGION_NAME == la_region)

      bbox <- st_bbox(centered_map)

      m <- m |>
        addPolygons(data = background_map,
                    layerId = ~REGION_NAME,
                    label = ~REGION_NAME,
                    weight = 2, color = "#9F9D9C", fillOpacity = 0,
                    highlightOptions = highlightOptions(
                      weight = 5,
                      color = "#9F9D9C",
                      bringToFront = TRUE)) |>
        addPolygons(data = outline_map, weight = 2, color = "#353433", fillOpacity = 0) |>
        add_coloured_layer(data = coloured_map) |>
        fitBounds(lng1 = bbox$xmin[[1]],
                  lat1 = bbox$ymin[[1]],
                  lng2 = bbox$xmax[[1]],
                  lat2 = bbox$ymax[[1]])|>
        legend(data = coloured_map, x = coloured_map$STATISTIC)

    } else {

      coloured_map <- map_rds|>
        left_join(stat_data, by = "REGION_NAME")

      m <- m |>
        add_coloured_layer(data = coloured_map) |>
        legend(data = coloured_map, x = coloured_map$STATISTIC)

    }

  }

  if(region_type == "LA") {

    if(!str_detect(rd_region, "All") & str_detect(la_region, "All")) {

      background_map <- map_las
      centered_map <- map_rds |> filter(REGION_NAME == rd_region)
      coloured_map <- map_las |> filter(REGION_NAME %in% (rd_la_lookup |> filter(RD == rd_region) |> pull(LA)))|>
        left_join(stat_data, by = "REGION_NAME")

      bbox <- st_bbox(centered_map)

      m <- m |>
        addPolygons(data = background_map,
                    layerId = ~REGION_NAME,
                    label = ~REGION_NAME,
                    weight = 2, color = "#9F9D9C", fillColor = "#9df99f", fillOpacity = 0,
                    highlightOptions = highlightOptions(
                      weight = 5,
                      bringToFront = TRUE)) |>
        addPolygons(data = centered_map, weight = 2, color = "#353433", fillOpacity = 0) |>
        add_coloured_layer(data = coloured_map) |>
        fitBounds(lng1 = bbox$xmin[[1]],
                  lat1 = bbox$ymin[[1]],
                  lng2 = bbox$xmax[[1]],
                  lat2 = bbox$ymax[[1]])|>
        legend(data = coloured_map, x = coloured_map$STATISTIC)

    } else if(!str_detect(rd_region, "All") & !str_detect(la_region, "All")){

      background_map <- map_las
      coloured_map <- map_las |> filter(REGION_NAME == la_region)|>
        left_join(stat_data, by = "REGION_NAME")

      bbox <- st_bbox(coloured_map)

      m <- m |>
        addPolygons(data = background_map,
                    layerId = ~REGION_NAME,
                    label = ~REGION_NAME,
                    weight = 2, color = "#9F9D9C", fillOpacity = 0,
                    highlightOptions = highlightOptions(
                      weight = 5,
                      color = "#9F9D9C",
                      bringToFront = TRUE)) |>
        add_coloured_layer(data = coloured_map) |>
        fitBounds(lng1 = bbox$xmin[[1]],
                  lat1 = bbox$ymin[[1]],
                  lng2 = bbox$xmax[[1]],
                  lat2 = bbox$ymax[[1]])|>
        legend(data = coloured_map, x = coloured_map$STATISTIC)

    } else {
      coloured_map <- map_las|>
        left_join(stat_data, by = "REGION_NAME")

      m <- m |>
        add_coloured_layer(data = coloured_map) |>
        legend(data = coloured_map, x = coloured_map$STATISTIC)
    }

  }

  m
}
