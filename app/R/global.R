# Copyright 2023 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


# PARAMETERS ----
# I'm working on this project daily ATM so we'll leave it like this for now
last_updated = format(ymd("2025-03-07"), "%b %d, %Y")

# Do you want to load and process data from the Excel file (T) or load data from the .Rds file? The latter is faster but obviously requires generating the file and saving it beforehand
load_data = F

# Excel toolkit file path
xl_path = path_wd('app/Local Area Economic Profiles 2024 Toolkit V3.xlsx')

# LA shape file path
FILE_EDA <- "EDA_union.shp"

## Color code for industries
industry_colors = c(
  "Agriculture and food" = "#CCEBC5",
  "Construction" = "#D55E00",
  "Finance, insurance and real estate" = "#B3DE69",
  "Fishing hunting and trapping" = "#FF0000",
  "Forestry" = "#4DAF4A",
  "Information and communications technology" = "#AA4499",
  "Mining" = "#661100",
  "Oil and gas" = "#999999",
  "Other services" = "#D9D9D9",
  "Rail transport" = "#8DD3C7",
  "Retail trade" = "#80B1D3",
  "Tourism" = "#377EB8",
  "Water transport" = "#6699CC",
  "Government transfers" = "#44AA99",
  "Non-employment market income" = "#984EA3",
  "Public sector" = "#DDCC77"
)

is_local = Sys.getenv('SHINY_PORT') == ""

# GET DATA ----
## If load_data = T:
# Reads in the data from the Excel file and processes it
# Creates RD to LA lookup table
# Creates the spatial files from FILE_EDA and bcmaps
## If load_data = F:
# Read in already created data
if (load_data) {

  sheets <- c('Descriptive Stats', 'Jobs', 'Income Dependencies')
  start_row <- c(3, 5, 5)

  data_orig = map2(sheets, start_row, function(sheet, skip) readxl::read_excel(xl_path, sheet, skip = skip) |> janitor::clean_names('screaming_snake'))

  ## clean region names across all sheets
  data_orig <- map(data_orig, ~mutate(.x, across(c(REGION_NAME, PARENT_RD), clean_regions)))

  ## add "RD" suffix to regional districts with the same name as a local area
  LA_list <- data_orig[[1]] |> filter(GEO_TYPE == "EDA") |> distinct(REGION_NAME) |> pull()
  data_orig <- map(data_orig, ~mutate(.x, REGION_NAME = ifelse(GEO_TYPE == "RD" & REGION_NAME %in% LA_list, paste(REGION_NAME, "RD"), REGION_NAME)))
  data_orig <- map(data_orig, ~mutate(.x, PARENT_RD = ifelse(PARENT_RD %in% LA_list, paste(PARENT_RD, "RD"), PARENT_RD)))

  data <- list()

  ## Format Descriptive Stats
  data[["Descriptive Stats"]] <- data_orig[[1]] |>
    select(KEY, REGION_NAME, REF_YEAR, GEO_TYPE, PARENT_RD, POPULATION, TOTAL_JOBS,
           AVERAGE_EMPLOYMENT_INCOME, TOTAL_INCOME = TOTAL_INCOME_M, DIVERSITY_INDEX) |>
    mutate(across(POPULATION:DIVERSITY_INDEX, ~str_remove_all(.x, "^(-|F)$"))) |> ## remove missing data identifiers
    mutate(across(c(REF_YEAR, POPULATION, TOTAL_JOBS), as.integer)) |>
    mutate(across(AVERAGE_EMPLOYMENT_INCOME:DIVERSITY_INDEX, as.double)) |>
    pivot_longer(-c(KEY, REGION_NAME, REF_YEAR, GEO_TYPE, PARENT_RD),
                 names_to = "VARIABLE",
                 names_transform = ~snakecase::to_any_case(.x, "sentence"),
                 values_to = "VALUE") |>
    mutate(VARIABLE = ifelse(VARIABLE == "Total income", "Total income (millions)", VARIABLE)) |>
    mutate(
      FORMATTED_VALUE = case_when(
        VARIABLE == "Population" ~ label_comma(accuracy = 1)(VALUE),
        VARIABLE == "Total jobs" ~ label_comma(accuracy = 1)(VALUE),
        VARIABLE == "Average employment income" ~ label_dollar(suffix = "/yr")(VALUE),
        VARIABLE == "Total income (millions)" ~ label_dollar()(VALUE),
        VARIABLE == "Diversity index" ~ label_comma(accuracy = 0.1)(VALUE)),
      ICON = case_when(
        VARIABLE == "Population" ~ "earth-americas",
        VARIABLE == "Total jobs" ~ "user-tie",
        VARIABLE == "Average employment income" ~ "money-bills",
        VARIABLE == "Total income (millions)" ~ "hand-holding-dollar",
        VARIABLE == "Diversity index" ~ "chart-pie"))

  ## Add map labels
  map_labels <- bind_rows(
    data[["Descriptive Stats"]] |>
      filter(VARIABLE != "Diversity index") |>
      group_by(REGION_NAME, REF_YEAR) |>
      mutate(MAP_LABEL = paste0("<strong>", REGION_NAME, "</strong><br>", paste(paste(VARIABLE, ": ", FORMATTED_VALUE), collapse = "<br>"))) |>
      ungroup() |>
      filter(VARIABLE == "Population") |>
      select(REGION_NAME, REF_YEAR, VARIABLE, MAP_LABEL),
    data[["Descriptive Stats"]] |>
      filter(VARIABLE == "Diversity index") |>
      group_by(REGION_NAME, REF_YEAR) |>
      mutate(MAP_LABEL = paste0("<strong>",REGION_NAME,"</strong><br>", VARIABLE, ": ", FORMATTED_VALUE)) |>
      select(REGION_NAME, REF_YEAR, VARIABLE, MAP_LABEL)
  )

  data[["Descriptive Stats"]] <- data[["Descriptive Stats"]] |>
    left_join(map_labels, by = c("REGION_NAME", "REF_YEAR", "VARIABLE"))

  ## Format dominant income source data
  data[["Dominant Income Sources"]] <- data_orig[[1]] |>
    select(KEY, REGION_NAME, REF_YEAR, GEO_TYPE, PARENT_RD,
           DOMINANT_BASIC_INCOME_SOURCE, DOMINANT_PRIVATE_SECTOR_EMPLOYMENT_BASIC_INCOME_SOURCE) |>
    mutate(across(DOMINANT_BASIC_INCOME_SOURCE:DOMINANT_PRIVATE_SECTOR_EMPLOYMENT_BASIC_INCOME_SOURCE, ~str_remove_all(.x, "^(-|F)$"))) |> ## remove missing data identifiers
    mutate(across(c(REF_YEAR), as.integer)) |>
    pivot_longer(-c(KEY, REGION_NAME, REF_YEAR, GEO_TYPE, PARENT_RD),
                 names_to = "VARIABLE",
                 names_transform = ~snakecase::to_any_case(.x, "sentence"),
                 values_to = "VALUE") |>
    mutate(VALUE = case_when(VALUE == "FIRE" ~ "Finance, insurance and real estate",
                             VALUE == "ICT" ~ "Information and communications technology",
                             VALUE == "Non-employment market income" ~ VALUE, ## sentence case will remove the "-", so make special case
                             TRUE ~ snakecase::to_any_case(VALUE, "sentence")),
           FORMATTED_VALUE = ifelse(VALUE == "", NA, VALUE),
           COLOR = as.character(industry_colors[VALUE]),
           MAP_LABEL = paste0("<strong>",REGION_NAME,"</strong><br>", FORMATTED_VALUE))

  ## Format Jobs - Top 5 jobs per region
  data[["Jobs"]] <- data_orig[[2]] |>
    rename_all(str_remove_all, "_TOTAL") |>
    pivot_longer(-c(KEY, REGION_NAME, REF_YEAR, STATISTIC, GEO_TYPE, PARENT_RD),
                 names_to = "VARIABLE",
                 names_transform = ~snakecase::to_any_case(.x, case = "sentence"),
                 values_to = "VALUE") |>
    filter(!VARIABLE %in% c("Total", "Forestry", "Agriculture", "Tourism transportation",
                            "Tourism accommodation", "Tourism food and beverage", "Tourism recreation and entertainment",
                            "Tourism retail", "Tourism other", "Public sector", "Other services")) |>
    mutate(VARIABLE = case_when(
      str_detect(VARIABLE, "Agriculture") ~ str_replace(VARIABLE,"Agriculture", "Agriculture:"),
      str_detect(VARIABLE, "Forestry") ~ str_replace(VARIABLE, "Forestry", "Forestry:"),
      str_detect(VARIABLE, "Other services") ~ str_replace(VARIABLE, "Other services", "Other services:"),
      str_detect(VARIABLE, "Public sector") ~ str_replace(VARIABLE, "Public sector", "Public sector:"),
      VARIABLE == "Film and tv" ~ "Film and TV",
      VARIABLE == "Fire" ~ "Finance, insurance and real estate",
      VARIABLE == "Ict" ~ "Information and communications technology",
      TRUE ~ VARIABLE)) |>
    filter(!VALUE %in% c("-", "F")) |> ## remove missing data
    mutate(VALUE = as.numeric(VALUE),
           FORMATTED_VALUE = label_comma()(VALUE)) |>
    group_by(KEY, REGION_NAME, REF_YEAR, STATISTIC, GEO_TYPE, PARENT_RD) |>
    slice_max(order_by = VALUE, n = 5) |>
    ungroup()

  ## Format Income Dependencies
  data[["Income Dependencies"]]<- data_orig[[3]] |>
    select(KEY, REGION_NAME, REF_YEAR, GEO_TYPE, PARENT_RD, contains("TOTAL"),
           GOVERNMENT_TRANSFER_INCOME, NON_EMPLOYMENT_MARKET_INCOME) |>
    rename_all(str_remove_all, "_TOTAL") |>
    mutate(across(FORESTRY:NON_EMPLOYMENT_MARKET_INCOME, ~str_remove_all(.x, "^(-|F)$"))) |>
    mutate(across(c(REF_YEAR), as.integer)) |>
    mutate(across(FORESTRY:NON_EMPLOYMENT_MARKET_INCOME, as.double)) |>
    pivot_longer(-c(KEY, REGION_NAME, REF_YEAR, GEO_TYPE, PARENT_RD),
                 names_to = "VARIABLE",
                 names_transform = ~snakecase::to_any_case(.x, "sentence"),
                 values_to = "VALUE") |>
    mutate(
      FORMATTED_VALUE = label_percent(accuracy = 0.01)(VALUE),
      VARIABLE = case_when(
        VARIABLE == "Film and tv" ~ "Film and TV",
        VARIABLE == "Fire" ~ "Finance, insurance and real estate",
        VARIABLE == "Ict" ~ "Information and communications technology",
        str_detect(VARIABLE, "Non employment") ~ str_replace(VARIABLE, "Non employment", "Non-employment"),
        TRUE ~ VARIABLE),
    MAP_LABEL = paste0("<strong>",REGION_NAME,"</strong><br>",VARIABLE," income dependency: ", FORMATTED_VALUE),
    MAP_TITLE = "Percent of economic base")

  data[["Economic Base"]] <- data_orig[[3]] |>
    rename_all(str_remove_all, "_TOTAL") |>
    pivot_longer(-c(KEY, REGION_NAME, REF_YEAR, STATISTIC, GEO_TYPE, PARENT_RD),
                 names_to = "VARIABLE",
                 names_transform = ~snakecase::to_any_case(.x, case = "sentence"),
                 values_to = "VALUE") |>
    filter(!VARIABLE %in% c("Total", "Forestry", "Agriculture", "Tourism transportation",
                            "Tourism accommodation", "Tourism food and beverage", "Tourism recreation and entertainment",
                            "Tourism retail", "Tourism other", "Public sector", "Other services")) |>
    mutate(VARIABLE = case_when(
      str_detect(VARIABLE, "Agriculture") ~ str_replace(VARIABLE,"Agriculture", "Agriculture:"),
      str_detect(VARIABLE, "Forestry") ~ str_replace(VARIABLE, "Forestry", "Forestry:"),
      str_detect(VARIABLE, "Other services") ~ str_replace(VARIABLE, "Other services", "Other services:"),
      str_detect(VARIABLE, "Public sector") ~ str_replace(VARIABLE, "Public sector", "Public sector:"),
      str_detect(VARIABLE, "Non employment") ~ str_replace(VARIABLE, "Non employment", "Non-employment"),
      VARIABLE == "Film and tv" ~ "Film and TV",
      VARIABLE == "Fire" ~ "Finance, insurance and real estate",
      VARIABLE == "Ict" ~ "Information and communications technology",
      TRUE ~ VARIABLE))  |>
    filter(!VALUE %in% c("-", "F")) |> ## remove missing data
    mutate(VALUE = as.numeric(VALUE),
           FORMATTED_VALUE = label_percent(accuracy = 0.01)(VALUE)) |>
    group_by(KEY, REGION_NAME, REF_YEAR, STATISTIC, GEO_TYPE, PARENT_RD) |>
    slice_max(order_by = VALUE, n = 5) |>
    ungroup()

  saveRDS(data, here::here() %,% '/app/' %,% "data.Rds")

  ## Create lookup table for RD to LA
  rd_la_lookup <- data[["Descriptive Stats"]] |>
    filter(GEO_TYPE == "EDA") |>
    distinct(RD = PARENT_RD, LA = REGION_NAME) |>
    # mutate(RD = ifelse(RD %in% LA, paste(RD, "RD"), RD)) |>
    # mutate(across(c(RD, LA), clean_regions)) |>
    arrange(RD, LA)

  rd_la_lookup <- bind_rows(
    rd_la_lookup |> mutate(RD = "All regional districts") |> arrange(RD, LA),
    rd_la_lookup
  )

  saveRDS(rd_la_lookup, here::here() %,% '/app/' %,% "rd_la_lookup.Rds")

  ## load BC mapping info (from bc data catalogue using the bcmaps package)
  map_bc <- bcmaps::bc_bound() |>
    ## simplify the boundaries, dTolerance in meters
    st_simplify(dTolerance = 100, preserveTopology = TRUE) |>
    st_transform(4326) |> ## transform to LAT/LONG for mapping
    st_union() |>
    st_as_sf() |>
    transmute(REGION_NAME = "British Columbia")

  ## load LA (EDA) mapping info (created by R_scripts/merging_geometries.R)
  map_las <- read_sf(FILE_EDA) |>
    transmute(REGION_NAME = clean_regions(EDA_name))|>
    mutate(REGION_NAME = ifelse(str_detect(REGION_NAME, "Chilcotin"), "Chilcotin", REGION_NAME)) |>
    group_by(REGION_NAME) |>
    summarize(geometry = st_union(geometry)) |>
    st_transform(3005) ## transform to meters to join with RDs and simplify (later)

  ## load RD mapping info (from bc data catalogue using the bcmaps package)
  map_rds <- bcmaps::regional_districts() |>
    transmute(REGION_NAME = clean_regions(ADMIN_AREA_NAME))|>
    mutate(REGION_NAME = ifelse(REGION_NAME %in% map_las$REGION_NAME, paste(REGION_NAME, "RD"), REGION_NAME)) |>
    bind_rows(map_las |> filter(REGION_NAME == "Northern Rockies") |> mutate(REGION_NAME = "Northern Rockies RD"))

  ## create "clipped" rd map info by removing the regions over the ocean
  non_island_rds <- map_rds |>
    filter(!REGION_NAME %in% c("Skeena-Queen Charlotte", "Kitimat-Stikine", "Central Coast RD",
                               "Mount Waddington", "Strathcona RD", "Alberni-Clayoquot",
                               "Comox Valley", "Nanaimo RD", "Cowichan Valley", "Capital",
                               "Metro Vancouver", "Sunshine Coast RD", "Powell River RD"))

  island_rds <- map_rds |>
    filter(REGION_NAME %in% c("Skeena-Queen Charlotte", "Kitimat-Stikine", "Central Coast RD",
                              "Mount Waddington", "Strathcona RD", "Alberni-Clayoquot",
                              "Comox Valley", "Nanaimo RD", "Cowichan Valley", "Capital",
                              "Metro Vancouver", "Sunshine Coast RD", "Powell River RD")) |>
    st_intersection(map_las |>
                      left_join(rd_la_lookup, by = c("REGION_NAME" = "LA")) |>
                      filter(RD %in% c("Skeena-Queen Charlotte", "Kitimat-Stikine", "Central Coast RD",
                                       "Mount Waddington", "Strathcona RD", "Alberni-Clayoquot",
                                       "Comox Valley", "Nanaimo RD", "Cowichan Valley", "Capital",
                                       "Metro Vancouver", "Sunshine Coast RD", "Powell River RD")) |>
                      summarize(geometry = smoothr::fill_holes(st_union(geometry), units::set_units(1000, km^2)))
    )

  map_rds_clipped <- bind_rows(
    non_island_rds,
    island_rds) |>
    ms_simplify(sys = TRUE, keep = 0.04) |>
    st_transform(4326) |>
    st_make_valid()

  ## simplify other map data
  map_las <- map_las |>
    ms_simplify(sys = TRUE, keep = 0.04) |>
    st_transform(4326) |>
    st_make_valid()

  map_rds <- map_rds |>
    ms_simplify(sys = TRUE, keep = 0.04) |>
    st_transform(4326) |>
    st_make_valid()

  saveRDS(map_las, "app/map_las.rds")
  saveRDS(map_rds, "app/map_rds.rds")
  saveRDS(map_rds_clipped, "app/map_rds_clipped.rds")
  saveRDS(map_bc, "app/map_bc.rds")

} else {
  data = readRDS(here::here(ifelse(is_local, 'app', '.'), "data.Rds"))
  rd_la_lookup = readRDS(here::here(ifelse(is_local, 'app', '.'),"rd_la_lookup.Rds"))
  map_las <- readRDS(here::here(ifelse(is_local, 'app', '.'), "map_las.rds"))
  map_rds <- readRDS(here::here(ifelse(is_local, 'app', '.'), "map_rds.rds"))
  map_rds_clipped<- readRDS(here::here(ifelse(is_local, 'app', '.'), "map_rds_clipped.rds"))
  map_bc <- readRDS(here::here(ifelse(is_local, 'app', '.'), "map_bc.rds"))
}

# read all the tooltips we'll use later
tooltips = source(here::here() %,% ifelse(is_local, '/app', '') %,% '/R/tooltips.R')$value

# tease out the years for which we have data
latest_year = max(data[[1]]$REF_YEAR)
earliest_year = min(data[[1]]$REF_YEAR)
years = unique(data[[1]]$REF_YEAR)


# tease out the industries from data[[2]]
#industries = to_sentence_case(setdiff(names(data[[2]]), c("KEY", "REGION_NAME", "REF_YEAR", "STATISTIC", "GEO_TYPE", "PARENT_RD", "TOTAL")))

# read the code for the home page
#home_page = source(here::here() %,% ifelse(is_local, '/app', '') %,% '/home_page.R')$value



# Make the nice HTML for the labels on the map widget
# summary_map_labels = map(filter(RDs_sf, REF_YEAR == last_year) |> pull(REGION_NAME), function(name) {
#   "<strong>" %,% name %,% "</strong><br/>\n" %,% (map(regional_profile_info$col, function(col) {
#     regional_profile_info$col_short[match(col, regional_profile_info$col)] %,% ":" %,,% regional_profile_info$label[[match(col, regional_profile_info$col)]](pull(filter(RDs_sf, REF_YEAR == last_year, REGION_NAME == name), col))
#   }) |>
#       paste(collapse="<br />"))
# }) |>
#   lapply(HTML)




