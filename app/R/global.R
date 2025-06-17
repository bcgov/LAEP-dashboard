# Copyright 2025 Province of British Columbia
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


# parameters ----
last_updated = format(ymd("2025-06-17"), "%b %d, %Y")

# Do you want to include google analytics tracking code
google_tracking = F

# Do you want to load and process data from the Excel file (T) or load data from the .Rds file? The latter is faster but obviously requires generating the file and saving it beforehand
load_data = F

# Excel toolkit file name
## note path will be rootfolder/data
xl_filename = "Local Area Economic Profiles 2024 Toolkit V3.xlsx"

# LA shape file name
## note path will be rootfolder/data
FILE_EDA <- "EDA_union.shp"

## Color code for industries
industry_colors = c(
  "Agriculture and food" = "#006d2c",               ## dark green
  "Construction" = "#d95f0e",                       ## orange
  "Finance, insurance and real estate" = "#54278f", ## dark purple
  "Fishing hunting and trapping" = "#e31a1c",       ## red
  "Forestry" = "#7fbf7b",                           ## light green
  "Information and communications technology" = "#ae017e", ## magenta
  "Mining" = "#993404",                             ## rust
  "Oil and gas" = "#636363",                        ## dark grey
  "Other services" = "#cccccc",                     ## light grey
  "Rail transport" = "#df65b0",                     ## pink
  "Retail trade" = "#80B1D3",                       ## med blue
  "Tourism" = "#08519c",                            ## dark blue
  "Water transport" = "#b9dbed",                    ## light blue
  "Government transfers" = "#8dd3c7",               ## light teal
  "Non-employment market income" = "#9e9ac8",       ## light purple
  "Public sector" = "#d8b365"                       ## sand
)

# create app data ----
## If load_data = T:
# Reads in the data from the Excel file and processes it
# Creates RD to LA lookup table
# Creates the spatial files from FILE_EDA and bcmaps
## note: assumes the code is run locally for file paths
if (load_data) {
  source(here::here("app", "R", "functions.R"))

  sheets <- c("Descriptive Stats", "Jobs", "Income Dependencies", "Location Quotients")
  start_row <- c(3, 5, 5, 5)

  data_orig = map2(sheets,
                   start_row,
                   function(sheet, skip) {
                     readxl::read_excel(here::here("data", xl_filename), sheet, skip = skip) |>
                       janitor::clean_names("screaming_snake") |>
                       filter(!is.na(REGION_NAME))
                   })

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
    mutate(
      FORMATTED_VALUE = case_when(
        VARIABLE == "Population" ~ label_comma(accuracy = 1)(VALUE),
        VARIABLE == "Total jobs" ~ label_comma(accuracy = 1)(VALUE),
        VARIABLE == "Average employment income" ~ label_dollar(suffix = "/yr")(VALUE),
        VARIABLE == "Total income" ~ label_dollar(suffix = " million")(VALUE),
        VARIABLE == "Diversity index" ~ label_comma(accuracy = 0.1)(VALUE)),
      ICON = case_when(
        VARIABLE == "Population" ~ "earth-americas",
        VARIABLE == "Total jobs" ~ "user-tie",
        VARIABLE == "Average employment income" ~ "money-bills",
        VARIABLE == "Total income" ~ "hand-holding-dollar",
        VARIABLE == "Diversity index" ~ "chart-pie"))

  ## Add map labels
  map_labels <- bind_rows(
    data[["Descriptive Stats"]] |>
      group_by(REGION_NAME, REF_YEAR) |>
      mutate(MAP_LABEL = paste0("<strong>", REGION_NAME, "</strong><br>", paste(paste0(VARIABLE, ": ", FORMATTED_VALUE), collapse = "<br>"))) |>
      ## to add an intermediate title after population stat, use str_replace
      mutate(MAP_LABEL = str_replace_all(MAP_LABEL, "Total jobs", "<br><span style = 'font-weight:700'>Other regional statistics</span><br>Total jobs")) |>
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
           VARIABLE = str_remove_all(VARIABLE, "employment "),
           COLOR = as.character(industry_colors[VALUE]),
           COLOR = ifelse(is.na(COLOR), "white", COLOR),
           MAP_LABEL = paste0("<strong>",REGION_NAME,"</strong><br>", FORMATTED_VALUE))

  ## Format Jobs - Top 5 jobs per region
  jobs <- data_orig[[2]] |>
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
           FORMATTED_VALUE = label_comma()(VALUE))

  ## get top 5 jobs
  data[["Jobs"]] <- jobs |>
    semi_join(jobs |>
                group_by(KEY, REGION_NAME, REF_YEAR, STATISTIC, GEO_TYPE, PARENT_RD) |>
                slice_max(order_by = VALUE, n = 5) |>
                ungroup() |>
                distinct(REGION_NAME, VARIABLE),
              by = c("REGION_NAME", "VARIABLE"))
  rm(jobs)


  ## Format Income Dependencies
  data[["Income Shares Map"]]<- data_orig[[3]] |>
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
    MAP_LABEL = paste0("<strong>",REGION_NAME,"</strong><br>",
                       str_wrap(paste0(VARIABLE," share: ", FORMATTED_VALUE), width = 30)),
    MAP_LABEL = str_replace_all(MAP_LABEL, "\n", "<br>"),
    MAP_TITLE = str_wrap(paste("Basic income share from:", VARIABLE), width = 20))

  ## format data for the income shares table
  data[["Income Shares Table"]] <- data_orig[[3]] |>
    pivot_longer(-c(KEY, REGION_NAME, REF_YEAR, STATISTIC, GEO_TYPE, PARENT_RD),
                 names_to = "VARIABLE",
                 names_transform = ~snakecase::to_any_case(.x, case = "sentence"),
                 values_to = "VALUE") |>
    mutate(TOTAL_COLUMN = str_detect(VARIABLE, "total|Government transfer income|Non employment market income"),
           PARENT_VARIABLE = as.character(str_extract_all(VARIABLE, paste("Forestry", "Agriculture", "Tourism", "Public sector", "Other services", sep = "|"))),
           VARIABLE = case_when(
             !TOTAL_COLUMN ~ str_remove_all(VARIABLE, paste("Forestry", "Agriculture", "Tourism", "Public sector", "Other services", sep = "|")),
              TOTAL_COLUMN ~ str_remove_all(VARIABLE, "total")),
           VARIABLE = VARIABLE |> str_squish() |> to_any_case(case = "sentence"),
           VARIABLE = case_when(
             VARIABLE == "Film and tv" ~ "Film and TV",
             VARIABLE == "Fire" ~ "Finance, insurance and real estate",
             VARIABLE == "Ict" ~ "Information and communications technology",
             VARIABLE == "Non employment market income" ~ "Non-employment market income",
             TRUE ~ VARIABLE),
           VARIABLE = fct_inorder(VARIABLE), ## make factor to keep correct order
           TABLE_ORDER = as.integer(VARIABLE),
           VALUE = str_remove_all(VALUE, "^(-|F)$"),
           VALUE = as.numeric(VALUE),
           FORMATTED_VALUE = label_percent(accuracy = 0.01)(VALUE))

  ## Format Location Quotients - Top 5 industries per region
  lq <- data_orig[[4]] |>
    rename_all(str_remove_all, "_TOTAL") |>
    pivot_longer(-c(KEY, REGION_NAME, REF_YEAR, STATISTIC, GEO_TYPE, PARENT_RD),
                 names_to = "VARIABLE",
                 names_transform = ~snakecase::to_any_case(.x, case = "sentence"),
                 values_to = "VALUE") |>
    filter(!VARIABLE %in% c("Total", "Forestry", "Agriculture", "Tourism transportation",
                            "Tourism accommodation", "Tourism food and beverage", "Tourism recreation and entertainment",
                            "Tourism retail", "Tourism other", "Public sector", "Other services")) |>
    mutate(VARIABLE = case_when(
      REGION_NAME == "British Columbia" ~ "All industries",
      str_detect(VARIABLE, "Agriculture") ~ str_replace(VARIABLE,"Agriculture", "Agriculture:"),
      str_detect(VARIABLE, "Forestry") ~ str_replace(VARIABLE, "Forestry", "Forestry:"),
      str_detect(VARIABLE, "Other services") ~ str_replace(VARIABLE, "Other services", "Other services:"),
      str_detect(VARIABLE, "Public sector") ~ str_replace(VARIABLE, "Public sector", "Public sector:"),
      VARIABLE == "Film and tv" ~ "Film and TV",
      VARIABLE == "Fire" ~ "Finance, insurance and real estate",
      VARIABLE == "Ict" ~ "Information and communications technology",
      TRUE ~ VARIABLE)) |>
    filter(!VALUE %in% c("-", "F")) |> ## remove missing data
    mutate(VALUE = ifelse(REGION_NAME == "British Columbia", 1, as.numeric(VALUE)),
           FORMATTED_VALUE = ifelse(REGION_NAME == "British Columbia", 1, label_comma(accuracy = 0.01)(round_half_up(VALUE, digits = 2)))) |>
    distinct(KEY, REGION_NAME, REF_YEAR, STATISTIC, GEO_TYPE, PARENT_RD, VARIABLE, VALUE, FORMATTED_VALUE)

  ## get top 5 lqs
  data[["Location Quotients"]] <- lq |>
    semi_join(lq |>
                group_by(KEY, REGION_NAME, REF_YEAR, STATISTIC, GEO_TYPE, PARENT_RD) |>
                slice_max(order_by = VALUE, n = 5) |>
                ungroup() |>
                distinct(REGION_NAME, VARIABLE),
              by = c("REGION_NAME", "VARIABLE"))
  rm(lq)

  ## save data
  saveRDS(data, here::here("app", "data", "data.rds"))

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

  ## save lookup table
  saveRDS(rd_la_lookup, here::here("app", "data", "rd_la_lookup.rds"))

  ## load LA (EDA) mapping info (created by R_scripts/merging_geometries.R)
  map_las <- read_sf(here::here("data", FILE_EDA)) |>
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
    ms_simplify(sys = TRUE, keep = 0.2) |>
    st_transform(4326) |>
    st_make_valid()

  map_rds <- map_rds |>
    ms_simplify(sys = TRUE, keep = 0.04) |>
    st_transform(4326) |>
    st_make_valid()

  ## explicitly make geographic attributes constant
  st_agr(map_las) <- "constant"
  st_agr(map_rds) <- "constant"
  st_agr(map_rds_clipped) <- "constant"

  ## save mapping data
  saveRDS(map_las, here::here("app", "data", "map_las.rds"))
  saveRDS(map_rds, here::here("app", "data", "map_rds.rds"))
  saveRDS(map_rds_clipped, here::here("app", "data", "map_rds_clipped.rds"))

}

# load data for app ----

is_local = Sys.getenv("SHINY_PORT") == ""

## attribute data and lookup table
data = readRDS(here::here(ifelse(is_local, "app", "."), "data", "data.rds"))
rd_la_lookup = readRDS(here::here(ifelse(is_local, "app", "."), "data", "rd_la_lookup.rds"))

## map data
map_las <- readRDS(here::here(ifelse(is_local, "app", "."), "data", "map_las.rds"))
map_rds <- readRDS(here::here(ifelse(is_local, "app", "."), "data", "map_rds.rds"))
map_rds_clipped<- readRDS(here::here(ifelse(is_local, "app", "."), "data", "map_rds_clipped.rds"))

## tooltips
tooltips = source(here::here(ifelse(is_local, "app", "."), "R", "tooltips.R"))$value




