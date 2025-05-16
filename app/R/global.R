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




# PARAMETERS

# I envision this section requiring tinkering with over time.



# Do you want to load and process data from the Excel file (T) or load data from the .Rds file? The latter is faster but obviously requires generating the file and saving it beforehand
load_data = F

xl_path = path_wd('app/Local Area Economic Profiles 2024 Toolkit V3.xlsx')

pages = tibble(
  name = c("Home", "Regional Profile", "LAEP Calculator"),
  icon = c("home", "line-chart", "calculator")
)

# Start page for local/shinyapps.io
start_page = c("Regional Profile", "Home")

# This tibble stores info for the "Regional Profile" page. For example, what columns from data[[1]] do you want to display? Hopefully, pretty self-explanatory. Note that `label` is a string for how to format the number: regular with a comma (you can use NA if lazy), dollar, or percent are the obvious choices. This string will be converted into the appropriate corresponding label function from the scales package.
regional_profile_info = tibble(
  col = c("POPULATION", "TOTAL_JOBS", "TOTAL_INCOME", "AVERAGE_EMPLOYMENT_INCOME", "DIVERSITY_INDEX"),
  col_formatted = c("Population", "Total jobs", "Total income (millions)", "Average employment income", "Diversity index"),
  icon = c("earth-americas", "user-tie", "money-bills", "hand-holding-dollar", "chart-pie"),
  label = list(label_comma(), label_comma(), label_dollar(), label_dollar(suffix = "/yr"), label_comma())
  )


# I'm working on this project daily ATM so we'll leave it like this for now
last_updated = format(ymd("2025-03-07"), "%b %d, %Y")





# Constants

# I envision this section not requiring much tinkering over time.

is_local = Sys.getenv('SHINY_PORT') == ""

# This chunk reads in the data from the Excel file and processes it. I am worried about reproducibility here because the Excel file is complex. You can see that currently it attempts to read these tabs: 'Descriptive Stats', 'Jobs', 'Income Dependencies', 'Location Quotients', 'Employment Impact Ratios', 'Avg Incomes'. The c(3, 5, 5, 5, 4, 5) refers to the 'skip' parameter for each tab when reading. This is not ideal - would be much safer to have this data in a more uniform state. However, this works for the time being. You will see that three .Rds files are created: data, BC_sf, and RD_sf. `data` is a list, with each element as the (processed) Excel tab (ie data[[1]] is the data for 'Descriptive Stats' and so on). The two `_sf` objects are the geography data from the bcmaps package, joined up with data[[1]].

# Some questions to ponder going forward:

# Why does it say 'RD' at the end of some of these?
# is data[[1]] consistent with data[[2]] and so on? Can we check formally?
# Do these line up nicely with bcmaps?
# Is there a way to match geography with LA-level data?
# Why are we missing two RDs in the map?

if (load_data) {

  sheets <- c('Descriptive Stats', 'Jobs', 'Income Dependencies')
  start_row <- c(3, 5, 5)

  data = map2(sheets, start_row, function(sheet, skip) readxl::read_excel(xl_path, sheet, skip = skip) |> janitor::clean_names('screaming_snake'))
  names(data) <- sheets

  ## clean region names across all sheets
  data <- map(data, ~mutate(.x, across(c(REGION_NAME, PARENT_RD), clean_regions)))

  ## add "RD" suffix to regional districts with the same name as a local area
  LA_list <- data[[1]] |> filter(GEO_TYPE == "EDA") |> distinct(REGION_NAME) |> pull()
  data <- map(data, ~mutate(.x, REGION_NAME = ifelse(GEO_TYPE == "RD" & REGION_NAME %in% LA_list, paste(REGION_NAME, "RD"), REGION_NAME)))
  data <- map(data, ~mutate(.x, PARENT_RD = ifelse(PARENT_RD %in% LA_list, paste(PARENT_RD, "RD"), PARENT_RD)))

  ## Format Descriptive Stats
  data[["Descriptive Stats"]] <- data[["Descriptive Stats"]] |>
    select(KEY, REGION_NAME, REF_YEAR, GEO_TYPE, PARENT_RD, POPULATION, TOTAL_JOBS,
           AVERAGE_EMPLOYMENT_INCOME, TOTAL_INCOME = TOTAL_INCOME_M, DIVERSITY_INDEX,
           DOMINANT_BASIC_INCOME_SOURCE, DOMINANT_PRIVATE_SECTOR_EMPLOYMENT_BASIC_INCOME_SOURCE) |>
    mutate(across(POPULATION:DIVERSITY_INDEX, ~str_remove_all(.x, "-"))) |>
    mutate(across(c(REF_YEAR, POPULATION, TOTAL_JOBS), as.integer)) |>
    mutate(across(AVERAGE_EMPLOYMENT_INCOME:DIVERSITY_INDEX, as.double))

  ## Format Jobs - Top 5 jobs per region
  data[["Jobs"]] <- data[["Jobs"]] |>
    pivot_longer(-c(KEY, REGION_NAME, REF_YEAR, STATISTIC, GEO_TYPE, PARENT_RD),
                 names_to = "INDUSTRY",
                 names_transform = ~snakecase::to_any_case(.x, "title"),
                 values_to = "NUMBER_OF_JOBS") |>
    filter(!INDUSTRY %in% c("Total", "Forestry Total", "Agriculture Total", "Tourism: Transportation",
                            "Tourism: Accommodation", "Tourism: Food and beverage", "Tourism: Recreation and entertainment",
                            "Tourism: Retail", "Tourism: Other", "Public Sector Total", "Other Services Total")) |>
    filter(!NUMBER_OF_JOBS %in% c("-", "F")) |>
    mutate(NUMBER_OF_JOBS = as.numeric(NUMBER_OF_JOBS)) |>
    group_by(KEY, REGION_NAME, REF_YEAR, STATISTIC, GEO_TYPE, PARENT_RD) |>
    slice_max(order_by = NUMBER_OF_JOBS, n = 5) |>
    ungroup()

  ## Format Income Dependencies
  ## ...

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



  # BC_sf = bc_bound()
  # saveRDS(BC_sf, here::here() %,% ifelse(is_local, '/app', '') %,% "/BC_sf.Rds")
  #
  # RDs_sf = regional_districts() |>
  #   mutate(ADMIN_AREA_NAME_CLEANED = clean_regions(ADMIN_AREA_NAME), .before=1) |>
  #   select(ADMIN_AREA_NAME_CLEANED, geometry) |>
  #   rename(REGION_NAME = ADMIN_AREA_NAME_CLEANED) |>
  #   inner_join(filter(data[[1]], GEO_TYPE == "RD")) |>
  #   select(REGION_NAME, geometry, REF_YEAR, POPULATION:last_col()) |>
  #   st_as_sf() |>
  #   st_transform(crs = 4326)
  #
  # saveRDS(RDs_sf, here::here() %,% ifelse(is_local, '/app', '') %,% "/RDs_sf.Rds")

} else {
  data = readRDS(here::here(ifelse(is_local, 'app', '.'), "data.Rds"))
  # BC_sf = readRDS(here::here() %,% ifelse(is_local, '/app', '') %,% "/BC_sf.Rds")
  # RDs_sf = readRDS(here::here() %,% ifelse(is_local, '/app', '') %,% "/RDs_sf.Rds")
  rd_la_lookup = readRDS(here::here(ifelse(is_local, 'app', '.'),"rd_la_lookup.Rds"))
}

# boolean for whether this is running locally or on shinyapps.io
is_local = Sys.getenv('SHINY_PORT') == ""

# Two dfs that filter data[[1]] by RD or LA level.
# RDs = data[[1]] |>
#   filter(GEO_TYPE == "RD") |>
#   pull(REGION_NAME) |>
#   unique() |>
#   sort()
# LAs = data[[1]] |>
#   filter(GEO_TYPE == "EDA") |>
#   pull(REGION_NAME) |>
#   unique() |>
#   sort()

# tease out the years for which we have data
last_year = max(data[[1]]$REF_YEAR)
first_year = min(data[[1]]$REF_YEAR)
years = unique(data[[1]]$REF_YEAR)
shift_share_year_combos = crossing(years, years) |> set_names(c("y1", "y2")) |> filter(y1 < y2) |> transmute(x=y1 %,,% "to" %,,% y2) |> deframe()

# tease out the industries from data[[2]]
industries = to_sentence_case(setdiff(names(data[[2]]), c("KEY", "REGION_NAME", "REF_YEAR", "STATISTIC", "GEO_TYPE", "PARENT_RD", "TOTAL")))

# read the code for the home page
home_page = source(here::here() %,% ifelse(is_local, '/app', '') %,% '/home_page.R')$value

# read all the tooltips we'll use later
tooltips = source(here::here() %,% ifelse(is_local, '/app', '') %,% '/tooltips.R')$value

# Make the nice HTML for the labels on the map widget
# summary_map_labels = map(filter(RDs_sf, REF_YEAR == last_year) |> pull(REGION_NAME), function(name) {
#   "<strong>" %,% name %,% "</strong><br/>\n" %,% (map(regional_profile_info$col, function(col) {
#     regional_profile_info$col_short[match(col, regional_profile_info$col)] %,% ":" %,,% regional_profile_info$label[[match(col, regional_profile_info$col)]](pull(filter(RDs_sf, REF_YEAR == last_year, REGION_NAME == name), col))
#   }) |>
#       paste(collapse="<br />"))
# }) |>
#   lapply(HTML)


# make the nice HTML for the page selector
pages$choice = lapply(1:nrow(pages), function(i) span(icon(pages$icon[i]), pages$name[i]))

