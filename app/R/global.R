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

## Some useful libraries
pacman::p_load(shiny, bslib, tidyverse, htmltools, reactable, shinyjs, shinyWidgets, fs, janitor, snakecase, readr, plotly)


home_text = paste(readr::read_lines(here::here() %,% "/app/www/about.txt"), collapse = "\n")

xl_path = path(here::here() %,% '/data/Local Area Economic Profiles 2024 Toolkit V3.xlsx')

if (!file_exists(here::here() %,% "/data/data.Rds")) {
  data = map2(c('Descriptive Stats', 'Jobs', 'Income Dependencies', 'Location Quotients', 'Employment Impact Ratios', 'Avg Incomes'), c(3, 5, 5, 5, 4, 5), function(sheet, skip) readxl::read_excel(xl_path, sheet, skip = skip) |> janitor::clean_names('screaming_snake'))

  names(data[[1]]) = str_replace(names(data[[1]]), "_M$", "")

  data[[1]] = data[[1]] |>
    mutate(across(c(POPULATION, TOTAL_JOBS, REF_YEAR), as.integer)) |>
    mutate(across(AVERAGE_EMPLOYMENT_INCOME:FOREST_SECTOR_VULNERABILITY_INDEX, as.double))
  saveRDS(data, "data/data.Rds")
} else data = readRDS(here::here() %,% "/data/data.Rds")

rds = data[[1]] |> filter(GEO_TYPE == "RD") |> unique() |> pull(REGION_NAME)
edas = data[[1]] |> filter(GEO_TYPE == "EDA") |> unique() |> pull(REGION_NAME)
last_year = max(data[[1]]$REF_YEAR)
first_year = min(data[[1]]$REF_YEAR)
years = unique(data[[1]]$REF_YEAR)
regions = setdiff(unique(data[[1]]$REGION_NAME), "British Columbia")
industries = to_sentence_case(setdiff(names(data[[2]]), c("KEY", "REGION_NAME", "REF_YEAR", "STATISTIC", "GEO_TYPE", "PARENT_RD", "TOTAL")))
shift_share_year_combos = crossing(years, years) |> set_names(c("y1", "y2")) |> filter(y1 < y2) |> transmute(x=y1 %,,% "to" %,,% y2) |> deframe()

home_page = source(here::here() %,% '/home_page.R')
tooltips = source(here::here() %,% '/tooltips.R')
