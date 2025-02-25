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
library(shiny)
library(shinydashboard)
library(tidyverse)  ## for data manipulation
library(janitor)    ## for cleaning data (includes rounding functions)
library(lubridate)  ## for dates
library(ggplot2)    ## for plots
library(plotly)     ## for interactive plots
library(DT)         ## for tables
pacman::p_load(reactable, shinyjs, shinyWidgets, fs)


xl_path = path(here::here() %,% '/data/Local Area Economic Profiles 2024 Toolkit V3.xlsx')

if (!file_exists("data/data.Rds")) {
  data = map2(c('Descriptive Stats', 'Jobs', 'Income Dependencies', 'Location Quotients', 'Employment Impact Ratios', 'Avg Incomes'), c(3, 5, 5, 5, 4, 5), function(sheet, skip) readxl::read_excel(xl_path, sheet, skip = skip) |> janitor::clean_names('screaming_snake'))

  # fix #5

  data[[1]] = data[[1]] |>
    mutate(across(c(POPULATION, TOTAL_JOBS, REF_YEAR), as.integer)) |>
    mutate(across(AVERAGE_EMPLOYMENT_INCOME:FOREST_SECTOR_VULNERABILITY_INDEX, as.double))
  saveRDS(data, "data/data.Rds")
} else data = readRDS("data/data.Rds")

rds = data[[1]] |> filter(GEO_TYPE == "RD") |> unique() |> pull(REGION_NAME)
edas = data[[1]] |> filter(GEO_TYPE == "EDA") |> unique() |> pull(REGION_NAME)
last_year = max(data[[1]]$REF_YEAR)
first_year = min(data[[1]]$REF_YEAR)
years = unique(data[[1]]$REF_YEAR)
