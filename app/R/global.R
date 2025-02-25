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

data = map2(c('Descriptive Stats', 'Jobs', 'Income Dependencies', 'Location Quotients', 'Employment Impact Ratios', 'Avg Incomes'), c(3, 5, 5, 5, 4, 5), function(sheet, skip) readxl::read_excel(xl_path, sheet, skip = skip))

# fix #5
