
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


## Use this file to save functions that your app will use

`%,%` = paste0
`%,,%` = paste
pa = function(x) print(x, n=Inf)


home_page = function() htmltools::p(paste(read_lines(here::here() %,% "/app/www/about.txt"), collapse = "\n"))

regional_profile_page = function(region='Nelson') {

  regional_data = filter(data[[1]], REGION_NAME == region)

  g1 = renderPlot(
    regional_data |>
    select(REF_YEAR, POPULATION, TOTAL_JOBS, TOTAL_INCOME_M, AVERAGE_EMPLOYMENT_INCOME, DIVERSITY_INDEX) |>
    pivot_longer(cols = 2:last_col()) |>
    mutate(year = fct_rev(as.factor(REF_YEAR))) |>
    ggplot(aes(y=name, x=value, fill=year)) +
    geom_col(position = 'dodge') +
    ggthemes::theme_clean() +
    theme(legend.position = 'bottom'))

  make_infobox = function(title = 'Population', col = NULL, formatter = scales::label_comma, color='olive', icon='map') {

    if (is.null(col)) col = snakecase::to_screaming_snake_case(title)
    infoBox(title, value = formatter()(filter(regional_data, REF_YEAR == last_year) |> pull(!!col)), subtitle = "B.C. Total: " %,% formatter()(filter(data[[1]], REGION_NAME == 'British Columbia', REF_YEAR == last_year) |> pull(!!col)), color = color, icon = icon(icon), fill = T)
  }

  fluidPage(
    h1("Regional Index for" %,,% region),
    fluidRow(
      make_infobox(),
      make_infobox("Total Jobs", color='navy', icon='arrow-left'),
      make_infobox("Average Employment Income", formatter=scales::label_dollar, color='orange')
    ),
    fluidRow(
      make_infobox("Basic Income Share", formatter=scales::label_percent),
      make_infobox("Diversity Index", formatter=scales::label_comma),
      make_infobox("Forest Sector Vulnerability Index", formatter=scales::label_comma)
    ),
    fluidRow(
      tabBox(title = "Economic Profile",
        tabPanel("Data", regional_data |>
            select(REF_YEAR, POPULATION, TOTAL_JOBS, TOTAL_INCOME_M, AVERAGE_EMPLOYMENT_INCOME, DIVERSITY_INDEX) |>
            janitor::clean_names(case='sentence') |>
            reactable()),
        tabPanel("Graph", plotOutput("g1"))
      )
    )
  )
}

regional_profile_page() |> htmltools::browsable()
