
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


pacman::p_load(shiny, bslib, tidyverse, htmltools, reactable, shinyjs, shinyWidgets, fs, janitor, snakecase, readr, plotly)

`%,%` = paste0
`%,,%` = paste
pa = function(x) print(x, n=Inf)

tooltip_text = function(text="text", tooltip_text = "This is a tooltip yo", icon_name = "info-circle", icon_style = 'color: red;') span(text, tippy::tippy(icon(icon_name, style=icon_style), tooltip_text))

make_value_box = function(df, title = 'Population', col = NULL, formatter = scales::label_comma, theme=NULL, icon='map') {
  all_themes = c("primary", "secondary", "success", "info", "warning", "danger", "light", "dark")

  if (is.null(theme)) theme = sample(all_themes, 1)
  if (is.null(col)) col = snakecase::to_screaming_snake_case(title)
  bslib::value_box(
    title = title,
    value = formatter()(filter(df, REF_YEAR == last_year) |> pull(!!col)),
    showcase = icon(icon),
    theme = theme,
    p("B.C. Total: " %,% formatter()(filter(data[[1]], REGION_NAME == 'British Columbia', REF_YEAR == last_year) |> pull(!!col))),
  )
}

t2 = function(df, pivot_col, new_col_name) {
  na = setdiff(names(df), pivot_col)
  pi = pull(df[pivot_col])
  df = df |>
    select(-all_of(pivot_col)) |>
    t() |>
    as_tibble(.name_repair = 'minimal')
  names(df) = pi
  df[new_col_name] = to_sentence_case(na)
  df = relocate(df, all_of(new_col_name), .before=1)
  return(df)
}

laep_scenario_card = function(i) {
  card(
    id = "laep_" %,% i,
    full_screen = T,
    card_title("Scenario #" %,% i),
    layout_column_wrap(width=1/4, fill = F,
      pickerInput("laep_year_" %,% i, label = "Select Reference Year", choices = years),
      pickerInput("laep_area_" %,% i, label = "Select Region", choices = regions),
      pickerInput("laep_industry_" %,% i, label = "Select Industry", choices = industries),
      pickerInput("laep_SSN_" %,% i, label = "Social Safety Net?", choices = c("Yes", "No"))
    ),
    #reactableOutput("laep_t" %,% i),
    reactableOutput("laep_t" %,% i),
    #layout_column_wrap(width=1/2, fill = F,
      #actionBttn("add_laep" %,% i, "Add a Scenario"),
      div(style = 'max-width: 300px;', actionBttn("delete_laep_" %,% i, "Delete this Scenario", color = 'danger'))
    #)
  )
}

bcsHeader = function (id, appname, github = NULL)
{
  ns <- shiny::NS(id)
  htmltools::tagList(htmltools::tags$head(htmltools::tags$style(htmltools::HTML("#header_col {background-color:#003366; border-bottom:2px solid #fcba19; position:fixed; z-index:10000;\"}"))),
  #htmltools::tagList(htmltools::tags$head(htmltools::tags$style(htmltools::HTML("#header_col {background-color:#003366; border-bottom:2px solid #fcba19; position:fixed;\"}"))),

    htmltools::tags$head(htmltools::tags$style(htmltools::HTML(".header {padding:0 0px 0 0px; display:flex; height:80px; width:100%;}"))),
    htmltools::tags$head(htmltools::tags$style(htmltools::HTML(".banner {width:100%; display:flex; justify-content:flex-start; align-items:center; margin: 0 10px 0 10px}"))),
    htmltools::tags$head(htmltools::tags$style(htmltools::HTML("#app_title {font-weight:400; color:white; margin: 5px 5px 0 18px;}"))),

    #htmltools::tags$head(htmltools::tags$style(htmltools::HTML(".link_list_div {margin-left:auto; margin-right:0;}"))),
    htmltools::tags$head(htmltools::tags$style(htmltools::HTML(".link_list_div {margin-left:auto; margin-right:50px;}"))),



    shiny::column(id = "header_col", width = 12, htmltools::tags$header(class = "header",
      htmltools::tags$div(class = "banner", htmltools::a(href = "https://www2.gov.bc.ca/gov/content/data/about-data-management/bc-stats",
        htmltools::img(src = "bcstats_logo_rev.png",
          title = "BC Stats", height = "80px", alt = "British Columbia - BC Stats"),
        onclick = "gtag"), shiny::h1(id = "app_title",
          appname), htmltools::tags$div(class = "link_list_div",
            shiny::uiOutput(ns("links_yn"))), if (!is.null(github))
              htmltools::tags$a(href = github, shiny::icon("github",
                "fa-lg"), style = "color:white")))))
}
