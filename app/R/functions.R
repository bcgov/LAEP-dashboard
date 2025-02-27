
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

bcsapps_bslib_header = function(id = 'header', appname = 'LAEP dashboard', github = NULL) {
  ns <- shiny::NS(id)

  htmltools::tagList(htmltools::tags$head(htmltools::tags$style(htmltools::HTML("#header_col {background-color:#003366; border-bottom:2px solid #fcba19; position:fixed; z-index:10000;\"}"))),

    # I added this one
    htmltools::tags$head(htmltools::tags$style(htmltools::HTML(".navbar-static-top {margin: 30px 0 20px 0;}"))),

    htmltools::tags$head(htmltools::tags$style(htmltools::HTML(".header {padding: 10px 0 10px 0; display:flex; height:80px; width:100%;}"))),
    htmltools::tags$head(htmltools::tags$style(htmltools::HTML(".banner {width:100%; display:flex; justify-content:flex-start; align-items:center; margin: 0 10px 0 10px}"))),
    htmltools::tags$head(htmltools::tags$style(htmltools::HTML("#app_title {font-weight:400; color:white; margin: 5px 5px 0 18px;}"))),
    htmltools::tags$head(htmltools::tags$style(htmltools::HTML(".link_list_div {margin-left:auto; margin-right: 20px; inline-size: min-content;}"))),
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


make_infobox = function(df, title = 'Population', col = NULL, formatter = scales::label_comma, color=NULL, icon='map') {
  if (is.null(color)) color = sample(c('red', 'yellow', 'aqua', 'blue', 'light-blue', 'green', 'navy', 'teal', 'olive', 'lime', 'orange', 'fuchsia', 'purple', 'maroon', 'black'), 1)
  if (is.null(col)) col = snakecase::to_screaming_snake_case(title)
  infoBox(title, value = formatter()(filter(df, REF_YEAR == last_year) |> pull(!!col)), subtitle = "B.C. Total: " %,% formatter()(filter(data[[1]], REGION_NAME == 'British Columbia', REF_YEAR == last_year) |> pull(!!col)), color = color, icon = icon(icon), fill = T)
}

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
