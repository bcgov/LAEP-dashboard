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


pacman::p_load(shiny, bslib, tidyverse, htmltools, reactable, shinyjs, shinyWidgets, fs, janitor, snakecase, readr, plotly, leaflet, bcmaps, scales, openxlsx2, ggiraph, patchwork, shinycssloaders)

# some convenience functions
`%,%` = paste0
`%,,%` = paste
pa = function(x) print(x, n=Inf)

is_local = Sys.getenv('SHINY_PORT') == ""

# make the little red 'i' thingies for popover information
info_icon = function(tooltip) popover(icon("info-circle", style="color: red"), tooltip)

# this is probably not well-written but it works: it's a function to transpose a data frame, intended for those output tables that have years as column names. `pivot_col` is typically going to be REF_YEAR; it's the column that will become the col names of the df. `new_col_name` is the name of first column of the resultant df, something like "Industry"
t2 = function(df, pivot_col, new_col_name) {
  na = setdiff(names(df), pivot_col)
  pi = pull(df[pivot_col])
  df = df |>
    select(-all_of(pivot_col)) |>
    t() |>
    as_tibble(.name_repair = 'unique')
  names(df) = as.character(pi)
  df[new_col_name] = to_sentence_case(na)
  df = relocate(df, all_of(new_col_name), .before=1)
  return(df)
}

# So data plays nicely with bcmaps
clean_regions = function(x) {
  x |>
    str_replace("Regional District( of)?", "") |>
    str_replace("Region", "") |>
    str_replace("RD", "") |>
    str_replace("-", " ") |>
    str_replace("\\(.+\\)", "") |>
    str_replace("-", " ") |>
    str_replace("qathet", "Powell River") |>
    str_replace("North Coast", "Skeena Queen Charlotte") |>
    str_squish()
}


# Returns an individual bslib 'value box' for the first row of the Regional Profile page
make_value_box = function(df, title, col, labeller, theme, icon, tooltip = NULL) {
  bslib::value_box(
    title = if (is.null(tooltip)) title else span(title, info_icon(tooltip)),
    value = labeller(filter(df, REF_YEAR == last_year) |> pull(!!col)),
    showcase = icon(icon),
    theme = theme,
    p("B.C. Total: " %,% labeller(filter(data[[1]], REGION_NAME == 'British Columbia', REF_YEAR == last_year) |> pull(!!col)))
  )
}

# Returns the 2x3 grid of value boxes
make_regional_profile_boxes = function(df, df_info, tooltips) {
  div(
    layout_column_wrap(width=1/3, fill = F,
      !!!map(1:3, function(i) make_value_box(df, title = to_sentence_case(df_info$col[i]), col = df_info$col[i], labeller = df_info$label[[i]], theme = df_info$theme[i], icon = df_info$icon[i], tooltips[[df_info$col[i]]]))
    ),
    layout_column_wrap(width=1/3, fill = F,
      !!!map(4:6, function(i) make_value_box(df, title = to_sentence_case(df_info$col[i]), col = df_info$col[i], labeller =  df_info$label[[i]], theme = df_info$theme[i], icon = df_info$icon[i], tooltips[[df_info$col[i]]]))
    )
  )
}
