
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
    full_screen = T,
    card_title("Scenario #" %,% i),
    layout_column_wrap(width=1/4, fill = F,
      pickerInput("year", label = "Select Reference Year", choices = years),
      pickerInput("area", label = "Select Region", choices = regions),
      pickerInput("industry", label = "Select Industry", choices = industries),
      pickerInput("SSN", label = "Social Safety Net?", choices = c("Yes", "No"))
    ),
    reactableOutput("laep" %,% i),
    layout_column_wrap(width=1/2, fill = F,
      actionBttn("add_laep" %,% i, "Add a Scenario"),
      actionBttn("delete_laep" %,% i, "Delete this Scenario")
    )
  )
}
