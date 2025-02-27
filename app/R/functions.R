
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

make_infobox = function(df, title = 'Population', col = NULL, formatter = scales::label_comma, color=NULL, icon='map') {
  if (is.null(color)) color = sample(c('red', 'yellow', 'aqua', 'blue', 'light-blue', 'green', 'navy', 'teal', 'olive', 'lime', 'orange', 'fuchsia', 'purple', 'maroon', 'black'), 1)
  if (is.null(col)) col = snakecase::to_screaming_snake_case(title)
  infoBox(title, value = formatter()(filter(df, REF_YEAR == last_year) |> pull(!!col)), subtitle = "B.C. Total: " %,% formatter()(filter(data[[1]], REGION_NAME == 'British Columbia', REF_YEAR == last_year) |> pull(!!col)), color = color, icon = icon(icon), fill = T)
}

tooltip_text = function(text="text", tooltip_text = "This is a tooltip yo", icon_name = "info-circle", icon_style = 'color: red;') span(text, tippy::tippy(icon(icon_name, style=icon_style), tooltip_text))
