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

## load libraries
pacman::p_load(shiny, bslib, plotly, tidyverse, htmltools,
               reactable, shinyjs, shinyWidgets, fs, janitor,
               snakecase, leaflet, bcmaps, rmapshaper, sf, scales,
               openxlsx2, ggiraph, patchwork, shinycssloaders,
               bcsapps, bcstatslinks)


# make the info for popover icons
info_icon = function(tooltip, color = NULL) {
  if(!is.null(color)) { style = paste0("color:", color) }
  else { style = "" }
  popover(icon("info-circle", style=style), tooltip)
}

# function for cleaning region names
## necessary for the region names from bcmaps to match the data
clean_regions = function(x) {
  x |>
    str_replace("Regional District( of)?", "") |>
    str_replace("Region", "") |>
   # str_replace("RD", "") |>
    str_replace(" - ", "-") |>
    str_replace("\\((Unincorporated)\\)", "") |>
    str_replace("qathet", "Powell River") |>
    str_replace("North Coast", "Skeena-Queen Charlotte") |>
    str_replace("Columbia Shuswap", "Columbia-Shuswap") |>
    str_squish()
}

# make a custom value box for the first row of the Regional Profile page
make_value_box = function(df, tooltip = NULL) {
  bslib::card(
    class = "bcs_vb",
    card_body(
      class = "bcs_vb",
      if(is.null(tooltip)) { span(icon(df$ICON), df$VARIABLE) }
      else { span(icon(df$ICON), df$VARIABLE, info_icon(tooltip)) },
      h4(df$FORMATTED_VALUE)
    )
  )
}

make_summary_table_output = function(df=toy_df) {

  df |> select(REF_YEAR, VARIABLE, VALUE, FORMATTED_VALUE) |>
    group_by(VARIABLE) |>
    mutate(change = round_half_up(VALUE/lag(VALUE) -1, digits = 3),
           dir = case_when(change > 0 ~ "▲",
                           change < 0 ~ "▼",
                           TRUE ~ "")) |>
    ungroup() |>
    mutate(final = case_when(is.na(change) ~ FORMATTED_VALUE,
                             TRUE ~  paste0(FORMATTED_VALUE, "<br>(", dir, label_percent()(abs(change)),  ")"))) |>
    select(REF_YEAR, Variable = VARIABLE, final) |>
    pivot_wider(names_from = "REF_YEAR", values_from = "final")

}

nicetable = function(df, ...) {reactable(df, ..., striped = TRUE, highlight = T)}
