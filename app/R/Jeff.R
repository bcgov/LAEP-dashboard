# This file is gonna house the code that Jeff (or whoever the subject expert is) fills in to render the final outputs of the dashboard. The idea is to isolate the step from going from the input chosen by the user (via all the drop down menus, etc.) to the output (such as a table or graph). So all that's needed is a function that achieves this and the data scientist can handle all the other stuff in the app. Also note that I have provided "toy" data frames so you can write these functions outside the Shiny app. I am hoping that it should be sufficient for the function to work successfully here with the correct toy data frame.

# just run this bit every time to get the toy dfs
is_local = Sys.getenv('SHINY_PORT') == ""
if (is_local) {
  source(paste0(here::here(), ifelse(is_local, '/app', ''), "/R/functions.R"), local = T)
  source(paste0(here::here(), ifelse(is_local, '/app', ''), "/R/global.R"), local = T)
  toy_region = "Cariboo"
  toy_df = data[[1]] |> filter(REGION_NAME == toy_region) |> select(REF_YEAR, !!!regional_profile_info$col)
  toy_df2 = data[[2]] |>
    filter(GEO_TYPE == "RD", REGION_NAME == toy_region) |>
    select(-matches("_TOTAL$")) |>
    select(REF_YEAR, TOTAL:last_col()) |>
    mutate(across(everything(), as.integer))
  toy_df2 = t2(toy_df2, "REF_YEAR", "Industry")

}


# These are the functions that I require the SME to write

# Very much WIP, but this function returns a bslib card used for one of those "LAEP Scenarios". Note that all the objects in the card have the `_i` suffix so they can be updated.
laep_scenario_card = function(i) {
  card(
    id = "laep_card_" %,% i,
    full_screen = T,
    card_title("Scenario #" %,% i),
    layout_column_wrap(width=1/4, fill = F,
      pickerInput("laep_year_" %,% i, label = "Select Reference Year", choices = years),
      pickerInput("laep_area_" %,% i, label = "Select Region", choices = RDs),
      pickerInput("laep_industry_" %,% i, label = "Select Industry", choices = industries),
      pickerInput("laep_social_safety_" %,% i, label = "Social Safety Net?", choices = c(T, F), selected = sample(c(T,F), 1))
    ),
    reactableOutput("laep_table_" %,% i),
    div(style = 'max-width: 300px;', actionBttn("delete_laep_" %,% i, "Delete this Scenario", color = 'danger'))
  )
}



# The bare bones of a LAEP scenario output table. I have no idea what to do here. Go nuts.
make_laep_scenario_table_output = function(year, region, industry, social_safety = T) {
  data[[2]] |>
    filter(REF_YEAR == year) |>
    filter(REGION_NAME == region) |>
    mutate(SOCIAL_SAFETY = social_safety) |>
    select(REF_YEAR, REGION_NAME, to_screaming_snake_case(!!industry), SOCIAL_SAFETY)
}

# My weak sauce attempt at the 'shift share' table. This is incomplete.
make_shift_share_table_output = function(df = toy_df2, yrs = years[1:2]) {
  df = df |>
    select(Industry, !!as.character(yrs))
  df$change = pull(df[,3]) - pull(df[,2])

  bind_rows(
    filter(df, Industry == "Total") |> mutate(group = "Total"),
    filter(df, Industry != "Total") |> arrange(names(df[3])) |> head(5) |> mutate(group = "Best"),
    filter(df, Industry != "Total") |> arrange(names(df)[3]) |> tail(5) |> mutate(group = "Worst")
  ) |>
    relocate(group, .before=1)
}






# The following functions, in my humble opinion, work well and don't require anything else




# this function makes the summary table on Regional Profile Page. I believe it is complete as is.
make_summary_table_output = function(df=toy_df) {
  x = df |> select(REF_YEAR, !!!regional_profile_info$col)
  for (i in (1:length(regional_profile_info$col))) {
    x[,i+1] = regional_profile_info$label[[i]]()(pull(x[,i+1]))
  }
  x = x |>
    t2("REF_YEAR", "Variable")
  return(x)
}

# make_summary_table_output(toy_df)


# This guy returns a list of plots for the summary graph.
make_summary_graph_output = function(df = toy_df, cols = regional_profile_info$col) {
  cols = to_screaming_snake_case(cols)
  labellers = regional_profile_info$label[match(cols, regional_profile_info$col)]

  x = map2(cols, labellers, function(var, labeller) {
    df |>
      select(REF_YEAR, !!var) |>
      janitor::clean_names(case='sentence') |>
      pivot_longer(cols = 2:last_col()) |>
      mutate(year = as.factor(`Ref year`)) |>
      ggplot(aes(y=value, x=year, fill=year)) +
      geom_col_interactive(aes(tooltip = year %,% ":" %,,% labeller()(value), data_id = year), position = 'dodge', hover_nearest = T) +
      ggthemes::theme_clean() +
      theme(legend.position = 'bottom') +
      scale_fill_viridis_d() +
      labs(x=NULL, y=NULL, fill=NULL) +
      guides(fill='none') +
      scale_y_continuous(labels = labeller()) +
      facet_wrap(~name)
  })

  return(x)
}

make_summary_map_output = function(region = toy_region) {
  df = RDs_sf |>
    filter(REF_YEAR == last_year) |>
    select(REGION_NAME, geometry, !!!regional_profile_info$col) |>
    mutate(is_chosen = REGION_NAME == region)

  centroid = st_centroid(filter(df, REGION_NAME == region) |> pull(geometry)) |> st_coordinates() |> unique()

  df |>
    leaflet() |>
    setView(lng = centroid[1], lat = centroid[2], zoom = 6) |>
    addTiles() |>
    addPolygons(
      fillColor = topo.colors(10, alpha = NULL),
      stroke = T,
      weight = ~ifelse(is_chosen, 5, 1),
      highlightOptions = highlightOptions(
        weight = 5,
        color = "#666",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = summary_map_labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")
    )
}

make_industry_table = function(df = toy_df2) {
  df |>
    mutate(x = df[, ncol(df)]) |>
    arrange(desc(x)) |>
    select(-x) |>
    filter(Industry != "Total") |>
    head(5) |>
    mutate(across(where(is.numeric), ~label_comma()(.)))
}
