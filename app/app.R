# Copyright 2025 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


# Define UI
ui <- function(req) {
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "BC_Sans.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "variables.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$link(rel = "shortcut icon", href = "favicon.png"), ## to add BCGov favicon
      if(google_tracking){ includeHTML("www/google-analytics.html") },  ## to add GA tracking code
      bcsapps::bcsHeaderUI(id = 'header',
                           appname = tagList(
                             ## use a smaller title if viewed on mobile
                             tags$span(class = "desktop-title", "Local Area Economic Profiles - Interactive Map"),
                             tags$span(class = "mobile-title", "LAEP")
                           ),
                           github = "https://github.com/bcgov/LAEP-dashboard")
      ), ## end of tags$head

    column(width = 12, style = "margin-top:100px",

           page_sidebar(
             full_screen = TRUE,
             window_title = "Local Area Economic Profiles", ## browser tab name

             # sidebar ----
             sidebar = sidebar(
               gap = 0,
               open = list(desktop = "open", mobile = "closed"), ## start with the sidebar closed if viewing on mobile
               list(
                 div(
                   id = "sidebar_div",
                   strong("Filters:"),

                   div(id = "boundary_type",
                       class = "sidebar_filters",
                       pickerInput("choose_level",
                                   "Choose Boundary Type",
                                   choices = c("Regional Districts" = "RD", "Local Areas" = "LA"),
                                   selected = "RD")),

                   div(id = "region",
                       class = "sidebar_filters",
                       pickerInput("choose_RD",
                                   "Choose Regional District",
                                   choices = rd_la_lookup |> distinct(RD) |> pull(),
                                   selected = "All regional districts"),
                       pickerInput("choose_LA",
                                   "Choose Local Area",
                                   choices = c("All local areas", rd_la_lookup |> distinct(LA) |> pull()),
                                   selected = "All local areas"),
                       actionButton("reset_map",
                                    label = "Reset map view to B.C.",
                                    icon = icon("arrow-rotate-right"))),

                   div(id = "topic",
                       class = "sidebar_filters",
                       pickerInput("choose_topic",
                                   "Choose Map Topic",
                                   choices = c("Population", "Diversity index", "Basic income shares", "Dominant basic income sources"),
                                   selected = "Population"),
                       conditionalPanel(
                         condition = "input.choose_topic == 'Basic income shares'",
                         pickerInput("choose_industry",
                                     "Choose Income Source:",
                                     choices = data[["Income Shares Map"]] |> pull(VARIABLE) |> unique())),
                       conditionalPanel(
                         condition = "input.choose_topic == 'Dominant basic income sources'",
                         tooltip(
                           id = "popover",
                           placement = "right",
                           radioGroupButtons("choose_source",
                                             choices = c("All basic income sources" = "Dominant basic income source",
                                                         "Private sector sources only" = "Dominant private sector basic income source"),
                                             selected = "Dominant basic income source",
                                             direction = "vertical",
                                             justified = TRUE,
                                             width = "100%"),
                           HTML('The dominant basic income source is the largest source of external income flowing into the region.
                                 Choose "All basic income sources" to show the overall dominant source for
                                 each region, and "Private sector sources only" to find the most important basic
                                 income source related to a private sector industry.')))),

                   div(id = "resources",
                       style = "margin-top:25px",
                       strong("Resources:"),
                       br(),
                       a("LAEP main page",
                         href="https://www2.gov.bc.ca/gov/content/data/statistics/economy/input-output-model#profiles"),
                       br(),
                       a("LAEP toolkit",
                         href="https://www2.gov.bc.ca/assets/gov/data/statistics/economy/input-output-model/local_area_economic_profiles_2023_toolkit.xlsx"),
                       br(),
                       a("FAQs", href = "")),

                   div(id = "feedback",
                       style = "margin-top:25px",
                       strong("Feedback:"),
                       br(),
                       a("Let us know how we can improve",
                         href = "https://dpdd.atlassian.net/servicedesk/customer/portal/12")
                       ),

                   div(id = "update_date",
                       class = "small-body",
                       style = "margin-top:25px",
                       HTML("Last updated:", last_updated))

                   ) ## end of sidebar_div
                 ) ## end of list
               ), ## end of sidebar

             # main panel ----
             div(
               id = "main_panel_div",

               ## intro
               h4("Economic Profile for", textOutput("profile_heading", inline = TRUE)),
               span(HTML("This dashboard provides economic insights for regional districts and local areas across British Columbia.
                         Use the dropdown menus or interactive map to explore economic diversity and income dependencies
                         in your local community. Selecting a region — either from the dropdowns or the map — will automatically
                         update the dashboard with relevant data for that area.<br>
                         For deeper insights, check out the Local Area Economic Profiles' main page on the <a href='https://www2.gov.bc.ca/gov/content/data/statistics/economy/input-output-model#profiles'>BC Stats website.</a>")),
                br(),br(),

               ## first row (map and summary table)
               layout_column_wrap(
                 width = 1/2,
                 card(full_screen = TRUE,
                      card_header(
                        div(
                          style = "display: flex; align-items: center; gap: 1rem;",
                          tags$label("Select Year:"),
                          radioGroupButtons(
                            inputId = "selected_year",
                            label = NULL,
                            choices = c("2010", "2015", "2020"),
                            selected = "2020"))),
                      card_body(leafletOutput("map"))),
                 card(full_screen = TRUE,
                      card_header(uiOutput("summary_table_header")),
                      card_body(reactableOutput("summary_table")))),

               ## second row (three tables)
               accordion(
                 open = FALSE,
                 accordion_panel(
                   title = uiOutput("income_shares_header"),
                   value = "income_share_card",
                   reactableOutput("income_shares")),
                 accordion_panel(
                   title =uiOutput("top_5_jobs_header"),
                   value = "top_5_jobs_card",
                   reactableOutput("top_5_jobs")),
                 accordion_panel(
                   title =uiOutput("top_5_lqs_header"),
                   value = "top_5_lq_card",
                   reactableOutput("top_5_lqs")))

               ) ## end of main_panel_div
             ) ## end of page sidebar
           ), ## end of column

    bcsapps::bcsFooterUI(id = 'footer')
    ) ## end of taglist
}

# Define server logic
server <- function(input, output) {

  # reactive values ----

  ## selected region ----
  ## set the initial selected region to British Columbia
  selected_region <- reactiveVal("British Columbia")

  ## ignore_choose_rd ----
  ## reactive value for tracking when not to update selected_region via choose_RD
  ignore_choose_rd <- reactiveVal(FALSE)


  # observe events ----

  ## selected region ----
  observeEvent(selected_region(), {

    ## if the selected region is British Columbia (i.e., reset map clicked)
    ##   update choose_RD selected value to all regional districts
    ##   update choose_LA selected value to all local areas, plus set choices to all available LAs
    if(selected_region() == "British Columbia") {
      updatePickerInput(
        inputId = "choose_RD",
        selected = "All regional districts")

      updatePickerInput(
        inputId = "choose_LA",
        choices = c("All local areas", rd_la_lookup |> pull(LA)),
        selected = "All local areas")
    }

    ## if the selected region is a regional district
    ##  update the choose_RD selected value to the selected region
    ##  update the choose_LA choices to the LAs available in that RD, plus set the choose_LA selected value to "All"
    if(selected_region() %in% rd_la_lookup$RD) {
      updatePickerInput(
        inputId = "choose_RD",
        selected = selected_region())

      updatePickerInput(
        inputId = "choose_LA",
        choices = c("All local areas", rd_la_lookup |> filter(RD == selected_region()) |> pull(LA)),
        selected = "All local areas")
    }

    ## if the selected region is a local area
    ## update the choose_RD selected value to the RD which contains the LA
    ## update the choose_La choices to the LAs available in the associated RD, plus set the choose_LA selected to the selected region
    if(selected_region() %in% rd_la_lookup$LA) {

      new_rd <- rd_la_lookup |> filter(LA == selected_region() & RD != "All regional districts") |> pull(RD)

      if(new_rd != input$choose_RD) {
      ignore_choose_rd(TRUE) ## set to true so that updating choose_RD won't update selected_region

      updatePickerInput(
        inputId = "choose_RD",
        selected = new_rd)
      }

      updatePickerInput(
        inputId = "choose_LA",
        choices = c("All local areas", rd_la_lookup |> filter(RD == new_rd) |> pull(LA)),
        selected = selected_region())
    }
  })


  ## rd dropdown ----
  observeEvent(input$choose_RD, {

    ## if the choose_RD drop down is changed, update the selected region
    ## if ignore_chose_rd = TRUE set to false and do nothing more
    ## else if choose_RD = All RDs -> selected_region = BC
    ## else -> selected_region = choose_RD
    if (ignore_choose_rd()) {
      ignore_choose_rd(FALSE) ## reset to FALSE
    } else if(input$choose_RD == "All regional districts") {
      selected_region("British Columbia")
    } else {
      selected_region(input$choose_RD)
    }
  })


  ## la dropdown ----
  observeEvent(input$choose_LA, {

    ## if the choose_LA drop down is changed, update the selected region
    ## if choose_RD = All RDs -> selected_region = BC
    ## else if choose_RD != All RDs, but choose_LA = All LAs -> selected_region = choose_RD
    ## else -> selected_region = choose_LA
    if(input$choose_RD == "All regional districts" & input$choose_LA == "All local areas") {
      selected_region("British Columbia")
    } else if (input$choose_LA == "All local areas") {
      selected_region(input$choose_RD)
    } else {
      selected_region(input$choose_LA)
    }
  })


  ## map click ----
  observeEvent(input$map_shape_click, {

    # update the selected region to the region clicked on in the map
    selected_region(input$map_shape_click$id)
  })


  ## reset map button ----
  observeEvent(input$reset_map, {

    # update selected region
    selected_region("British Columbia")

    # update rd dropdown
    updatePickerInput(
      inputId = "choose_RD",
      selected = "All regional districts")

    ## update la dropdown
    updatePickerInput(
      inputId = "choose_LA",
      selected = "All local areas")

    ## reset map zoom
    bbox <- st_bbox(map_rds)
    leafletProxy("map") |>
      fitBounds(lng1 = bbox$xmin[[1]],
                lat1 = bbox$ymin[[1]],
                lng2 = bbox$xmax[[1]],
                lat2 = bbox$ymax[[1]])
  })


  # outputs ----

  ## profile header ----
  output$profile_heading <- output$profile_heading_mobile <- renderText({
    selected_region()
  })


  ## map ----
  output$map = renderLeaflet({

    req(input$choose_level, input$choose_RD, input$choose_LA, input$choose_topic, input$selected_year)

    ## determine which stats to map
    # Check if current inputs match the default config
    is_default <- identical(input$choose_level, default_map_inputs$choose_level) &&
      identical(input$choose_RD, default_map_inputs$choose_RD) &&
      (is.null(input$choose_LA) || input$choose_LA == "") &&
      identical(input$choose_topic, default_map_inputs$choose_topic) &&
      identical(input$selected_year, default_map_inputs$selected_year)

    df <- if (is_default) {
      default_map_data

    } else if (input$choose_topic == "Population") {
      data[["Descriptive Stats"]] |> filter(REF_YEAR == input$selected_year, VARIABLE == "Population")

    } else if(input$choose_topic == "Diversity index") {
      data[["Descriptive Stats"]] |> filter(REF_YEAR == input$selected_year, VARIABLE == "Diversity index")

    } else if(input$choose_topic == "Basic income shares") {
      req(input$choose_industry)
      data[["Income Shares Map"]] |> filter(REF_YEAR == input$selected_year, VARIABLE == input$choose_industry)

    } else {
      req(input$choose_source)
      data[["Dominant Income Sources"]] |> filter(REF_YEAR == input$selected_year, VARIABLE == input$choose_source)
    }

    ## make the map
    make_map(input$choose_level,
             input$choose_RD,
             input$choose_LA,
             stat_data = df)
    }) |>
    bindCache(input$choose_level,
              input$choose_RD,
              input$choose_LA,
              input$choose_topic,
              input$selected_year,
              input$choose_industry,
              input$choose_source)


  ## summary table ----
  output$summary_table_header <- renderUI({

    span(info_icon(tooltips$summary_statistics),
         "Summary statistics for: ",
         selected_region())
  })

  output$summary_table <- renderReactable({

    req(selected_region())

    table <- if (selected_region() == "British Columbia") {
      default_table_data
    } else {
      data[["Descriptive Stats"]] |>
        filter(REGION_NAME == selected_region()) |>
        make_summary_table_output()
    }

    reactable(
      table,
      sortable = FALSE,
      striped = TRUE,
      theme = reactableTheme(
        stripedColor = "#f6f9fc",
      ),
      defaultColDef = colDef(html = TRUE),
      columns = list(
        Variable = colDef(align = "left", minWidth = 200,## use minWidth as it will scale proportionally
                          cell = function(value) {  ## use tippy package to add tooltips
                            div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
                                tippy(value, tooltip = tooltips[value], theme = "light"))
                          }),
        `2010` = colDef(align = "right", minWidth = 100),
        `2015` = colDef(align = "right", minWidth = 100),
        `2020` = colDef(align = "right", minWidth = 100))
    )
  }) |>
    bindCache(selected_region())


  ## income shares ----
  output$income_shares_header <- renderUI({

    span(info_icon(tooltips$income_shares),
         "Basic income shares for: ",
         selected_region())
  })

  output$income_shares <- renderReactable({

    ## separate into two tables to be able to nest the sub-sectors
    table_outer <- data[["Income Shares Table"]] |>
      filter(REGION_NAME == selected_region()) |>
      filter(TOTAL_COLUMN) |>
      select(REF_YEAR, TABLE_ORDER, VARIABLE, R = VALUE, `F` = FORMATTED_VALUE) |>
      pivot_wider(names_from = "REF_YEAR", values_from = c("R", "F"))  |>
      mutate(across(starts_with("F"), ~ifelse(is.na(.x), "", .x)))

    table_inner <- data[["Income Shares Table"]] |>
      filter(REGION_NAME == selected_region()) |>
      filter(!TOTAL_COLUMN) |>
      select(REF_YEAR, TABLE_ORDER, PARENT_VARIABLE, VARIABLE, FORMATTED_VALUE) |>
      pivot_wider(names_from = "REF_YEAR", values_from = "FORMATTED_VALUE", names_prefix = "F_") |>
      mutate(across(starts_with("F"), ~ifelse(is.na(.x), "", .x)))

    reactable(table_outer,
              defaultSorted = "TABLE_ORDER",
              striped = TRUE,
              pagination = FALSE,
              theme = reactableTheme(
                stripedColor = "#f6f9fc",
              ),
              ## for correct sort order for factors and formatted values,
              ## use numeric columns in the table but have the display value come from the formatted columns
              ## from https://github.com/glin/reactable/issues/202#issuecomment-2689390142
              columns = list(
                TABLE_ORDER = colDef(
                  name = "Basic income source",
                  align = "left",
                  minWidth = 190,
                  cell = function(value, index) table_outer$VARIABLE[index]
                ),
                R_2010 = colDef(name = "2010", align = "right", minWidth = 67, defaultSortOrder = "desc", sortNALast = TRUE, cell = function(value, index) table_outer$F_2010[index]),
                R_2015 = colDef(name = "2015", align = "right", minWidth = 67, defaultSortOrder = "desc", sortNALast = TRUE, cell = function(value, index) table_outer$F_2015[index]),
                R_2020 = colDef(name = "2020", align = "right", minWidth = 67, defaultSortOrder = "desc", sortNALast = TRUE, cell = function(value, index) table_outer$F_2020[index]),
                ## hide the formatted columns, as their values are displayed in their associated numeric column
                VARIABLE = colDef(show=F),
                F_2010 = colDef(show = F),
                F_2015 = colDef(show = F),
                F_2020 = colDef(show = F)
              ),
              ## details for nested tables
              details = function(index) {
                subsectors <- table_inner|> filter(PARENT_VARIABLE == as.character(table_outer$VARIABLE[index]))
                if(nrow(subsectors)>0) {
                  reactable(subsectors,
                            sortable = FALSE,
                            borderless = TRUE,
                            theme = reactableTheme(
                              headerStyle = list(display = "none"),  # hide headers in the nested tables
                              style = list("backgroundColor" = "#F3F2F1")
                            ),
                            columns = list(
                              TABLE_ORDER = colDef(show = F),
                              PARENT_VARIABLE = colDef(show = F),
                              ## use padding to indent the first column of the nested table
                              ## adjust the minWidth so the subsequent columns align correctly
                              VARIABLE = colDef(style = list(paddingLeft = "60px"), minWidth = 230),
                              F_2010 = colDef(align = "right", minWidth = 67),
                              F_2015 = colDef(align = "right", minWidth = 67),
                              F_2020 = colDef(align = "right", minWidth = 67)))
                }}) ## end of reactable
  })


  ## jobs ----
  output$top_5_jobs_header <- renderUI({

    span(info_icon(tooltips$top_employment),
         "Top industries by number of jobs for: ",
         selected_region())
  })

  output$top_5_jobs <- renderReactable({

    table <- data[["Jobs"]] |>
      filter(REGION_NAME == selected_region()) |>
      select(REF_YEAR, Industry = VARIABLE, R = VALUE, `F` = FORMATTED_VALUE) |>
      pivot_wider(names_from = "REF_YEAR", values_from = c("R", "F")) |>
      mutate(across(starts_with("F"), ~ifelse(is.na(.x), "", .x)))

    if(nrow(table) == 0) {
      table <- data.frame("Industry" = "This region falls entirely within First Nations boundaries. The data is unavailable as BC Stats did not receive consent to release it.",
                          "R_2010" = "",
                          "R_2015" = "",
                          "R_2020" = "",
                          "F_2010" = "",
                          "F_2015" = "",
                          "F_2020" = "")
    }

    reactable(
      table,
      defaultSorted = "R_2020",
      striped = TRUE,
      theme = reactableTheme(
        stripedColor = "#f6f9fc",
      ),
      defaultColDef = colDef(html = TRUE, sortNALast = TRUE, defaultSortOrder = "desc", align = "right", minWidth = 60),
      columns = list(
        Industry = colDef(align = "left", minWidth = 200),
        R_2010 = colDef(name = "2010", cell = function(value, index) table$F_2010[index]),
        R_2015 = colDef(name = "2015", cell = function(value, index) table$F_2015[index]),
        R_2020 = colDef(name = "2020", cell = function(value, index) table$F_2020[index]),
        F_2010 = colDef(show = F),
        F_2015 = colDef(show = F),
        F_2020 = colDef(show = F)))
  })


  ## location quotients ----
  output$top_5_lqs_header <- renderUI({

    span(info_icon(tooltips$top_lq),
         "Top industries by location quotient for: ",
         selected_region())
  })

  output$top_5_lqs <- renderReactable({

    table <- data[["Location Quotients"]] |>
      filter(REGION_NAME == selected_region()) |>
      select(REF_YEAR, Industry = VARIABLE, R = VALUE, `F` = FORMATTED_VALUE) |>
      pivot_wider(names_from = "REF_YEAR", values_from = c("R", "F")) |>
      mutate(across(starts_with("F"), ~ifelse(is.na(.x), "", .x)))

    if(nrow(table) == 0) {
      table <- data.frame("Industry" = "This region falls entirely within First Nations boundaries. The data is unavailable as BC Stats did not receive consent to release it.",
                          "R_2010" = "",
                          "R_2015" = "",
                          "R_2020" = "",
                          "F_2010" = "",
                          "F_2015" = "",
                          "F_2020" = "")
    }

    reactable(
      table,
      defaultSorted = "R_2020",
      striped = TRUE,
      theme = reactableTheme(
        stripedColor = "#f6f9fc",
      ),
      defaultColDef = colDef(html = TRUE, sortNALast = TRUE, defaultSortOrder = "desc", align = "right", minWidth = 60, ),
      columns = list(
        Industry = colDef(align = "left", minWidth = 200),
        R_2010 = colDef(name = "2010", cell = function(value, index) table$F_2010[index]),
        R_2015 = colDef(name = "2015", cell = function(value, index) table$F_2015[index]),
        R_2020 = colDef(name = "2020", cell = function(value, index) table$F_2020[index]),
        F_2010 = colDef(show = F),
        F_2015 = colDef(show = F),
        F_2020 = colDef(show = F)))
  })


  bcsapps::bcsHeaderServer(id = 'header', links = T)
  bcsapps::bcsFooterServer(id = 'footer')
}

# Run the application
shinyApp(ui = ui, server = server)
