
bcs_vb_theme <- value_box_theme(bg = "#D8EAFD", fg = "#2D2D2D")

# Define UI for application that draws a histogram
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
               padding = c(10,24),
               gap = 0,
               open = list(desktop = "open", mobile = "closed"),
               title = "Economic Profile for",
               list(
                 div(
                   id = "choose_region_div",
                   h4(class = "sidebar-region", textOutput("profile_heading")),
                   radioGroupButtons("choose_level",
                                     choices = c("Regional Districts" = "RD", "Local Areas" = "LA"),
                                     selected = "RD", direction = "vertical",
                                     justified = TRUE,
                                     width = "100%"),
                   pickerInput("choose_RD",
                               "Choose Regional District",
                               choices = rd_la_lookup |> distinct(RD) |> pull(),
                               selected = "All regional districts"),
                   pickerInput("choose_LA",
                               "Choose Local Area",
                               choices = c("All local areas", rd_la_lookup |> distinct(LA) |> pull()),
                               selected = "All local areas"),
                   pickerInput(
                     "choose_topic",
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
                            income source related to a private sector industry.'))),
                   actionButton("reset_map",
                                label = "Reset map view",
                                icon = icon("arrow-rotate-right")),
                   div(style = "margin-top:25px",
                       strong("Resources:"),
                       br(),
                       a("LAEP main page", href="https://www2.gov.bc.ca/gov/content/data/statistics/economy/input-output-model#profiles"),
                                              br(),
                       a("LAEP toolkit", href="https://www2.gov.bc.ca/assets/gov/data/statistics/economy/input-output-model/local_area_economic_profiles_2023_toolkit.xlsx"),
                       br(),
                       a("FAQs", href = "")),
                   div(class = "small-body",
                       style = "margin-top:25px",
                       HTML("Last updated:", last_updated))
                   ) ## end of choose_region_div
                 ) ## end of list
               ), ## end of sidebar

             # main panel ----
             div(
               id = "Regional Profile",
               tags$span(class = "mobile-title", h4("LAEP for: ", textOutput("profile_heading_mobile", inline = TRUE)) ),
               uiOutput("regional_profile_row1"), ## value boxes

               ## first row (map and summary table)
               layout_column_wrap(
                 width = 1/2,
                 card(full_screen = TRUE,
                      card_header(
                        div(
                          style = "display: flex; align-items: center; gap: 1rem;",
                          tags$label("Select Year:"),
                          radioButtons(
                            inputId = "selected_year",
                            label = NULL,
                            choices = c("2010", "2015", "2020"),
                            selected = "2020",
                            inline = TRUE))),
                      card_body(leafletOutput("map"))),
                 card(full_screen = TRUE,
                      card_header(uiOutput("summary_table_header")),
                      card_body(reactableOutput("summary_table")))),

               ## second row (three tables)
               layout_columns(
                 ## left column (income shares table)
                 card(full_screen = TRUE,
                      card_header(uiOutput("income_shares_header")),
                      card_body(reactableOutput("income_shares"))),

                 ## right column (top 5 jobs and top 5 lq tables)
                 layout_columns(
                   card(full_screen = TRUE,
                        card_header(uiOutput("top_5_jobs_header")),
                        card_body(reactableOutput("top_5_jobs"))),
                   card(full_screen = TRUE,
                        card_header(uiOutput("top_5_lqs_header")),
                        card_body(reactableOutput("top_5_lqs"))),
                   col_widths = c(12,12)))
               ) ## end of main panel div
             ) ## end of page sidebar
           ), ## end of column

    bcsapps::bcsFooterUI(id = 'footer')
    ) ## end of taglist
}

# Define server logic required to draw a histogram
server <- function(input, output) {

  # reactive values ----

  ## selected region ----
  ## set the initial selected region to British Columbia
  selected_region <- reactiveVal("British Columbia")


  # observe events ----

  ## selected region ----
  observeEvent(selected_region(), {

    ## if the selected region is British Columbia (i.e., reset map clicked)
    ##   update choose_RD selected value to all regional disticts
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
      # print(paste("mapclick LA:", selected_region()))
      new_rd <- rd_la_lookup |> filter(LA == selected_region() & RD != "All regional districts") |> pull(RD)

      updatePickerInput(
        inputId = "choose_RD",
        selected = new_rd)

      updatePickerInput(
        inputId = "choose_LA",
        choices = c("All local areas", rd_la_lookup |> filter(RD == new_rd) |> pull(LA)),
        selected = selected_region())
    }
  })


  ## rd dropdown ----
  observeEvent(input$choose_RD, {

    ## if the choose_RD drop down is changed, update the selected region
    ## if choose_RD = All RDs -> BC
    ## else if choose_RD != All RDs, but choose_LA = All LAs -> input$choose_RD
    ## else -> don't update the selected_region, as the value changed due to a change in the selected LA
    if(input$choose_RD == "All regional districts") {
      selected_region("British Columbia")
    } else if (input$choose_LA == "All local areas") {
      selected_region(input$choose_RD)
    } else {
      ## do nothing
    }
  })


  ## la dropdown ----
  observeEvent(input$choose_LA, {

    ## if the choose_LA drop down is changed, update the selected region
    ## if choose_RD = All RDs -> BC
    ## else if choose_RD != All RDs, but choose_LA = All LAs -> input$choose_RD
    ## else -> input$choose_LA
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


  ## reset map ----
  observeEvent(input$reset_map, {

    # update selected region
    selected_region("British Columbia")

    # update rd dropdown
    updatePickerInput(
      inputId = "choose_RD",
      selected = "All regional districts")

    ## update la dropdown#
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


  ## summary boxes ----
  output$regional_profile_row1 = renderUI({

    df <- data[["Descriptive Stats"]] |> filter(REGION_NAME == selected_region(), REF_YEAR == 2020)

    div(
      layout_column_wrap(width=1/5,
                         fill = T,
                         !!!map(df$VARIABLE,
                                ~make_value_box(df |> filter(VARIABLE == .x),
                                                tooltips[[.x]]))
      ))
  })

  ## map ----
  output$map = renderLeaflet({

    req(input$choose_level, input$choose_RD, input$choose_LA, input$choose_topic)

    ## determine which stats to map
    if (input$choose_topic == "Population") {
      df <- data[["Descriptive Stats"]] |> filter(REF_YEAR == input$selected_year, VARIABLE == "Population")

    } else if(input$choose_topic == "Diversity index") {
      df <- data[["Descriptive Stats"]] |> filter(REF_YEAR == input$selected_year, VARIABLE == "Diversity index")

    } else if(input$choose_topic == "Basic income shares") {
      df <- data[["Income Shares Map"]] |> filter(REF_YEAR == input$selected_year, VARIABLE == input$choose_industry)

    } else {
      df <- data[["Dominant Income Sources"]] |> filter(REF_YEAR == input$selected_year, VARIABLE == input$choose_source)
    }

    ## make the map
    make_map(input$choose_level,
             input$choose_RD,
             input$choose_LA,
             stat_data = df)
    })


  ## summary table ----
  output$summary_table_header <- renderUI({

    span(info_icon(tooltips$summary_statistics),
         "Summary statistics for: ",
         selected_region())
  })

  output$summary_table <- renderReactable({

    table <- data[["Descriptive Stats"]] |>
      filter(REGION_NAME == selected_region()) |>
      make_summary_table_output()

    reactable(
      table,
      sortable = FALSE,
      striped = TRUE,
      theme = reactableTheme(
        stripedColor = "#f6f9fc",
      ),
      defaultColDef = colDef(html = TRUE),
      columns = list(
        Variable = colDef(align = "left", minWidth = 200), ## use minWidth as it will scale proportionally
        `2010` = colDef(align = "right", minWidth = 100),
        `2015` = colDef(align = "right", minWidth = 100),
        `2020` = colDef(align = "right", minWidth = 100))
    )
  })


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


  ## location quotiens ----
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
