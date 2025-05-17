
library(shiny)

bcs_vb_theme <- value_box_theme(bg = "#D8EAFD", fg = "#2D2D2D")

# Define UI for application that draws a histogram
ui <- function(req) {
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "fonts.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "variables.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$link(rel = "shortcut icon", href = "favicon.png"), ## to add BCGov favicon
      bcsapps::bcsHeaderUI(id = 'header', appname = "Local Area Economic Profiles", github = "https://github.com/bcgov/LAEP-dashboard")
    ),
    column(width = 12, style = "margin-top:100px",
           page_sidebar(
             full_screen = TRUE,
             window_title = "Local Area Economic Profiles", ## browser tab name
             sidebar = sidebar(title = "Regional Profile for",
                               list(
               div(id = "choose_region_div",
                   h4(class = "sidebar-region", textOutput("profile_heading")),
                   radioGroupButtons("choose_level", choices = c("Regional Districts" = "RD", "Local Areas" = "LA"), selected = "RD", direction = "vertical", justified = TRUE, width = "100%"),
                   pickerInput("choose_RD", "Choose Regional District", choices = rd_la_lookup |> distinct(RD) |> pull(), selected = "All regional districts", multiple = F),
                   pickerInput("choose_LA", "Choose Local Area", choices = "All local areas", multiple = F, selected = "All local areas"),
                   pickerInput("choose_topic", "Choose Topic",
                               choices = c("Basic statistics" = "POPULATION",
                                           "Diversity index" = "DIVERSITY_INDEX",
                                           "Income shares",
                                           "Income sources"

                                           # "Basic income sources" = "DOMINANT_BASIC_INCOME_SOURCE",
                                           # "Private sector basic income sources" = "DOMINANT_PRIVATE_SECTOR_EMPLOYMENT_BASIC_INCOME_SOURCE"
                                           ),
                               multiple = F, selected = "POPULATION"),
                   conditionalPanel(
                     condition = "input.choose_topic == 'Income shares'",
                     pickerInput("choose_industry", "Choose industry:", choices = c("Forestry", "Construction"))
                   ),
                   conditionalPanel(
                     condition = "input.choose_topic == 'Income sources'",
                     radioGroupButtons("choose_source", choices = c("Dominant basic source", "Dominant private sector source"), selected = "Dominant basic source", direction = "vertical", justified = TRUE, width = "100%")
                   ),
                   actionButton("reset_map", label = "Reset map", icon = icon("arrow-rotate-right"))


               )
             )
             ),

             div(id = "Regional Profile",
                 uiOutput("regional_profile_row1"),
                 layout_column_wrap(width = 1/2,
                                    card(full_screen = TRUE,
                                         card_body(leafletOutput("summary_map"#, height = "100%"
                                                                 ))),
                                    uiOutput("summary_card"))
                 #                    card(full_screen = TRUE,
                 #                         card_body(reactableOutput("summary_table")))),
                 # card("Top 5 industries by employment", reactableOutput("industry_table"))
             #    )
             )
)
),bcsapps::bcsFooterUI(id = 'footer')
)
}

# Define server logic required to draw a histogram
server <- function(input, output) {

  # reactive values ----
  selected_region <- reactiveVal("British Columbia")

  # observe events ----
  ## change to level (RD or LA) ----
  observeEvent(input$choose_level, {

    # if(input$choose_level == "RD") {
    #   updatePickerInput(
    #     inputId = "choose_LA",
    #     choices = c("All local areas"),
    #     selected = "All local areas")
    #
    #
    # } else {
    #
    #
    #   updatePickerInput(
    #     inputId = "choose_LA",
    #     choices = c("All local areas", rd_la_lookup |> filter(RD == input$choose_RD) |> pull(LA)),
    #     selected = isolate(input$choose_LA) ## do not update selected
    #   )
    # }

  })


  ## change to regional district ----
  observeEvent(input$choose_RD, {
    req(input$choose_RD)

    # Update local area dropdown
    new_LA_list <- rd_la_lookup |> filter(RD == input$choose_RD) |> pull(LA)

    ## check if selected LA is in the LA list linked to the selected RD
    ## and that the selection has not been reset (i.e., All regions)
    if(input$choose_RD == "All regional districts") {keep_selected <- FALSE}
    else if(!input$choose_LA %in% new_LA_list) {keep_selected <- FALSE}
    else {keep_selected <-  TRUE}

    if(keep_selected) {
      updatePickerInput(
        inputId = "choose_LA",
        choices = c("All local areas", rd_la_lookup |> filter(RD == input$choose_RD) |> pull(LA)),
        selected = isolate(input$choose_LA) ## do not update selected
      )
    } else {
      updatePickerInput(
        inputId = "choose_LA",
        choices = c("All local areas", rd_la_lookup |> filter(RD == input$choose_RD) |> pull(LA)),
        selected = "All local areas"
      )
    }

    # Update selected region
    if(input$choose_RD == "All regional districts") {
      selected_region("British Columbia")
    } else if(input$choose_RD != "All regional districts" & input$choose_LA == "All local areas") {
      selected_region(input$choose_RD)
    } else {
      selected_region(input$choose_LA)
    }

  })


  ## change to local area ----
  observeEvent(input$choose_LA, {
    req(input$choose_LA)

    # Update selected regional district
    if(input$choose_LA != "All local areas") {
      updatePickerInput(
        inputId = "choose_RD",
        selected = rd_la_lookup |> filter(LA == input$choose_LA & RD != "All regional districts") |> pull(RD)

      )
    }

    # Update selected region
    if(input$choose_RD == "All regional districts") {
      selected_region("British Columbia")
    } else if(input$choose_RD != "All regional districts" & input$choose_LA == "All local areas") {
      selected_region(input$choose_RD)
    } else {
      selected_region(input$choose_LA)
    }

  })


  ## change to clicked region on map ----
  observeEvent(input$summary_map_shape_click, {

    # Update selected region
    selected_region(input$summary_map_shape_click$id)

    if(selected_region() %in% rd_la_lookup$RD) {

      updatePickerInput(
        inputId = "choose_RD",
        selected = selected_region()
      )

    }

    if(selected_region() %in% rd_la_lookup$LA) {

      updatePickerInput(
        inputId = "choose_LA",
        selected = selected_region()
      )

    }


  })

  observeEvent(input$reset_map, {

    # Update selected region
    selected_region("British Columbia")

    # Update drop downs
    updatePickerInput(
      inputId = "choose_RD",
      selected = "All regional districts"
    )

  })


  # outputs ----

  output$regional_profile_row1 = renderUI(
    make_regional_profile_boxes(
      data[["Descriptive Stats"]] |> filter(REGION_NAME == selected_region(), REF_YEAR == 2020),
      regional_profile_info,
      tooltips)
    )

  output$summary_map = renderLeaflet(
    make_map(input$choose_level, input$choose_RD, input$choose_LA,
             stat_data = data[["Descriptive Stats"]] |> filter(REF_YEAR == 2020) |> select(REGION_NAME, STATISTIC = input$choose_topic),
             stat = regional_profile_info |> filter(col == input$choose_topic) |> pull(col_formatted)))


  output$summary_card <- renderUI({
    if (input$choose_topic == "POPULATION") {
      header <- "Summary"
    } else {
      header <- "Top 5 industries by employment"
    }

    card(
      card_header(header),
      reactableOutput("summary_table")
    )
  })

  output$summary_table <- renderReactable({
    if (input$choose_topic == "POPULATION") {
      reactable(
        make_summary_table_output(
          data[["Descriptive Stats"]] |>
            filter(REGION_NAME == selected_region())
        ),
        defaultColDef = colDef(html = TRUE),
        striped = TRUE
      )
    } else {
      reactable(
        make_industry_table(
          data[["Jobs"]] |>
            filter(REGION_NAME == selected_region())
        ),
        defaultColDef = colDef(html = TRUE),
        striped = TRUE
      )
    }
  })
  #
  #
  #
  # output$summary_table = renderReactable(
  #   reactable(defaultColDef = colDef(html = TRUE),
  #             striped = TRUE,
  #             make_summary_table_output(data[["Descriptive Stats"]] |>
  #                                         filter(REGION_NAME == selected_region()))
  #             )
  #   )
  #
  # output$industry_table = renderReactable(
  #   reactable(defaultColDef = colDef(html = TRUE),
  #             striped = TRUE,
  #             make_industry_table(data[["Jobs"]] |>
  #                                   filter(REGION_NAME == selected_region()))
  #             )
  # )





  output$profile_heading <- renderText({
    selected_region()
  })





  bcsapps::bcsHeaderServer(id = 'header', links = T)
  bcsapps::bcsFooterServer(id = 'footer')
}

# Run the application
shinyApp(ui = ui, server = server)
