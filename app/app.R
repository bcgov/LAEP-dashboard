
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
                   pickerInput("choose_topic", "Choose Map Topic",
                               choices = c("Population",
                                           "Diversity index",
                                           "Income shares",
                                           "Dominant income sources"),
                               multiple = F, selected = "Basic statistics"),
                   conditionalPanel(
                     condition = "input.choose_topic == 'Income shares'",
                     pickerInput("choose_industry", "Choose industry:", data[["Income Dependencies"]] |> pull(VARIABLE) |> unique())
                   ),
                   conditionalPanel(
                     condition = "input.choose_topic == 'Dominant income sources'",
                     radioGroupButtons("choose_source",
                                       choices = c("Basic sources" = "Dominant basic income source",
                                                   "Private sector sources" = "Dominant private sector employment basic income source"),
                                       selected = "Dominant basic income source",
                                       direction = "vertical", justified = TRUE, width = "100%")
                   ),
                   actionButton("reset_map", label = "Reset map view", icon = icon("arrow-rotate-right")),
                   div(style = "margin-top:25px",
                     strong("Resources:"),
                     br(),
                     a("LAEP toolkit", href=""),
                     br(),
                     a("FAQs", href = ""))


               )
             )
             ),

             div(id = "Regional Profile",
                 uiOutput("regional_profile_row1"),
                 layout_column_wrap(width = 1/2,
                                    card(full_screen = TRUE,
                                         card_body(div(
                                           style = "display: flex; align-items: center; gap: 1rem;",
                                           tags$label("Select Year:"),
                                           radioButtons(
                                           inputId = "selected_year",
                                           label = NULL,
                                           choices = c("2010", "2015", "2020"),
                                           selected = "2020",
                                           inline = TRUE
                                         )),
                                           leafletOutput("map"#, height = "100%"
                                                                 ))),
                                    card(full_screen = TRUE,
                                         card_header(span("Summary statistics", info_icon(tooltips$summary_statistics))),
                                         card_body(reactableOutput("summary_table")))
                                    ),
                 layout_column_wrap(width = 1/2,
                                    card(full_screen = TRUE,
                                         card_header(span("Top 5 industries by basic income share", info_icon(tooltips$top_shares))),
                                         card_body(reactableOutput("top_5_shares"))),
                                    card(full_screen = TRUE,
                                         card_header(span("Top 5 industries by number of jobs", info_icon(tooltips$top_employment))),
                                         card_body(reactableOutput("top_5_jobs"))))

                                    # uiOutput("summary_card")
                 #)
                 #                    card(full_screen = TRUE,
                 #                         card_body(reactableOutput("summary_table")))),
                 # card("Top 5 industries by employment", reactableOutput("industry_table"))
                 )
           #  )
)
),
bcsapps::bcsFooterUI(id = 'footer')
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
    new_selected <- rd_la_lookup |> filter(LA == input$choose_LA & RD != "All regional districts") |> pull(RD)
    print(input$choose_LA)
    print(new_selected)

    # Update selected regional district
    if(input$choose_LA != "All local areas") {
      updatePickerInput(
        inputId = "choose_RD",
        selected = new_selected

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
  observeEvent(input$map_shape_click, {

    # Update selected region
    selected_region(input$map_shape_click$id)

    if(selected_region() %in% rd_la_lookup$RD) {
      print(paste("mapclick RD:", selected_region()))
      updatePickerInput(
        inputId = "choose_RD",
        selected = selected_region()
      )

    }

    if(selected_region() %in% rd_la_lookup$LA) {
      print(paste("mapclick LA:", selected_region()))
      new_rd <- rd_la_lookup |> filter(LA == selected_region() & RD != "All regional districts") |> pull(RD)

      updatePickerInput(
        inputId = "choose_LA",
        choices = c("All local areas", rd_la_lookup |> filter(RD == new_rd) |> pull(LA)),
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
  output$profile_heading <- renderText({
    selected_region()
  })

  output$regional_profile_row1 = renderUI({
    make_regional_profile_boxes(
      data[["Descriptive Stats"]] |> filter(REGION_NAME == selected_region(), REF_YEAR == latest_year),
      tooltips)
    })

  output$map = renderLeaflet({
    req(input$choose_level, input$choose_RD, input$choose_LA, input$choose_topic)

    if (input$choose_topic == "Population") {
      df <- data[["Descriptive Stats"]] |> filter(REF_YEAR == input$selected_year, VARIABLE == "Population")

    } else if(input$choose_topic == "Diversity index") {
      df <- data[["Descriptive Stats"]] |> filter(REF_YEAR == input$selected_year, VARIABLE == "Diversity index")

    } else if(input$choose_topic == "Income shares") {
      df <- data[["Income Dependencies"]] |> filter(REF_YEAR == input$selected_year, VARIABLE == input$choose_industry)

    } else {
      df <- data[["Dominant Income Sources"]] |> filter(REF_YEAR == input$selected_year, VARIABLE == input$choose_source)
    }


    make_map(input$choose_level,
             input$choose_RD,
             input$choose_LA,
             stat_data = df)
    })

  ## alternate mapping code ----
  # output$map <- renderLeaflet({
  #   leaflet(options = leafletOptions(zoomSnap = 0.2, zoomDelta = 1, attributionControl = FALSE)) |>
  #     addProviderTiles("CartoDB.Voyager") |>
  #     addPolygons(data = map_bc, layerId = ~REGION_NAME, label = ~REGION_NAME,
  #                 weight = 0, color = "transparent", fillOpacity = 0) |>
  #     addControl(html = HTML(
  #       '<div class="leaflet-control-attribution">
  #       Leaflet | &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>
  #       contributors &copy; <a href="https://carto.com/">CARTO</a>
  #     </div>'), position = "bottomleft")
  # })
  #
  # observeEvent({ input$choose_level; input$choose_RD; input$choose_LA; input$choose_topic}, {
  #   proxy <- leafletProxy("map") |> clearShapes() |> clearControls()
  #
  #   # Retrieve filtered data depending on input
  #   region_type <- input$choose_level
  #   rd_region <- input$choose_RD
  #   la_region <- input$choose_LA
  #   topic <- input$choose_topic
  #
  #   stat = regional_profile_info |> filter(col == topic) |> pull(col_formatted)
  #   stat_data = data[["Descriptive Stats"]] |> filter(REF_YEAR == 2020) |> select(REGION_NAME, STATISTIC = all_of(topic))
  #
  #   pal <- colorNumeric("YlGn", domain = NULL)
  #   light_border<- "#9F9D9C"
  #   dark_border <- "#252423" #"#353433"
  #
  #   if (region_type == "RD") {
  #     print("regiontype=RD")
  #     if (!str_detect(rd_region, "All") & str_detect(la_region, "All")) {
  #       print("region !all, la =all")
  #
  #       background_map <- map_rds
  #       outline_map <- map_rds |> filter(REGION_NAME == rd_region) |> mutate(color = dark_border) |>
  #         bind_rows(map_las |>
  #                     filter(REGION_NAME %in% (rd_la_lookup |> filter(RD == rd_region) |> pull(LA))) |>
  #                     mutate(color = light_border)
  #                   )
  #       centered_map <- map_rds |> filter(REGION_NAME == rd_region)
  #       colored_map <- map_las |>
  #         filter(REGION_NAME %in% (rd_la_lookup |> filter(RD == rd_region) |> pull(LA))) |>
  #         left_join(stat_data, by = "REGION_NAME")
  #
  #     } else if (!str_detect(rd_region, "All") & !str_detect(la_region, "All")) {
  #       print("region !all, la !all")
  #
  #       background_map <- map_rds |> st_difference(map_las |> filter(REGION_NAME == la_region))
  #       outline_map <- map_las |> filter(REGION_NAME == la_region) |> mutate(color = dark_border)
  #       centered_map <- map_las |> filter(REGION_NAME == la_region)
  #       colored_map <- map_las |> filter(REGION_NAME == la_region)|>
  #         left_join(stat_data, by = "REGION_NAME")
  #
  #     } else {
  #       print("region = All")
  #       background_map <- NULL
  #       outline_map <- map_rds |> mutate(color = light_border)
  #       centered_map <- map_rds
  #       colored_map <- map_rds_clipped |>
  #         left_join(stat_data, by = "REGION_NAME")
  #     }
  #
  #   } else {  # LA
  #     print("regiontype = LA")
  #     if (!str_detect(rd_region, "All") & str_detect(la_region, "All")) {
  #       print("LA: region !all, la =all")
  #
  #       background_map <- map_las |> st_difference(map_las |>
  #                                                    filter(REGION_NAME %in% (rd_la_lookup |> filter(RD == rd_region) |> pull(LA))))
  #       outline_map <- map_rds |> filter(REGION_NAME == rd_region) |> mutate(color = dark_border) |>
  #         bind_rows(map_las |>
  #                     filter(REGION_NAME %in% (rd_la_lookup |> filter(RD == rd_region) |> pull(LA))) |>
  #                     mutate(color = light_border)
  #         )
  #       centered_map <- map_rds |> filter(REGION_NAME == rd_region)
  #       colored_map <- map_las |>
  #         filter(REGION_NAME %in% (rd_la_lookup |> filter(RD == rd_region) |> pull(LA))) |>
  #         left_join(stat_data, by = "REGION_NAME")
  #
  #     } else if (!str_detect(rd_region, "All") & !str_detect(la_region, "All")) {
  #       print("LA: region !all, la !all")
  #       background_map <- map_las |> st_difference(map_las |> filter(REGION_NAME == la_region))
  #       outline_map <- map_las |> filter(REGION_NAME == la_region) |> mutate(color = dark_border)
  #       centered_map <- map_las |> filter(REGION_NAME == la_region)
  #       colored_map <- map_las |> filter(REGION_NAME == la_region) |>
  #         left_join(stat_data, by = "REGION_NAME")
  #
  #     } else {
  #       print("LA: region = All")
  #       background_map <- NULL
  #       outline_map <- map_las |> mutate(color = light_border)
  #       centered_map <- map_las
  #       colored_map <- map_las |>
  #         left_join(stat_data, by = "REGION_NAME")
  #     }
  #   }
  #
  #   bbox <- st_bbox(centered_map)
  #
  #   print("pre-mapping")
  #   if(!is.null(background_map)) {
  #     print("backgroundmap")
  #     proxy <- proxy |>
  #       addPolygons(data = background_map,
  #                   layerId = ~REGION_NAME,
  #                   label = ~REGION_NAME,
  #                   weight = 1, color = "#9F9D9C", fillOpacity = 0,
  #                   highlightOptions = highlightOptions(
  #                     weight = 5,
  #                     color = "#9F9D9C",
  #                     bringToFront = TRUE)
  #       )
  #   }
  #
  #   proxy <- proxy |>
  #     addPolygons(data = outline_map, weight = 2, color = ~color, fillOpacity = 0) |>
  #     add_colored_layer(data = colored_map, stat, pal) |>
  #     fitBounds(lng1 = bbox$xmin[[1]],
  #               lat1 = bbox$ymin[[1]],
  #               lng2 = bbox$xmax[[1]],
  #               lat2 = bbox$ymax[[1]]) |>
  #     add_legend(colored_map, stat, pal)
  #
  # })

# ----

  output$summary_table <- renderReactable({
    table <- data[["Descriptive Stats"]] |> filter(REGION_NAME == selected_region()) |> make_summary_table_output()

    reactable(
      table,
      defaultColDef = colDef(html = TRUE),
      striped = TRUE
    )

  })

  output$top_5_shares <- renderReactable({
    table <- data[["Economic Base"]] |>
      filter(REGION_NAME == selected_region()) |>
      select(REF_YEAR, Industry = VARIABLE, FORMATTED_VALUE) |>
      pivot_wider(names_from = "REF_YEAR", values_from = "FORMATTED_VALUE")

    reactable(
      table,
      defaultColDef = colDef(html = TRUE),
      columns = list(
        Industry = colDef(width = 200)
      ),
      striped = TRUE
    )

  })

  output$top_5_jobs <- renderReactable({
    table <- data[["Jobs"]] |>
      filter(REGION_NAME == selected_region()) |>
      select(REF_YEAR, Industry = VARIABLE, FORMATTED_VALUE) |>
      pivot_wider(names_from = "REF_YEAR", values_from = "FORMATTED_VALUE")

    reactable(
      table,
      defaultColDef = colDef(html = TRUE),
      columns = list(
        Industry = colDef(width = 200)
      ),
      striped = TRUE
    )

  })

  ## alternate table code ----
  # output$summary_card <- renderUI({
  #   if (input$choose_topic == "Population") {
  #     header <- "Summary statistics"
  #   } else if(input$choose_topic == "Diversity index") {
  #     header <- "Top 5 industries by employment"
  #   } else {
  #     header <- "Top 5 industries by income source"
  #   }
  #
  #   card(
  #     card_header(header),
  #     reactableOutput("summary_table")
  #   )
  # })
  #
  # output$summary_table <- renderReactable({
  #
  #   if (input$choose_topic == "Population") {
  #     table <- data[["Descriptive Stats"]] |> filter(REGION_NAME == selected_region()) |> make_summary_table_output()
  #
  #   } else if(input$choose_topic == "Diversity index") {
  #     table <- data[["Jobs"]] |>
  #       filter(REGION_NAME == selected_region()) |>
  #       select(REF_YEAR, Industry = VARIABLE, FORMATTED_VALUE) |>
  #       pivot_wider(names_from = "REF_YEAR", values_from = "FORMATTED_VALUE")
  #
  #   } else {
  #     table <- data[["Economic Base"]] |>
  #       filter(REGION_NAME == selected_region()) |>
  #       select(REF_YEAR, Industry = VARIABLE, FORMATTED_VALUE) |>
  #       pivot_wider(names_from = "REF_YEAR", values_from = "FORMATTED_VALUE")
  #   }
  #
  #   reactable(
  #     table,
  #     defaultColDef = colDef(html = TRUE),
  #     striped = TRUE
  #   )


 # })
#----


  bcsapps::bcsHeaderServer(id = 'header', links = T)
  bcsapps::bcsFooterServer(id = 'footer')
}

# Run the application
shinyApp(ui = ui, server = server)
