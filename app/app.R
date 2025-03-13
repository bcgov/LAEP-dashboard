is_local = Sys.getenv('SHINY_PORT') == ""
source(paste0(here::here(), ifelse(is_local, '/app', ''), "/R/functions.R"), local = T)
source(paste0(here::here(), ifelse(is_local, '/app', ''), "/R/global.R"), local = T)
source(paste0(here::here(), ifelse(is_local, '/app', ''), "/R/Jeff.R"), local = T)



summary_map_labels = map(filter(RDs_sf, REF_YEAR == last_year) |> pull(REGION_NAME), function(name) {
  "<strong>" %,% name %,% "</strong><br/>\n" %,% (map(regional_profile_info$col, function(col) {
    regional_profile_info$col_short[match(col, regional_profile_info$col)] %,% ":" %,,% regional_profile_info$label[[match(col, regional_profile_info$col)]]()(pull(filter(RDs_sf, REF_YEAR == last_year, REGION_NAME == name), col))
  }) |>
      paste(collapse="<br />"))
}) |>
  lapply(HTML)

map = RDs_sf |>
  filter(REF_YEAR == last_year) |>
  select(REGION_NAME, geometry, !!!regional_profile_info$col) |>
  leaflet() |>
  #setView(lng = centroid[1], lat = centroid[2], zoom = 6) |>
  addTiles() |>
  addPolygons(
    fillColor = topo.colors(10, alpha = NULL),
    stroke = T,
    #weight = ~ifelse(is_chosen, 5, 1),
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




ui <- function(req) {
  shiny::fluidPage(
    HTML("<html lang='en'>"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    fluidRow(
      bcsapps::bcsHeaderUI(id = 'header', appname = "LAEP Dashboard", github = "https://github.com/bcgov/LAEP-dashboard"),
      tags$head(tags$link(rel = "shortcut icon", href = "favicon.png")), ## to add BCGov favicon
      column(width = 12, style = "margin-top:100px",
        page_sidebar(
          useShinyjs(),
          window_title = "LAEP Dashboard",

          sidebar = list(

            radioGroupButtons(
              inputId = "choose_page",
              label = "Choose Your Page",
              choiceNames = pages$choice,
              choiceValues = pages$name,
              individual = TRUE,
              justified = T,
              direction = "vertical",
              status = "choose_page",
              selected = if (is_local) start_page[1] else start_page[2]
            ),

            div(id = "choose_region_div", style = "display: none;",
              pickerInput("choose_RD", "Choose the RD to profile", choices = RDs, multiple = F),
              radioGroupButtons("choose_level", "RD or EDA?", choices = c("RD", "EDA"), selected = "EDA"),
              pickerInput("choose_EDA", "Choose the EDA to profile", choices = EDAs, multiple = F),
              pickerInput("choose_shift_share", "Choose the Shift/Share Period to Analyze", choices = shift_share_year_combos, multiple = F)
            ),

            div(id = "choose_laep_div", style = "display: none;",
              actionBttn("add_laep_scenario", "Add a Scenario", color = 'success'),
            ),

            br(),
            br(),

            div(id = "download_div", style = "display: none;", downloadBttn("download_button", "Download data", size = 'sm', block = F, color = 'primary')),
            div(style = "text-align:left; color:#b8c7ce; font-size: .8em;", "Last updated:" %,,% last_updated)
          ), ## end sidebar

          div(id = 'Home', home_page),

          div(id = 'Regional Profile',
            h1(textOutput("regional_profile_h1")),
            uiOutput("regional_profile_row1"),
            layout_column_wrap(width = 1/2, fill = F, fillable = T,
              navset_card_tab(full_screen = T, title = "Summary",
                nav_panel("Table", reactableOutput("summary_table")),
                nav_panel("Graph", pickerInput("choose_summary_graph", "Choose Variables to Graph", choices = to_sentence_case(regional_profile_info$col), multiple = T, selected = "POPULATION"), withSpinner(girafeOutput("summary_graph"))),
                nav_panel("Map", withSpinner(leafletOutput("summary_map")))
              ),
              navset_card_tab(full_screen = T, title = span("Shift/Share Analysis", info_icon(tooltips$shift_share)), nav_panel("Table", reactableOutput("shift_share_table"))
              )
            ),
            card(span("Top 5 Industries by Employment", info_icon(tooltips$top_employment)), reactableOutput("industry_table"))
          ),

          div(id = 'LAEP Calculator', uiOutput("laep"))
        )
      ),

      bcsapps::bcsFooterUI(id = 'footer')
    )
  )
}




server <- function(input, output, session) {

  # REACTIVES

  # the number of LAEP scenarios (starts off with none)
  num_laeps = reactiveVal(0)

  # reactives for RD and EDA choices
  RD = eventReactive(input$choose_RD, input$choose_RD)
  EDA = reactive(if (input$choose_level == "RD") NULL else input$choose_EDA)

  # reactives for the data[[1]] and data[[2]] at the RD and EDA levels
  RD_data = eventReactive(RD(), filter(data[[1]], GEO_TYPE == "RD", REGION_NAME == RD()))
  RD_data_jobs = eventReactive(RD(), {
    df = data[[2]] |>
      filter(GEO_TYPE == "RD", REGION_NAME == RD()) |>
      select(-matches("_TOTAL$")) |>
      select(REF_YEAR, TOTAL:last_col()) |>
      mutate(across(everything(), as.integer))
    t2(df, "REF_YEAR", "Industry")
  })
  EDA_data = eventReactive(EDA(), if (is.null(EDA())) NULL else filter(data[[1]], GEO_TYPE == "EDA", REGION_NAME == EDA()))
  EDA_data_jobs = eventReactive(EDA(), {
    if (is.null(EDA())) return(NULL)
    df = data[[2]] |>
      filter(GEO_TYPE == "EDA", REGION_NAME == EDA()) |>
      select(-matches("_TOTAL$")) |>
      select(REF_YEAR, TOTAL:last_col()) |>
      mutate(across(everything(), as.integer))
    t2(df, "REF_YEAR", "Industry")
  })

  # pick the correct data frames above (RD or EDA level)
  data_final = eventReactive(input$choose_level, if (input$choose_level == "RD") RD_data() else EDA_data())
  data_jobs_final = eventReactive(input$choose_level, if (input$choose_level == "RD") RD_data_jobs() else EDA_data_jobs())

  # A text representation of the choices in the 'shift share'
  shift_share_years = eventReactive(input$choose_shift_share, as.integer(c(word(input$choose_shift_share, 1), word(input$choose_shift_share, 3))))

  # A list of all the LAEP scenarios
  laep_outputs = reactive({
    map(1:num_laeps(), function(i) {
      req(input[['laep_year_' %,% i]])
      make_laep_scenario_table_output(year = input[['laep_year_' %,% i]], region = input[['laep_area_' %,% i]], industry = input[['laep_industry_' %,% i]], social_safety = input[['laep_social_safety_' %,% i]])
    })
  })


  # OUTPUTS

  # Regional Profile Page outputs

  output$regional_profile_h1 = renderText(if (input$choose_level == "EDA") "Economic Profile: " %,% EDA() %,,% "Economic Area" else "Economic Profile: " %,% RD() %,,% "Regional District")

  output$regional_profile_row1 = renderUI(make_regional_profile_boxes(data_final(), regional_profile_info, tooltips))

  output$summary_table = renderReactable(reactable(make_summary_table_output(data_final())))

  output$summary_graph = renderGirafe({
    req(input$choose_summary_graph)
    x = make_summary_graph_output(data_final(), cols = input$choose_summary_graph)
    girafe(code = print(reduce(x, `+`) + plot_layout(ncol = 2, byrow = FALSE)))
  })

  output$summary_map = renderLeaflet(make_summary_map_output(region = input$choose_RD))

  output$shift_share_table = renderReactable(make_shift_share_table_output(data_jobs_final(), shift_share_years()) |> reactable(groupBy = 'group'))

  output$industry_table = renderReactable(reactable(make_industry_table(data_jobs_final())))

  # LAEP Calculator Page outputs
  observe(walk(1:num_laeps(), function(i) output[['laep_table_' %,% i]] = renderReactable(reactable(laep_outputs()[[i]], defaultPageSize = 5))))

  # download handler output
  output$download_button = downloadHandler(
    filename = function() {
      "LAEP_data_" %,% ymd(Sys.Date()) %,% ".xlsx"
    },
    content = function(con) {
      wb = wb_workbook()
      if (input$choose_page == "Regional Profile") {
        wb$add_worksheet("Regional Profile")
        wb$add_data(sheet = "Regional Profile", x = data_final())
      } else if (input$choose_page == "LAEP Calculator") {
        if (num_laeps() == 0) return(NULL)
        walk(1:num_laeps(), function(i) {
          wb$add_worksheet("LAEP Scenario " %,% i)
          wb$add_data(sheet = "LAEP Scenario " %,% i, x = laep_outputs()[[i]])
        })
      }
      wb$save(con)
    }
  )




  # OBSERVABLES


  # add a LAEP scenario
  observeEvent(input$add_laep_scenario, {
    num_laeps(num_laeps() + 1)
    insertUI("#laep", "beforeEnd", div(laep_scenario_card(num_laeps())), immediate = T)
  })

  # delete a LAEP scenario
  observe({
    req(num_laeps())
    for (i in 1:num_laeps()) {
      observeEvent(input[["delete_laep_" %,% i]], {
        removeUI("#laep_card_" %,% i, immediate = T)
      })
    }
  })


  observeEvent(input$choose_RD, updatePickerInput(session, inputId = "choose_EDA", choices = filter(data[[1]], GEO_TYPE == "EDA", PARENT_RD == input$choose_RD) |> pull(REGION_NAME) |> unique()))

  observe(if (input$choose_level == "RD") hide("choose_EDA") else show("choose_EDA"))

  observeEvent(input$choose_page, {
    shinyjs::show(input$choose_page)
    for (page in setdiff(pages$name, input$choose_page)) shinyjs::hide(page)
  })

  observe({
    if (input$choose_page == "Home") {
      shinyjs::hide("choose_region_div")
      shinyjs::hide("download_div")
      shinyjs::hide("choose_laep_div")
    } else if (input$choose_page == "Regional Profile") {
      shinyjs::show("choose_region_div")
      shinyjs::show("download_div")
      shinyjs::hide("choose_laep_div")
    } else if (input$choose_page == "LAEP Calculator") {
      shinyjs::show("choose_laep_div")
      shinyjs::hide("choose_region_div")
      if (num_laeps() > 0) shinyjs::show("download_div")
    }
  })

  bcsapps::bcsHeaderServer(id = 'header', links = T)
  bcsapps::bcsFooterServer(id = 'footer')
}

shinyApp(ui, server)
