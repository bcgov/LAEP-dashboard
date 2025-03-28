is_local = Sys.getenv('SHINY_PORT') == ""
source(paste0(here::here(), ifelse(is_local, '/app', ''), "/R/functions.R"), local = T)
source(paste0(here::here(), ifelse(is_local, '/app', ''), "/R/global.R"), local = T)
source(paste0(here::here(), ifelse(is_local, '/app', ''), "/R/Jeff.R"), local = T)

ui <- function(req) {
  shiny::fluidPage(
    HTML("<html lang='en'>"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    tags$head(includeHTML("www/google-analytics.html")),
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
              label = "1. Choose Your Page",
              choiceNames = pages$choice,
              choiceValues = pages$name,
              individual = TRUE,
              justified = T,
              direction = "vertical",
              status = "choose_page",
              selected = if (is_local) start_page[1] else start_page[2]
            ),

            div(id = "choose_region_div", style = "display: none;",
              pickerInput("choose_RD", span("2. Choose the Regional District", info_icon(tooltips$RD)), choices = RDs, multiple = F),
              radioGroupButtons("choose_level", span("3. Choose the Geography Level", info_icon(tooltips$geography_level)), choiceNames = c("Regional District", "Local Area"), choiceValues = c("RD", "LA"), selected="RD"),
              pickerInput("choose_LA", span("3.1 Choose the Regional District", info_icon(tooltips$RD)), choices = LAs, multiple = F),
              pickerInput("choose_shift_share", "4. Choose the Shift/Share Period to Analyze", choices = shift_share_year_combos, multiple = F)
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

  # reactive for LA choice (RD is always chosen so can use input$choose_RD)
  LA = reactive(if (input$choose_level == "RD") NULL else input$choose_LA)

  # reactives for the data[[1]] and data[[2]] at the RD and LA levels
  RD_data = eventReactive(input$choose_RD, filter(data[[1]], GEO_TYPE == "RD", REGION_NAME == input$choose_RD))
  RD_data_jobs = eventReactive(input$choose_RD, {
    df = data[[2]] |>
      filter(GEO_TYPE == "RD", REGION_NAME == input$choose_RD) |>
      select(-matches("_TOTAL$")) |>
      select(REF_YEAR, TOTAL:last_col()) |>
      mutate(across(everything(), as.integer))
    t2(df, "REF_YEAR", "Industry")
  })
  LA_data = eventReactive(LA(), if (is.null(LA())) NULL else filter(data[[1]], GEO_TYPE == "EDA", REGION_NAME == LA()))
  LA_data_jobs = eventReactive(LA(), {
    if (is.null(LA())) return(NULL)
    df = data[[2]] |>
      filter(GEO_TYPE == "EDA", REGION_NAME == LA()) |>
      select(-matches("_TOTAL$")) |>
      select(REF_YEAR, TOTAL:last_col()) |>
      mutate(across(everything(), as.integer))
    t2(df, "REF_YEAR", "Industry")
  })

  # pick the correct data frames above (RD or LA level)
  data_final = reactive(if (input$choose_level == "RD") RD_data() else LA_data())
  data_jobs_final = reactive(if (input$choose_level == "RD") RD_data_jobs() else LA_data_jobs())

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

  output$regional_profile_h1 = renderText(if (input$choose_level == "LA") "Economic Profile: " %,% LA() %,,% "Local Area" else "Economic Profile: " %,% input$choose_RD %,,% "Regional District")

  output$regional_profile_row1 = renderUI(make_regional_profile_boxes(data_final(), regional_profile_info, tooltips))

  output$summary_table = renderReactable(nicetable(make_summary_table_output(data_final()), rowStyle = function(index) if (index%%2==0) list(fontStyle = "italic", fontSize = ".8em")))

  output$summary_graph = renderGirafe({
    req(input$choose_summary_graph)
    x = make_summary_graph_output(data_final(), cols = input$choose_summary_graph)
    girafe(code = print(reduce(x, `+`) + plot_layout(ncol = 2, byrow = FALSE)))
  })

  output$summary_map = renderLeaflet(make_summary_map_output(region = input$choose_RD))

  output$shift_share_table = renderReactable(make_shift_share_table_output(data_jobs_final(), shift_share_years()) |> nicetable(groupBy = 'group'))

  output$industry_table = renderReactable(nicetable(make_industry_table(data_jobs_final())))

  # LAEP Calculator Page outputs
  observe(walk(1:num_laeps(), function(i) output[['laep_table_' %,% i]] = renderReactable(nicetable(laep_outputs()[[i]], defaultPageSize = 5))))

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

  # reset RD
  observeEvent(input$choose_RD, updateRadioGroupButtons(session, "choose_level", selected="RD"))


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


  observeEvent(input$choose_RD, updatePickerInput(session, inputId = "choose_LA", choices = filter(data[[1]], GEO_TYPE == "EDA", PARENT_RD == input$choose_RD) |> pull(REGION_NAME) |> unique()))

  observe(if (input$choose_level == "RD") hide("choose_LA") else show("choose_LA"))

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
