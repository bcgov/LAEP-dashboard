is_local = Sys.getenv('SHINY_PORT') == ""
source(paste0(here::here(), ifelse(is_local, '/app', ''), "/R/functions.R"))
source(paste0(here::here(), ifelse(is_local, '/app', ''), "/R/global.R"))


# constants
last_updated = format(now(), "%b %d, %Y")
pages = c("Home", "Regional Profile", "LAEP Calculator")


ui <- function(req) {
  shiny::fluidPage(
    theme = "styles.css",
    HTML("<html lang='en'>"),
    htmltools::tagList(htmltools::tags$head(htmltools::tags$style(htmltools::HTML(".recalculating {
    opacity: 1 !important;
    transition: none !important;
    color: tomato;
    /* background-color: tomato; */
  }")))),
    htmltools::tagList(htmltools::tags$head(htmltools::tags$style(htmltools::HTML("#header-links-linkList > div > div > div > div {
  min-width: 280px;}")))),
    htmltools::tagList(htmltools::tags$head(htmltools::tags$style(htmltools::HTML(".bslib-card[data-full-screen='true'] {z-index: 999999999;}")))),
      tags$head(
        tags$style(HTML("
      /* Customizing popover style */
      .popover {
        max-width: 500px; /* Maximum width of the popover */
        word-wrap: break-word; /* Allow long words to break */
        padding: 15px; /* Padding inside the popover */
        font-size: 16px; /* Font size */
        background-color: #f0f0f0; /* Background color */
        color: #333; /* Text color */
        border-radius: 10px; /* Rounded corners */
        z-index: 34782398473894327;
      }

      .popover-header {
        background-color: #007bff; /* Header background color */
        color: white; /* Header text color */
        font-weight: bold; /* Make the header text bold */
      }

      .popover-body {
        background-color: #f8f9fa; /* Popover body background */
      }
      "))),


    fluidRow(


      ## Replace appname with the title that will appear in the header
      #bcsapps::bcsHeaderUI(id = 'header', appname = "LAEP", github = "Replace with github URL or NULL"),
      bcsHeader(id = 'header', appname = "LAEP", github = "Replace with github URL or NULL"),

      tags$head(tags$link(rel = "shortcut icon", href = "favicon.png")), ## to add BCGov favicon

      column(width = 12,
        style = "margin-top:100px",

        page_sidebar(
          useShinyjs(),
          window_title = "LAEP",

          sidebar = list(
            pickerInput("choose_page", label = "Choose Your Page", width='100%', inline = T, choices = pages, selected = ifelse(is_local, "Regional Profile", "Home"), choicesOpt = list(icon = c("fa-home", "fa-line-chart", "fa-calculator")), options = pickerOptions(
              actionsBox = TRUE,
              iconBase = "fas"
            )),

            div(id = "choose_region_div",
              pickerInput("choose_RD", "Choose the RD to profile", choices = RDs, multiple = F),
              radioGroupButtons("choose_level", "RD or EDA?", choices = c("RD", "EDA"), selected = "EDA"),
              pickerInput("choose_EDA", "Choose the EDA to profile", choices = EDAs, multiple = F),
              pickerInput("choose_shift_share", "Choose the Shift/Share Period to Analyze", choices = shift_share_year_combos, multiple = F)
            ),

            div(id = "choose_laep_div",
              actionBttn("add_laep_scenario", "Add a Scenario", color = 'success'),
              br(),
              #br(),
              #actionBttn("reset_laep", "Reset this Page", color = 'danger')
            ),

            br(),
            br(),
            div(id = "download_div", actionBttn("download_button", "Download data", size = 'sm', block = F, color = 'primary', icon = icon('file'))),
            div(style = "text-align:center;color:#b8c7ce", uiOutput("update_date"))
          ),

          div(id = 'Home', home_page),

          div(id = 'Regional Profile',
            h1(textOutput("EDA_h1")),
            uiOutput("regional_profile_row1"),
            layout_column_wrap(width = 1/2, fill = F, fillable = T,
              navset_card_tab(full_screen = T, title = "Summary",
                nav_panel("Table", reactableOutput("t1")),
                nav_panel("Graph", pickerInput("choose_g1", "Choose Variables to Graph", choices = to_sentence_case(region_cols), multiple = T, selected = "POPULATION"), plotlyOutput("g1")),
                nav_panel("Map", leafletOutput("m1"))
              ),
              navset_card_tab(full_screen = T, title = span("Shift/Share Analysis", info_icon(tooltips$shift_share)), nav_panel("Table", reactableOutput("t2"))
              )
            ),
            card(span("Top 5 Industries by Employment", info_icon(tooltips$top_employment)), reactableOutput("t3"))
          ),

          div(id = 'LAEP Calculator', uiOutput("laep"))

        )
      ),

      bcsapps::bcsFooterUI(id = 'footer')
    )
  )
}

server <- function(input, output, session) {

  RD = eventReactive(input$choose_RD, input$choose_RD)
  EDA = reactive(if (input$choose_level == "RD") NULL else input$choose_EDA)

  observeEvent(input$choose_RD, updatePickerInput(session, inputId = "choose_EDA", choices = filter(data[[1]], GEO_TYPE == "EDA", PARENT_RD == input$choose_RD) |> pull(REGION_NAME) |> unique()))

  observe(if (input$choose_level == "RD") hide("choose_EDA") else show("choose_EDA"))

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

  data_final = eventReactive(input$choose_level, if (input$choose_level == "RD") RD_data() else EDA_data())
  data_jobs_final = eventReactive(input$choose_level, if (input$choose_level == "RD") RD_data_jobs() else EDA_data_jobs())

  shift_share_years = eventReactive(input$choose_shift_share, as.integer(c(word(input$choose_shift_share, 1), word(input$choose_shift_share, 3))))
  shift_share_data = eventReactive(data_jobs_final(), {
    df = data_jobs_final() |>
      select(Industry, !!as.character(shift_share_years()))
    df$change = pull(df[,3]) - pull(df[,2])
    return(df)
  })


  output$EDA_h1 = renderText(if (input$choose_level == "EDA") "Economic Profile: " %,% EDA() %,,% "Economic Area" else "Economic Profile: " %,% RD() %,,% "Regional District")

  output$regional_profile_row1 = renderUI(make_regional_profile_boxes(data_final(), region_cols, region_formatters, bs_themes_6, region_icons, region_tooltips))

  output$t1 = renderReactable({
    x = data_final() |> select(REF_YEAR, !!!region_cols)
    for (i in (1:6)) x[,i+1] = region_formatters[[i]]()(pull(x[,i+1]))
    x |>
      t2("REF_YEAR", "Variable") |>
      reactable()
  })

  output$g1 = renderPlotly({
    if (is.null(input$choose_g1)) return(NULL)
    p = data_final() |>
      select(REF_YEAR, to_screaming_snake_case(!!input$choose_g1)) |>
      janitor::clean_names(case='sentence') |>
      pivot_longer(cols = 2:last_col()) |>
      mutate(year = as.factor(`Ref year`)) |>
      ggplot(aes(y=value, x=year, fill=year)) +
      geom_col(position = 'dodge') +
      facet_wrap(~name, scales='free_y') +
      ggthemes::theme_clean() +
      theme(legend.position = 'bottom') +
      scale_fill_viridis_d() +
      labs(x=NULL, y=NULL, fill=NULL) +
      guides(fill='none')
    ggplotly(p)
  })



  output$m1 = renderLeaflet({
    df = RDs_sf |>
      filter(REF_YEAR == last_year) |>
      select(REGION_NAME, geometry, !!!region_cols) |>
      mutate(is_chosen = REGION_NAME == input$choose_RD)

    centroid = st_centroid(filter(df, REGION_NAME == input$choose_RD) |> pull(geometry)) |> st_coordinates() |> unique()

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
        label = m1_labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      )

  })

  output$t2 = renderReactable(
    bind_rows(
      filter(shift_share_data(), Industry == "Total") |> mutate(group = "Total"),
      filter(shift_share_data(), Industry != "Total") |> arrange(names(shift_share_data())[3]) |> head(5) |> mutate(group = "Best"),
      filter(shift_share_data(), Industry != "Total") |> arrange(names(shift_share_data())[3]) |> tail(5) |> mutate(group = "Worst")
    ) |>
      relocate(group, .before=1) |>
      reactable(groupBy = 'group')
  )

  output$t3 = renderReactable({
    data_jobs_final() |>
      mutate(x = data_jobs_final()[, ncol(data_jobs_final())]) |>
      arrange(desc(x)) |>
      select(-x) |>
      filter(Industry != "Total") |>
      head(5) |>
      reactable()
  })





  # output$download_button = downloadHandler(
  #   filename = function() {
  #     "LAEP_data_" %,% ymd(Sys.Date()) %,% ".xlsx"
  #   },
  #   content = function(con) {
  #     data = EDA_data()
  #     write.csv(data, con)
  #   }
  # )






  bcsapps::bcsHeaderServer(id = 'header', links = T)
  bcsapps::bcsFooterServer(id = 'footer')

  observeEvent(input$choose_page, {
    show(input$choose_page)
    for (page in setdiff(pages, input$choose_page)) hide(page)
  })

  observe({
    if (input$choose_page == "Home") {
      hide("choose_region_div")
      hide("download_div")
      hide("choose_laep_div")
    } else if (input$choose_page == "Regional Profile") {
      show("choose_region_div")
      show("download_div")
      hide("choose_laep_div")
    } else if (input$choose_page == "LAEP Calculator") {
      show("choose_laep_div")
      hide("choose_region_div")
      if (num_laeps() > 0) show("download_div")
    }
  })

  observe(walk(1:num_laeps(), function(i) output[['laep_t' %,% i]] = renderReactable({
    data[[2]] |>
      filter(REF_YEAR == input[['laep_year_' %,% i]]) |>
      filter(REGION_NAME == input[['laep_area_' %,% i]]) |>
      select(to_screaming_snake_case(!!input[['laep_industry_' %,% i]])) |>
      reactable(defaultPageSize = 5)
  })))


  num_laeps = reactiveVal(0)
  observeEvent(input$add_laep_scenario, {
    num_laeps(num_laeps() + 1)
    insertUI("#laep", "beforeEnd", div(laep_scenario_card(num_laeps())), immediate = T)
  })
  observe({
    req(num_laeps())
    for (i in 1:num_laeps()) {
      observeEvent(input[["delete_laep_" %,% i]], {
        removeUI("#laep_" %,% i, immediate = T)
      })
    }
  })

  observeEvent(input$download_button, {
    wb = wb_workbook()
    if (input$choose_page == "Regional Profile") {
      wb$add_worksheet("Regional Profile")
      wb$add_data(sheet = "Regional Profile", x = data_final())
    } else if (input$choose_page == "LAEP Calculator") {
      if (num_laeps() == 0) return(NULL)
      walk(1:num_laeps(), function(i) {
        wb$add_worksheet("LAEP Scenario " %,% i)
        wb$add_data(sheet = "LAEP Scenario " %,% i, x = data.frame(i))
      })
    }
    wb$open()

  })



}

shinyApp(ui, server)
