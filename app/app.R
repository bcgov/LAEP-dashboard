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
            pickerInput("choose_page", label = "Choose Your Page", width='100%', inline = T, choices = pages, selected = "Regional Profile", choicesOpt = list(icon = c("fa-home", "fa-line-chart", "fa-calculator")), options = pickerOptions(
              actionsBox = TRUE,
              iconBase = "fas"
            )),

            div(id = "choose_region_div",
              pickerInput("choose_RD", "Choose the RD to profile", choices = RDs, multiple = F),
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
            div(id = "download_div", downloadBttn("download_button", "Download data", size = 'sm', block = F, color = 'primary')),
            div(style = "text-align:center;color:#b8c7ce", uiOutput("update_date"))
          ),

          div(id = 'Home', home_page),

          div(id = 'Regional Profile',
            h1(textOutput("EDA_h1")),
            uiOutput("regional_profile_row1"),
            uiOutput("regional_profile_row2"),
            layout_column_wrap(width = 1/2, fill = F, fillable = T,
              navset_card_tab(full_screen = T, title = "Summary", nav_panel("Table", reactableOutput("t1")), nav_panel("Graph", pickerInput("choose_g1", "Choose Variables to Graph", choices = to_sentence_case(c("POPULATION", "TOTAL_JOBS", "TOTAL_INCOME", "AVERAGE_EMPLOYMENT_INCOME", "DIVERSITY_INDEX")), multiple = T, selected = "POPULATION"), plotlyOutput("g1")),
                nav_panel("Map", leafletOutput("l1"))
              ),
              navset_card_tab(full_screen = T, title = tooltip_text("Shift/Share Analysis", tooltips$value$shift_share), nav_panel("Table", reactableOutput("t2"))
              )
            ),
            card("Top 5 Industries by Employment", reactableOutput("t3"))
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
  EDA = eventReactive(input$choose_EDA, input$choose_EDA)

  observeEvent(input$choose_RD, updatePickerInput(session, inputId = "choose_EDA", choices = filter(data[[1]], GEO_TYPE == "EDA", PARENT_RD == input$choose_RD) |> pull(REGION_NAME) |> unique()))

  EDA_data = eventReactive(EDA(), filter(data[[1]], GEO_TYPE == "EDA", REGION_NAME == EDA()))
  EDA_data_jobs = eventReactive(EDA(), {
    df = data[[2]] |>
      filter(GEO_TYPE == "EDA", REGION_NAME == EDA()) |>
      select(-matches("_TOTAL$")) |>
      select(REF_YEAR, TOTAL:last_col()) |>
      mutate(across(everything(), as.integer))
    t2(df, "REF_YEAR", "Industry")
  })

  shift_share_years = eventReactive(input$choose_shift_share, as.integer(c(word(input$choose_shift_share, 1), word(input$choose_shift_share, 3))))
  shift_share_data = eventReactive(EDA_data_jobs(), {
    df = EDA_data_jobs() |>
      select(Industry, !!as.character(shift_share_years()))
    df$change = pull(df[,3]) - pull(df[,2])
    return(df)
  })


  output$EDA_h1 = renderText("Economic Profile: " %,% EDA())

  # maybe make this prettier in a self-contained function
  output$regional_profile_row1 = renderUI(
    layout_column_wrap(width=1/3, fill = F,
      make_value_box(EDA_data(), "Population", icon='earth-americas', theme = bs_themes_6[1]),
      make_value_box(EDA_data(), "Total Jobs", icon='tower-observation', theme = bs_themes_6[2]),
      make_value_box(EDA_data(), "Average Employment Income", formatter=scales::label_dollar, icon='money-bills', theme = bs_themes_6[3])
    )
  )

  output$regional_profile_row2 = renderUI(
    layout_column_wrap(width = 1/3, fill = F, fillable = F,
      make_value_box(EDA_data(), "Basic Income Share", formatter=scales::label_percent, icon='scale-balanced', theme = bs_themes_6[4]),
      make_value_box(EDA_data(), "Diversity Index", formatter=scales::label_comma, icon='rainbow', theme = bs_themes_6[5]),
      make_value_box(EDA_data(), "Forest Sector Vulnerability", col = to_screaming_snake_case("Forest Sector Vulnerability Index"), formatter=scales::label_comma, icon='tree', theme = bs_themes_6[6])
    )
  )

  output$t1 = renderReactable({
    EDA_data() |>
      select(REF_YEAR, POPULATION, TOTAL_JOBS, TOTAL_INCOME, AVERAGE_EMPLOYMENT_INCOME, DIVERSITY_INDEX) |>
      mutate(across(c(POPULATION, TOTAL_JOBS, DIVERSITY_INDEX), ~scales::label_comma()(.))) |>
      mutate(across(matches("INCOME"), ~scales::label_dollar()(.))) |>
      t2("REF_YEAR", "Variable") |>
      reactable()
  })

  output$g1 = renderPlotly({
    if (is.null(input$choose_g1)) return(NULL)
    p = EDA_data() |>
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

  output$l1 = renderLeaflet({
    m = filter(RDs_sf, str_detect(ADMIN_AREA_NAME, str_squish(str_replace(RD(), "RD", "")))) # kluge
    #if (nrow(m) != 1) browser()
    if (nrow(m) != 1) return(NULL)
    st_transform(m, crs = 4326) |>
      leaflet() |>
      addTiles() |>
      addPolygons()
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
    EDA_data_jobs() |>
      mutate(x = EDA_data_jobs()[, ncol(EDA_data_jobs())]) |>
      arrange(desc(x)) |>
      select(-x) |>
      filter(Industry != "Total") |>
      head(5) |>
      reactable()
  })






  output$download_button = downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      data = EDA_data()
      write.csv(data, con)
    }
  )





  bcsapps::bcsHeaderServer(id = 'header', links = T)
  bcsapps::bcsFooterServer(id = 'footer')

  observeEvent(input$choose_page, {
    show(input$choose_page)
    for (page in setdiff(pages, input$choose_page)) hide(page)
  })

  observeEvent(input$choose_page, if (input$choose_page == "Home") {
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
    show("download_div")
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




}

shinyApp(ui, server)
