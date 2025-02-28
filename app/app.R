# set up app
source(paste0(here::here(), "/app/R/functions.R"))
source(paste0(here::here(), "/app/R/global.R"))

# constants
last_updated = format(now(), "%b %d, %Y")
pages = c("Home", "Regional Profile")


ui = page_sidebar(
  useShinyjs(),

  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")),

  title = bcsapps_bslib_header(id = 'header', appname = "LAEP dashboard", github = "https://github.com/bcgov/LAEP-dashboard"),

  sidebar = list(
    pickerInput("choose_page", label = "Choose Your Page", width='100%', inline = T, choices = pages, selected = "Home", choicesOpt = list(icon = c("fa-home", "fa-line-chart")), options = pickerOptions(
      actionsBox = TRUE,
      iconBase = "fas"
    )),

    div(id = "choose_region_div",
      pickerInput("choose_region", "Choose the area to profile", choices = edas, multiple = F),
      pickerInput("choose_shift_share", "Choose the Shift/Share Period to Analyze", choices = shift_share_year_combos, multiple = F)

    ),

    br(),
    br(),
    div(id = "download_div", downloadBttn("download_button", "Download data!", size = 'sm', block = F, color = 'primary')),
    div(style = "text-align:center;color:#b8c7ce", uiOutput("update_date"))
  ),

  div(id = 'Home', uiOutput("Home")),
  div(id = 'Regional Profile',
    uiOutput("regional_profile_row1"),
    uiOutput("regional_profile_row2"),
    layout_column_wrap(width = 1/2, fill = F, fillable = T,
      navset_card_tab(full_screen = T, title = "Economic Profile", nav_panel("Table", reactableOutput("t1")), nav_panel("Graph", pickerInput("choose_g1", "Choose Variables to Graph", choices = to_sentence_case(c("POPULATION", "TOTAL_JOBS", "TOTAL_INCOME", "AVERAGE_EMPLOYMENT_INCOME", "DIVERSITY_INDEX")), multiple = T), plotOutput("g1"))),
      navset_card_tab(full_screen = T, title = tooltip_text("Shift/Share Analysis"), nav_panel("Table", reactableOutput("t2"))
      )
    ),
    card("Top 5 Industries by Employment", reactableOutput("t3"))
  ),

  bcsapps::bcsFooterUI(id = 'footer')
)

server <- function(input, output, session) {

  region = eventReactive(input$choose_region, input$choose_region)
  regional_data = eventReactive(region(), filter(data[[1]], REGION_NAME == region()))
  regional_data_jobs = eventReactive(region(), {
    df = data[[2]] |>
      filter(REGION_NAME == region()) |>
      select(-matches("_TOTAL$")) |>
      select(REF_YEAR, TOTAL:last_col()) |>
      mutate(across(everything(), as.integer))
    n = names(df)
    df = df |>
      t() |>
      as_tibble(.name_repair = 'minimal')
    names(df) = as.character(as.vector(df[1,]))
    df = df[-1,]
    df = mutate(df, Industry = to_sentence_case(n[-1]), .before=1)
  })


  shift_share_years = eventReactive(input$choose_shift_share, as.integer(c(word(input$choose_shift_share, 1), word(input$choose_shift_share, 3))))

  shift_share_data = eventReactive(regional_data_jobs(), {
    df = regional_data_jobs() |>
      select(Industry, !!as.character(shift_share_years()))
    df$change = pull(df[,3]) - pull(df[,2])
    return(df)
  })



  output$Home = renderUI(source(here::here() %,% '/home_page.R')$value)

  output$regional_profile_row1 = renderUI(
    layout_column_wrap(width=1/3, fill = F,
      make_value_box(regional_data(), "Population", icon='earth-americas'),
      make_value_box(regional_data(), "Total Jobs", icon='tower-observation'),
      make_value_box(regional_data(), "Average Employment Income", formatter=scales::label_dollar, icon='money-bills')
    )
  )

  output$regional_profile_row2 = renderUI(
    layout_column_wrap(width = 1/3, fill = F, fillable = F,
      make_value_box(regional_data(), "Basic Income Share", formatter=scales::label_percent, icon='scale-balanced'),
      make_value_box(regional_data(), "Diversity Index", formatter=scales::label_comma, icon='rainbow'),
      make_value_box(regional_data(), "Forest Sector Vulnerability", col = to_screaming_snake_case("Forest Sector Vulnerability Index"), formatter=scales::label_comma, icon='tree')
    )
  )

  output$t1 = renderReactable(
    regional_data() |>
      select(REF_YEAR, POPULATION, TOTAL_JOBS, TOTAL_INCOME, AVERAGE_EMPLOYMENT_INCOME, DIVERSITY_INDEX) |>
      mutate(across(c(POPULATION, TOTAL_JOBS, DIVERSITY_INDEX), ~scales::label_comma()(.))) |>
      mutate(across(matches("INCOME"), ~scales::label_dollar()(.))) |>
      janitor::clean_names(case='sentence') |>
      reactable()
  )

  output$g1 = renderPlot({
    if (is.null(input$choose_g1)) return(NULL)
    regional_data() |>
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
    regional_data_jobs() |>
      mutate(x = regional_data_jobs()[, ncol(regional_data_jobs())]) |>
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
      data = regional_data()
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
  } else {
    show("choose_region_div")
    show("download_div")
  })



}

shinyApp(ui, server)
