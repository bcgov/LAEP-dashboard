# notes to self:
# ask steph y to deploy to shinyapps
# ask steph y about bslib
# github this
# data as csvs?


# Copyright 2023 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

last_updated = format(now(), "%b %d, %Y")
pages = c("Home", "Regional Profile")

ui <- function(req) {
  shiny::fluidPage(
    theme = "styles.css",
    HTML("<html lang='en'>"),
    fluidRow(

      ## Replace appname with the title that will appear in the header
      bcsapps::bcsHeaderUI(id = 'header', appname = "LAEP dashboard", github = "Replace with github URL or NULL"),
      tags$head(tags$link(rel = "shortcut icon", href = "favicon.png")), ## to add BCGov favicon

      column(
        width = 12,
        style = "margin-top:75px",

        dashboardPage(
          skin = "blue",
          dashboardHeader(title = ""),

          dashboardSidebar(
            useShinyjs(),
            collapsed = FALSE,
            ## hide the standalone figure tab in the sidebar menu
            tags$head(tags$style(HTML("a[href = '#shiny-tab-page7']{ visibility: hidden; }"))),

            div(id = "choose_page_div", pickerInput("choose_page", label = "Choose Your Page", width='100%', inline = T, choices = pages, selected = "Regional Profile", choicesOpt = list(icon = c("fa-home", "fa-line-chart")), options = pickerOptions(
              actionsBox = TRUE,
              iconBase = "fas"
            ))),

            div(id = "choose_region_div",
              pickerInput("choose_region", "Choose the area to profile", choices = edas, multiple = F),
              pickerInput("choose_shift_share", "Choose the Shift/Share Period to Analyze", choices = shift_share_year_combos, multiple = F)

            ),

            br(),
            br(),
            div(id = "download_button_div", downloadBttn("download_button", "Download data!", size = 'sm', block = F, color = 'primary')),
            div(style = "text-align:center;color:#b8c7ce", uiOutput("update_date"))
          ), ## end sidebar ----

          dashboardBody(uiOutput("selected_page"))
        ) ## end dashboardPage
      ),

      bcsapps::bcsFooterUI(id = 'footer')
    )
  )
}












server <- function(input, output, session) {

  # reactables go here
  region = eventReactive(input$choose_region, input$choose_region)
  regional_data = eventReactive(region(), filter(data[[1]], REGION_NAME == region()))
  regional_data_jobs = eventReactive(region(), filter(data[[2]], REGION_NAME == region()))

  shift_share_years = eventReactive(input$choose_shift_share, as.integer(c(word(input$choose_shift_share, 1), word(input$choose_shift_share, 3))))

  regional_data_jobs_w_shift_share = eventReactive(shift_share_years(), filter(regional_data_jobs(), REF_YEAR %in% shift_share_years()))

  shift_share_t1 = eventReactive(input$choose_shift_share, {
    df = regional_data_jobs_w_shift_share() |>
      select(-matches("_TOTAL$")) |>
      select(TOTAL:last_col()) |>
      mutate(across(everything(), as.integer))

    df = df |>
      t() |>
      as_tibble(.name_repair = 'minimal') |>
      set_names(shift_share_years()) |>
      mutate(Industry = to_sentence_case(names(df)), .before=1)

    df$change = pull(df[,3]) - pull(df[,2])
    return(df)
  })

  shift_share_t2 = eventReactive(shift_share_t1(), {
    bind_rows(
      filter(shift_share_t1(), Industry == "Total") |> mutate(group = "Total"),
      filter(shift_share_t1(), Industry != "Total") |> arrange(names(shift_share_t1())[3]) |> head(5) |> mutate(group = "Best"),
      filter(shift_share_t1(), Industry != "Total") |> arrange(names(shift_share_t1())[3]) |> tail(5) |> mutate(group = "Worst")
    ) |>
      relocate(group, .before=1)
  })





  regional_profile_row_1 = eventReactive(region(), {
    fluidRow(
      make_infobox(regional_data(), "Population", icon='earth-americas'),
      make_infobox(regional_data(), "Total Jobs", icon='tower-observation'),
      make_infobox(regional_data(), "Average Employment Income", formatter=scales::label_dollar, icon='money-bills')
    )
  })

  regional_profile_row_2 = eventReactive(region(), {
    fluidRow(
      make_infobox(regional_data(), "Basic Income Share", formatter=scales::label_percent, icon='scale-balanced'),
      make_infobox(regional_data(), "Diversity Index", formatter=scales::label_comma, icon='rainbow'),
      make_infobox(regional_data(), "Forest Sector Vulnerability Index", formatter=scales::label_comma, icon='tree')
    )
  })

  regional_profile_t_1 = eventReactive(region(), {
    regional_data() |>
      select(REF_YEAR, POPULATION, TOTAL_JOBS, TOTAL_INCOME, AVERAGE_EMPLOYMENT_INCOME, DIVERSITY_INDEX) |>
      mutate(across(c(POPULATION, TOTAL_JOBS, DIVERSITY_INDEX), ~scales::label_comma()(.))) |>
      mutate(across(matches("INCOME"), ~scales::label_dollar()(.))) |>
      janitor::clean_names(case='sentence')
  })


  regional_profile_g_1 = eventReactive(region(), {
    regional_data() |>
      select(REF_YEAR, POPULATION, TOTAL_JOBS, TOTAL_INCOME, AVERAGE_EMPLOYMENT_INCOME, DIVERSITY_INDEX) |>
      janitor::clean_names(case='sentence') |>
      pivot_longer(cols = 2:last_col()) |>
      mutate(year = as.factor(`Ref year`)) |>
      ggplot(aes(y=value, x=year, fill=year)) +
      geom_col(position = 'dodge') +
      facet_wrap(~name, scales='free_y') +
      ggthemes::theme_clean() +
      theme(legend.position = 'bottom') +
      scale_fill_viridis_d()
  })


  # we will use a massive 'switch' statement to select the correct page to show

  output$selected_page = renderUI({
    switch(input$choose_page,

      "Home" = source(here::here() %,% '/home_page.R')$value,

      "Regional Profile" = fluidPage(
        regional_profile_row_1(),
        regional_profile_row_2(),
        fluidRow(
          tabBox(title = "Economic Profile for" %,,% region(), width = 6,
            tabPanel("Table", reactable(regional_profile_t_1())),
            tabPanel("Graph", ggplotly(regional_profile_g_1()))
          ),
          tabBox(title = tooltip_text("Shift/Share Analysis for job changes from" %,,% input$choose_shift_share, tooltips$value$shift_share), width = 6,
            tabPanel("Table", reactable(shift_share_t2(), groupBy = 'group'))
          )
        )
      )
    )
  })





  # set up download button - just using mtcars for now
  output$download_button = downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      data = regional_data()
      write.csv(data, con)
    }
  )

  output$update_date <- renderUI(paste("Last updated:", last_updated))

  # toggle download button
  observeEvent(input$choose_page, if (input$choose_page == "Home") hide("download_button_div") else show("download_button_div"))

  observeEvent(input$choose_page, if (input$choose_page == "Regional Profile") show("choose_region_div") else hide("choose_region_div"))

  ## Change links to false to remove the link list from the header
  bcsapps::bcsHeaderServer(id = 'header', links = T)
  bcsapps::bcsFooterServer(id = 'footer')
}




shiny::shinyApp(ui, server)
