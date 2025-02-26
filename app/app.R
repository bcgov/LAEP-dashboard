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
            div(id = "download_button_div", downloadBttn("download_button", "Download data as excel", size = 'xs', block = F, style = 'material-flat', color = 'primary')),
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


  regional_profile_row_1 = eventReactive(region(), {
    fluidRow(
      make_infobox(regional_data()),
      make_infobox(regional_data(), "Total Jobs", color='navy', icon='arrow-left'),
      make_infobox(regional_data(), "Average Employment Income", formatter=scales::label_dollar, color='orange')
    )
  })

  regional_profile_row_2 = eventReactive(region(), {
    fluidRow(
      make_infobox(regional_data(), "Basic Income Share", formatter=scales::label_percent),
      make_infobox(regional_data(), "Diversity Index", formatter=scales::label_comma),
      make_infobox(regional_data(), "Forest Sector Vulnerability Index", formatter=scales::label_comma)
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

      "Home" = fluidPage(
        fluidRow(home_text)
      ),

      "Regional Profile" = fluidPage(
        regional_profile_row_1(),
        regional_profile_row_2(),
        fluidRow(
          tabBox(title = "Economic Profile for" %,,% region(), width = '50%',
            tabPanel("Table", reactable(regional_profile_t_1())),
            tabPanel("Graph", ggplotly(regional_profile_g_1()))
          ),
          tabBox(title = "Shift/Share Analysis for job changes from" %,,% input$choose_shift_share, width = '50%',
            tabPanel("Table", reactable(regional_profile_t_1())),
            tabPanel("Graph", ggplotly(regional_profile_g_1()))
          )
        )
      )
    )
  })



  ## Change links to false to remove the link list from the header
  bcsapps::bcsHeaderServer(id = 'header', links = T)
  bcsapps::bcsFooterServer(id = 'footer')

  # set up download button - just using mtcars for now
  output$download_data = downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      data = case_match(input$choose_page, "" ~ "mtcars", "home" ~ "mtcars", "page1" ~ "iris")
      write.csv(get(data), con)
    }
  )

  output$update_date <- renderUI(paste("Last updated:", last_updated))

  # toggle download button
  observeEvent(input$choose_page, if (input$choose_page == "Home") hide("download_button_div") else show("download_button_div"))

  observeEvent(input$choose_page, if (input$choose_page == "Regional Profile") show("choose_region_div") else hide("choose_region_div"))
}




shiny::shinyApp(ui, server)



# to dos:
# use bslib cards for pages - might solve your issue
# rando colors for infoboxes
