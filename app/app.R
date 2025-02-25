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

            div(id = "choose_page_div", pickerInput("choose_page", width='100%', inline = T, choices = pages, selected = "Home", choicesOpt = list(icon = c("fa-home", "fa-line-chart")), options = pickerOptions(
              actionsBox = TRUE,
              iconBase = "fas"
            ))),
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

  # we will use a massive 'switch' statement to select the correct page to show

  output$selected_page = renderUI({
    switch(input$choose_page,

      "Home" = fluidRow(box(home_page())),

      "Regional Profile" = fluidRow(p("Regional Profile"))
    )
  })





  ## Change links to false to remove the link list from the header
  bcsapps::bcsHeaderServer(id = 'header', links = TRUE)
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
}


shiny::shinyApp(ui, server)
