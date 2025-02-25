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
            sidebarMenu(
              style = "position: fixed; overflow: visible;",
              id = "tabs", ## to be able to update with buttons on landing page
              #menuItem("Home", tabName = "home", icon = icon("home")),
              #menuItem("Small Business Growth", tabName = "page1", icon = icon("line-chart")),
              pickerInput("z", width='auto', inline = T, choices = list("home", "page1"), choicesOpt = list(icon = c("fa-home", "fa-line-chart")), options = pickerOptions(
                actionsBox = TRUE,
                title = "Please select a month",
                #header = "This is a header",
                iconBase = "fas"
              )),


              # menuItem("Small Business Employment", tabName = "page2", icon = icon("users")),
              # menuItem("Self-Employed", tabName = "page3", icon = icon("user"),
              #   menuSubItem("Main", tabName = "main"),
              #   menuSubItem("Women", tabName = "women"),
              #   menuSubItem("Indigenous people", tabName = "indigenous")),
              # menuItem("Contribution to Economy", tabName = "page4", icon = icon("usd")),
              # menuItem("Small Business Exports", tabName = "page5", icon = icon("truck")),
              # menuItem("Other Indicators", tabName = "page6", icon = icon("file-text")),
              # menuItem("Previous Versions", icon = icon("folder"), style = "padding:7px 0 7px 0",
              #   a(icon("download"), " Download 2023 data", id = "download_data_23", class = paste("shiny-download-link", "dwnldLink"),
              #     href = "", target = "_blank", download = NA),
              #   menuSubItem("Previous Reports", href = "https://llbc.ent.sirsidynix.net/client/en_GB/main/search/results?qu=small+business+profile&te=", newtab = TRUE, icon = icon("link"))),
              # menuItem("Small Business Resources", href = "https://www2.gov.bc.ca/gov/content/employment-business/business/small-business/resources", newtab = TRUE, icon = icon("link")),
              # menuItem("BC Data Catalogue Record", href = "https://catalogue.data.gov.bc.ca/dataset/14828d0e-3cab-4477-af30-eab919d3451a", newtab = TRUE, icon = icon("link")),

              br(),
              br(),
              downloadBttn(outputId = "download_data", "Download data as excel", size = 'xs', block = F, style = 'material-flat', color = 'primary'),
              actionBttn(inputId = "f", label = "Download data", size='sm', style = "material-flat", color = "default", no_outline = F, icon = icon("envelope")),
              div(style = "text-align:center;color:#b8c7ce", uiOutput("update_date"))
              # menuItem("Stand Alone Figures", tabName = "page7")
            )
          ), ## end sidebar ----
          dashboardBody(

            tabItems(

              tabItem("home",
                fluidRow(
                  div(
                    id = "light-blue",
                    box(
                      title = "Welcome",
                      p("Welcome!"),
                      pickerInput("dfd", "fdf", c(1,2,3))
                    )
                  )
                )
              ),

              tabItem("page1",
                bslib::card(
                  bslib::card_header("This is the header"),
                  plotOutput("g")
                )
              )

            ) ## end tabItems
          ) ## end dashboardBody
        )
      ),

      bcsapps::bcsFooterUI(id = 'footer')
    )
  )}












server <- function(input, output, session) {

  ## Change links to false to remove the link list from the header
  bcsapps::bcsHeaderServer(id = 'header', links = TRUE)
  bcsapps::bcsFooterServer(id = 'footer')


  observeEvent(input$z, updateTabItems(session, "tabs", input$z))

  output$download_data <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      data = case_match(input$z, "" ~ "mtcars", "home" ~ "mtcars", "page1" ~ "iris")
      write.csv(get(data), con)
    }
  )

  output$update_date <- renderUI(paste("Last updated:", last_updated))
  output$tbl = renderReactable(reactable(mtcars))
  output$g = renderPlot(ggplot(mtcars) + aes(am, gear) + geom_point())

}

shiny::shinyApp(ui, server)
