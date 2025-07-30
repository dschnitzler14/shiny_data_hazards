library(shiny)
library(shinydashboard)
library(markdown)
library(sortable)
library(grid)
library(png)
library(utils)

css_link <- tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
                      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0")
                      )


header <- dashboardHeader(
  title = "Data Hazards",
    tags$li(
      class = "dropdown",
      actionLink("about_link", label = "About", icon = icon("info-circle"))
    ),
    tags$li(
      class = "dropdown",
      actionLink("data_hazards_project_link", label = "Data Hazards Project", icon = icon("arrow-up-right-from-square"))
    )
  )


sidebar <- dashboardSidebar(

  sidebarMenu()
)

body <- dashboardBody(
css_link
)

# ui combined ----
ui <- dashboardPage(header, sidebar, body)

# server ----
server <- function(input, output, session) {

observeEvent(input$about_link, {
showModal(
    modalDialog(
    title = "About Data Hazards",
    uiOutput("about_box_markdown"),
    easyClose = TRUE,
    footer = NULL
    )
)
})

output$about_box_markdown <- renderUI({
    includeMarkdown("markdown/about_data_hazards.md")
})

observeEvent(input$data_hazards_project_link, {
  browseURL("https://datahazards.com/")

})
}

shinyApp(ui, server)