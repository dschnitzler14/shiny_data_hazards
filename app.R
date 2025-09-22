library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(markdown)
library(sortable)
library(grid)
library(png)
library(utils)
library(png)
library(gridExtra)
library(htmltools)
library(shinyjs)
library(tidyverse)
library(rvest)
library(xml2)


css_link <- tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
                      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0")
                      )

source("checkbox_data_hazards_module.R")
source("download_module.R")

fetch_hazard_index <- function(base = "https://datahazards.com/") {
  doc <- read_html(paste0(base, "labels.html"))

  cards  <- html_elements(doc, ".sd-card")
  hrefs  <- cards |> html_element("a.sd-stretched-link") |> html_attr("href")
  titles <- cards |> html_element("a.sd-stretched-link") |> html_text()
  imgs   <- cards |> html_element("img.sd-card-img-top") |> html_attr("src")
  alts   <- cards |> html_element("img.sd-card-img-top") |> html_attr("alt")

  hrefs_full <- paste0(base, hrefs)
  imgs_full  <- paste0(base, imgs)
  keys <- sub("\\.html$", "", basename(hrefs))

  hazards_df <- data.frame(
    key    = keys,
    title  = titles,
    href   = hrefs_full,
    img    = imgs_full,
    alt    = alts,
    stringsAsFactors = FALSE
  )

  hazards_df <- hazards_df[!is.na(hazards_df$key) & !is.na(hazards_df$title), , drop = FALSE]

}

hazards_df <- fetch_hazard_index()

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
  disable = FALSE
)

body <- dashboardBody(
css_link,
fluidRow(
column(12,
  box(
    title = tagList(icon("triangle-exclamation"), "Data Hazards"),
    id = "data_hazards",
    collapsible = TRUE,
    width = 12,
    solidHeader = TRUE,
    uiOutput("protocol_data_hazards_markdown"),
    checkbox_hazards_module_ui("haz", hazards_df, "Select Data Hazards:"),
    uiOutput("sel"),
    tags$div(style = "margin-top:16px"),
    download_hazards_module_ui("dl")
  )
)
)
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

output$protocol_data_hazards_markdown <- renderUI({
includeMarkdown("markdown/protocol_data_hazards.md")
})

 selected_keys <- checkbox_hazards_module_server("haz", hazards_df)

  output$sel <- renderUI({
    keys <- selected_keys()
    if (!length(keys)) return(tags$em("No hazards selected"))
    titles <- hazards_df$title[match(keys, hazards_df$key)]
    tags$ul(lapply(titles, tags$li))
  })

  download_hazards_module_server("dl", selected_keys, hazards_df)

}

shinyApp(ui, server)