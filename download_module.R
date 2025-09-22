download_hazards_module_ui <- function(id, label = "Download selected hazards as markdown") {
  ns <- NS(id)
  tagList(
    tags$div(style = "margin-top:16px"),
    downloadButton(ns("dl_md"), label)
  )
}

download_hazards_module_server <- function(id, selected_keys, hazards_df) {
  stopifnot(is.reactive(selected_keys))
  moduleServer(id, function(input, output, session) {

    hazards_df <- as.data.frame(hazards_df, stringsAsFactors = FALSE)
    hazards_df$key <- as.character(hazards_df$key)
    hazards_df <- hazards_df[!duplicated(hazards_df$key), , drop = FALSE]

    row_for <- function(k) {
  idx <- match(k, hazards_df$key); if (is.na(idx)) return(NULL)
  title <- gsub("\\|", "\\\\|", as.character(hazards_df$title[[idx]]))
  img   <- as.character(hazards_df$img[[idx]])

  icon_cell <- sprintf("![](%s){height=28px}", img)

  paste("|", icon_cell, "|", title, "|", "", "|", "", "|")
}


    output$dl_md <- downloadHandler(
      filename = function() paste0("data-hazards-self-assessment-", Sys.Date(), ".md"),
      contentType = "text/markdown",
      content = function(file) {
        ks <- selected_keys(); req(length(ks) > 0)

        header <- c(
          "# Data Hazards Self-Assessment: Project Name",
          "First Author1 and Second Author2",
          "1Address of first author",
          "2Address of second author",
          "",
          "## PROJECT OVERVIEW",
          "This is a brief introduction to your project. It might include links to places where more information about the project is available. You could also reference some other work here [1].",
          "",
          "## DATA HAZARDS ASSESSMENT",
          "| Icon | Hazard | Reasoning | Safety Precautions |",
          "|------|--------|-----------|--------------------|"
        )

        rows <- unlist(lapply(ks, row_for), use.names = FALSE)
        writeLines(c(header, rows, ""), file, useBytes = TRUE)
      }
    )
  })
}
