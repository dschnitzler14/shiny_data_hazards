get_hazard_sections <- function(key) {
  url <- sprintf("https://datahazards.com/hazards/%s.html", key)
  doc <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(doc)) return(NULL)

  grab <- function(id) {
    sec <- html_element(doc, paste0("section#", id))
    if (is.na(sec)) return(NULL)
    html_elements(sec, "a.headerlink") |> xml_remove()
    HTML(as.character(sec))
  }

  list(
    url         = url,
    description = grab("description"),
    examples    = grab("examples"),
    safety      = grab("safety-precautions")
  )
}

checkbox_hazards_module_ui <- function(id, hazards_df, label = "Select Data Hazards:",
                                       selected = NULL) {
  stopifnot(is.data.frame(hazards_df), all(c("key","title","img","alt") %in% names(hazards_df)))
  ns <- NS(id)

  items <- lapply(seq_len(nrow(hazards_df)), function(i) {
    k <- hazards_df$key[i]
    card <- actionLink(
      inputId = ns(paste0("open_", k)),
      label = tags$div(
        class = "hazard-card",
        tags$img(src = hazards_df$img[i], alt = hazards_df$alt[i], class = "hazard-img"),
        tags$div(class = "hazard-title", hazards_df$title[i])
      ),
      class = "hazard-card-link"
    )

    cb <- checkboxInput(
      inputId = ns(paste0("cb_", k)),
      label   = NULL,
      value   = !is.null(selected) && k %in% selected
    )

    tags$div(class = "hazard-item", `data-key` = k, card, tags$div(class = "hazard-checkbox", cb))
  })

  tagList(
    singleton(tags$head(tags$link(rel = "stylesheet", href = "hazards.css"))),
    tags$hr(class = "hazard-divider"),
    tags$div(class = "hazard-group-label", label),
    tags$div(class = "hazard-grid", items),
    tags$hr(class = "hazard-divider")
  )
}

checkbox_hazards_module_server <- function(id, hazards_df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    lapply(hazards_df$key, function(k) {
      local({
        kk <- k
        row <- hazards_df[hazards_df$key == kk, , drop = FALSE]
        ttl <- row$title
        img <- row$img
        alt <- row$alt

observeEvent(input[[paste0("open_", kk)]], {
  idx <- match(kk, hazards_df$key)
  validate(need(!is.na(idx), "Hazard not found"))
  row <- hazards_df[idx, , drop = FALSE]

  ttl <- as.character(row$title[[1]])
  img <- as.character(row$img[[1]])
  alt <- as.character(row$alt[[1]])

  showModal(modalDialog(title = ttl, "Loading…", easyClose = TRUE, footer = NULL))

  page <- get_hazard_sections(kk)

  removeModal()
  if (is.null(page)) {
    showModal(modalDialog(
      title = ttl,
      tags$div(class = "hazard-modal",
        tags$img(src = img, alt = alt, class = "hazard-modal-img"),
        tags$div(class = "hazard-modal-body",
          tags$h5("Description"),
          tags$p("Couldn’t load extended content right now."),
          tags$p(tags$a(href = sprintf("https://datahazards.com/hazards/%s.html", kk),
                        target = "_blank", "Open full page"))
        )
      ),
      easyClose = TRUE, footer = modalButton("Close"), size = "l"
    ))
    return(invisible())
  }

  showModal(modalDialog(
    title = ttl,
    tags$div(class = "hazard-modal",
      tags$img(src = img, alt = alt, class = "hazard-modal-img"),
      tags$div(class = "hazard-modal-body",
        page$description,
        page$examples,
        page$safety,
        tags$p(tags$a(href = page$url, target = "_blank", "Open full page"))
      )
    ),
    easyClose = TRUE,
    footer = modalButton("Close"),
    size = "l"
  ))
}, ignoreInit = TRUE)

      })
    })

    reactive({
      sel <- vapply(hazards_df$key, function(k) isTRUE(input[[paste0("cb_", k)]]), logical(1))
      hazards_df$key[sel]
    })
  })
}