# load data
load(file = here::here("shinyData.RData"))

smooth <- function(x, sigma = 0) {
  if (sigma == 0) return(x)
  
  shift_vector <- function(vec, n) {
    len <- length(vec)
    if (n > 0) {
      vec <- c(rep(0, n), vec[1:(len - n)])
    } else if (n < 0) {
      vec <- c(vec[(abs(n) + 1):len], rep(0, abs(n)))
    }
    vec
  }
  
  xx <- as.integer(-3 * sigma):as.integer(3 * sigma)
  weights <- dnorm(xx, mean = 0, sd = sigma)
  weights <- weights / sum(weights)
  y <- 0 * x
  for (k in seq_along(weights)) {
    y <- y + weights[k] * shift_vector(x, xx[k])
  }
  
  y
}

# pk <- "CohortCharacteristics"
# dependencies <- dependencies |>
#   dplyr::filter(.data$from %in% .env$pk | .data$to %in% .env$pk)
# # Example data: A directed network with 5 nodes
# tidyverse <- c(
#   "broom", "cli", "dbplyr", "dplyr", "dtplyr", "forcats", "ggplot2", 
#   "googledrive", "googlesheets4", "haven", "hms", "httr", "jsonlite", 
#   "lubridate", "magrittr", "modelr", "pillar", "purrr", "readr", "readxl",
#   "reprex", "rlang", "rstudioapi", "rvest", "stringr", "tibble", "tidyr", "xml2"
# )
# nodes <- dependencies|>
#   dplyr::select(name = "from") |>
#   dplyr::union_all(dplyr::select(dependencies, name = "to")) |>
#   dplyr::distinct() |>
#   dplyr::mutate(type = dplyr::case_when(
#     .data$name %in% .env$allPkgs$package_name ~ "omopverse",
#     .data$name %in% .env$tidyverse ~ "tidyverse",
#     .default = "other"
#   ))
# 
# edges <- dependencies
# 
# # Create a graph object
# graph <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
# 
# ggraph::ggraph(graph, layout = "fr") +  # "fr" = Fruchterman-Reingold layout
#   ggraph::geom_node_point(
#     ggplot2::aes(color = type),  # Node color from data
#     size = 5,           # Node size
#     shape = 19           # 19 = filled circle
#   ) +
#   ggraph::geom_edge_link(
#     ggplot2::aes(color = type),  # Edge/arrow color from data
#     arrow = ggplot2::arrow(
#       type = "closed",         # Arrow type ("open", "closed")
#       length = ggplot2::unit(0.01, "npc")  # Arrow size
#     ),
#     edge_width = 0.25           # Line thickness
#   ) +
#   ggraph::scale_edge_color_manual(values = c("Imports" = "black", "Suggests" = "gray70")) +
#   ggraph::geom_node_text(ggplot2::aes(label = name)) +
#   ggplot2::theme_void() 

# ui
ui <- bslib::page_navbar(
  theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
  title = "Packages Dashboard",
  bslib::nav_panel(
    title = "Activity",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        shinyWidgets::pickerInput(
          inputId = "activity_packages", 
          label = "Select packages",
          choices = unique(activity$package_name), 
          selected = unique(activity$package_name),
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = "activity_type", 
          label = "Activity",
          choices = unique(activity$activity), 
          selected = unique(activity$activity)[1],
          multiple = FALSE
        ),
        shiny::checkboxInput(
          inputId = "activity_releases", 
          label = "Show releases", 
          value = TRUE
        ),
        shiny::sliderInput(
          inputId = "activity_slider",
          label = "Smooth",
          value = 0,
          min = 0,
          max = 30,
          step = 1
        )
      ),
      bslib::card(
        bslib::card_header("Activity"),
        plotly::plotlyOutput("activity_plot")
      )
    )
  )
)

# server
server <- function(input, output) {
  output$activity_plot <- plotly::renderPlotly({
    x <- activity |>
      dplyr::filter(
        .data$activity == input$activity_type,
        .data$package_name %in% input$activity_packages
      ) |>
      dplyr::group_by(.data$package_name) |>
      dplyr::arrange(.data$date) |>
      dplyr::mutate(count = smooth(x = .data$count, sigma = input$activity_slider)) |>
      dplyr::ungroup()
    p <- ggplot2::ggplot(
      data = x, mapping = ggplot2::aes(x = date, y = count, colour = package_name, fill = package_name)
    ) +
      ggplot2::geom_line()
    plotly::ggplotly(p)
  })
}

shiny::shinyApp(ui, server)
