#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom rmarkdown render
#' @noRd
app_server <- function(input, output, session) {
  r_local <- reactiveValues(
    report_render_iteration = 0
  )

  temp_paths <- initialize_render_temp_dir(
    session = session
  )
  resource_path_prefix <- "temp_dir_render_html"
  addResourcePath(
    prefix = resource_path_prefix,
    directoryPath = temp_paths$path_temp_dir
  )

  observeEvent(
    input$dataset,
    {
      r_local$path_html_report <- rmarkdown::render(
        input = temp_paths$path_temp_template,
        params = list(dataset = input$dataset)
      )
      r_local$report_render_iteration <- r_local$report_render_iteration + 1
    }
  )

  output$report <- renderUI({
    r_local$report_render_iteration
    tags$iframe(
      src = file.path(resource_path_prefix, "template.html"),
      height = "1000px",
      width = "800px"
    )
  })
}


initialize_render_temp_dir <- function(session = shiny::getDefaultReactiveDomain()) {
  path_temp_dir <- tempfile(pattern = "report_")
  dir.create(path_temp_dir)
  path_temp_template <- file.path(
    path_temp_dir,
    "template.Rmd"
  )
  file.copy(
    from = app_sys("template.Rmd"),
    to = path_temp_template
  )

  # Cleanup the temp folder when the app exits
  session$onSessionEnded(function() {
    unlink(path_temp_dir, recursive = TRUE)
  })

  list(
    path_temp_dir = path_temp_dir,
    path_temp_template = path_temp_template
  )
}
