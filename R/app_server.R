#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom rmarkdown render
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  r_local <- reactiveValues(
    path_temp_dir = NULL
  )

  observeEvent(
    input$dataset,
    {
      path_temp_dir <- tempfile(pattern = "report")
      dir.create(path_temp_dir)
      path_temp_template <- file.path(
        path_temp_dir,
        "template.Rmd"
      )
      file.copy(
        from = app_sys("template.Rmd"),
        to = path_temp_template
      )
      path_html_report <- rmarkdown::render(
        input = path_temp_template,
        params = list(dataset = input$dataset)
      )

      r_local$path_temp_dir <- path_temp_dir
    }
  )

  output$report <- renderUI({
    addResourcePath(
      prefix = "temp_dir_render_html",
      directoryPath = r_local$path_temp_dir
    )
    tags$iframe(
      src = "temp_dir_render_html/template.html",
      height = "1000px",
      width = "800px"
    )
  })
}
