#' epiworldRShiny App Launcher
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @description 
#' Fires up the R Shiny App. You can find more examples and documentation at
#' the package's website: <https://UofUEpiBio.github.io/epiworldRShiny/>.
#' 
#' @import epiworldR
#' @import shinydashboard
#' @importFrom DT dataTableOutput renderDataTable
#' @import ggplot2
#' @import shinycssloaders
#' @importFrom plotly plot_ly add_markers add_segments layout
#' @importFrom shinyjs hidden useShinyjs toggle
#' @importFrom stats aggregate as.formula reshape
#' @importFrom utils write.csv packageVersion
#'
#' @return Loads and opens the R shiny app for the epiworldR package
#' @param ... Currently ignored.
#' @export
#' @name epiworldRShiny
NULL

epiworldR_env <- new.env()

#' Access to the epiworldR environment.
#' 
#' This function is for internal use only.
#' 
#' @return Returns the `epiworldR_env` environment.
#' @export
epiworldRenv <- function() {
  epiworldR_env
}

#' @export
#' @param custom_models_path Optional path to custom model files (see details).
#' @details
#' When `custom_models_path` is specified, the function will look for valid model files
#' at the specified path. These will be added to the list of available models.
#' The function expects R files named `shiny_<model_name>.R` which contain the model.
#' The function will also look for optional Markdown files named `shiny_<model_name>.md`
#' which contain the model description.
#' @rdname epiworldRShiny
epiworldRShiny <- function(custom_models_path = NULL, ...) {

  # If the package is not loaded, load it
  if (!"epiworldRShiny" %in% search()) {
    library(epiworldRShiny)
  }

  models_setup(custom_models_path)

  header <- shinydashboard::dashboardHeader(
    title = shiny::HTML(
      'epiworldR <text style="color: gray; font-size:50%">(beta)</text>'
      )
  )

  # Sets CSS cursor style for headers
  cursor_header_pointer <-
    sprintf(
      "#npis_header_%1$s, #network_header_%1$s, #population_header_%1$s",
      epiworldRenv()$models
    ) |>
    paste0(collapse = ", ") |>
    paste("{\n  cursor: pointer;\n}\n")

  sidebar <- do.call(
    "dashboardSidebar",
    c(
      list(
        width = NULL,
        shiny::tags$style(
          paste0(
            "#sidebarItemExpanded {overflow: auto;max-height: 100vh;}",
            cursor_header_pointer
          )
        ),
        shinyjs::useShinyjs(),
        shiny::selectInput(
          inputId = "model",
          label = shiny::h3("Model"),
          choices = unname(epiworldRenv()$models_names)
        )
      ),
      mget(paste0(epiworldRenv()$models, "_panel"), envir = epiworldRenv())
    )
  )

  # Getting the version of epiworldR
  epiworldRShiny_version <- utils::packageVersion("epiworldRShiny")
  epiworldR_version <- utils::packageVersion("epiworldR")

  body <- shinydashboard::dashboardBody(
    shiny::fluidRow(
      shiny::column(12, shiny::htmlOutput("model_description"))
    ),
    shiny::fluidRow(
      shiny::column(6, plotly::plotlyOutput("model_plot") |> shinycssloaders::withSpinner(color = "#009bff")),
      shiny::column(6, plotly::plotlyOutput("model_reproductive_plot") |> shinycssloaders::withSpinner(color = "#009bff"))
    ),
    shiny::HTML("<br>"),
    shiny::fluidRow(
      shiny::column(6, shiny::verbatimTextOutput("model_summary") |> shinycssloaders::withSpinner(color = "#009bff")),
      shiny::column(6, DT::dataTableOutput("model_table") |> shinycssloaders::withSpinner(color = "#009bff"))
    ),
    shiny::htmlOutput("download_button"),
    shiny::tags$style(type = 'text/css', "#downloadData {position: fixed; bottom: 20px; right: 20px; }"),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::markdown(
          paste(
            "epiworldRShiny version",
            epiworldRShiny_version,
            "| epiworldR version",
            epiworldR_version
            )
          )
      ),
      shiny::column(6, shiny::markdown("**The University of Utah**"))
    )
  )

    ui <- shinydashboard::dashboardPage(
      header = header,
      sidebar = sidebar,
      body = body,
      skin = "black"
    )

  server <- function(input, output, session) {

    for (i in c("npis", "network", "population")) {
      for (m in epiworldRenv()$models) {
        local({
          id0 <- paste0(i, "_header_", m)
          id1 <- paste0(i, "_inputs_", m)
          shinyjs::onclick(id = id0, shinyjs::toggle(id = id1, anim = TRUE))
        })
      }
    }

    model_id <- shiny::reactive(epiworldRenv()$models[input$model])

    model_output <- shiny::eventReactive(
      eventExpr = input[[paste0("simulate_", model_id())]],
      valueExpr = {
        eval({
          modelfun <- get(paste0("shiny_", model_id()), envir = epiworldRenv())
          modelfun(input)
          })
        }
      )

    output$model_description <- shiny::renderText({

      # Reading the model description from the package
      # - First check the prebuilt models
      fn <- system.file(
        "models", paste0("shiny_", model_id(), ".md"),
        package = "epiworldRShiny"
      )

      # If the model is not found in the prebuilt models, check the custom models
      if (!file.exists(fn)) {
        fn <- paste0(custom_models_path, paste0("/shiny_", model_id(), ".md"))
      }

      contents <- if (file.exists(fn))
        readLines(fn, warn = FALSE)
      else
        "No description available."

      shiny::markdown(contents)
    })

    output$model_plot <- plotly::renderPlotly({
      model_output()$epicurves_plot()
    })
    output$model_reproductive_plot <- plotly::renderPlotly({
      model_output()$reproductive_plot()
    })
    output$model_summary <- shiny::renderPrint({
      model_output()$model_summary()
    })
    output$model_table <- DT::renderDataTable({
      model_output()$model_table()
    }, options = list(
      scrollY = '600px',
      lengthMenu = c(16, 25, 50),
      pageLength = 16
    ), escape = FALSE)

    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        paste("data", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(model_output()$model_table(), file)
      }
    )

    output$download_button <- shiny::renderUI({
      shiny::downloadButton("downloadData", "Download Data")
    })
  }
  shiny::shinyApp(ui=ui, server=server)

}


#' @export
#'
#' @rdname epiworldRShiny
#' @description
#' `run_app` is a wrapper for the `epiworldRShiny` function. It is a
#' convenience function to run the app.
run_app <- epiworldRShiny
