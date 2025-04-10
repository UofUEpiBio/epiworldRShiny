#' epiworldRShiny App Launcher
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @description 
#' Fires up the R Shiny App. You can find more examples and documentation at
#' the package's website: <https://UofUEpiBio.github.io/epiworldRShiny/>.
#' 
#' @import epiworldR
#' @importFrom DT dataTableOutput renderDataTable
#' @import ggplot2
#' @importFrom plotly plot_ly add_markers add_segments layout
#' @importFrom stats aggregate as.formula reshape
#' @importFrom utils write.csv packageVersion

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

  header <- shiny::HTML(
    'epiworldR <text style="color: gray; font-size:50%">(beta)</text>'
  )

  # Sets CSS cursor style for headers
  cursor_header_pointer <-
    sprintf(
      "#npis_header_%1$s, #network_header_%1$s, #population_header_%1$s, #advanced_header_%1$s",
      epiworldRenv()$models
    ) |>
    paste0(collapse = ", ") |>
    paste("{\n  cursor: pointer;\n}\n")

  sidebar <- do.call(
    bslib::sidebar,
    c(
      list(
        width = 300,
        # shinyjs::useShinyjs(),
        shiny::selectInput(
          inputId = "model",
          label = shiny::h3("Model"),
          choices = unname(epiworldRenv()$model_display_names)
        )
      ),
      # Need to pass it unnamed
      unname(
        mget(paste0(epiworldRenv()$models, "_panel"), envir = epiworldRenv())
        )
    )
  )

  # Getting the version of epiworldR
  epiworldRShiny_version <- utils::packageVersion("epiworldRShiny")
  epiworldR_version <- utils::packageVersion("epiworldR")

  # Footer
  foot <- shiny::div(
    shiny::markdown(
      paste(
        "epiworldRShiny version",
        epiworldRShiny_version,
        "| epiworldR version",
        epiworldR_version
      )
    ),
    shiny::markdown("**The University of Utah**"),
    style="font-size:80%;text-align: center;"
  )
  
  body <- shiny::mainPanel(# shinydashboard::dashboardBody(
    shiny::uiOutput("model_body"),
    shiny::htmlOutput("download_button"),
    shiny::tags$style(type = 'text/css', "#downloadData {position: fixed; bottom: 20px; right: 20px; }"),
    foot
  )

  link_epiworldr <- shiny::a(
    shiny::icon("github"), "epiworldR",
    href = "https://github.com/UofUEpiBio/epiworldR",
    target = "_blank"
  )

  link_epiworldrshiny <- shiny::a(
    shiny::icon("github"), "epiworldRShiny",
    href = "https://github.com/UofUEpiBio/epiworldRShiny",
    target = "_blank"
  )

  # Rendering the NEWS file
  news_file <- system.file(
    "NEWS.md",
    package = "epiworldRShiny"
  ) |> readLines() |> shiny::markdown()

  ui <- bslib::page_navbar( # shinydashboard::dashboardPage(
    bslib::nav_panel(title = "Home", body),
    bslib::nav_spacer(),
    bslib::nav_menu(
      title = "Links",
      align = "right",
      bslib::nav_item(link_epiworldr),
      bslib::nav_item(link_epiworldrshiny)
    ),
    bslib::nav_panel(title = "News", news_file),
    title        = header,
    sidebar      = sidebar,
    window_title = "epiworldRShiny: An R Shiny App for epiworldR",
    lang         = "en"
  )

  server <- function(input, output, session) {

    # Getting the model ID
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

    output$model_body <- shiny::renderUI({
      # If the user has a function to create the body
      if (exists(paste0("body_", model_id()), envir = epiworldRenv())) {

        body_fun <- get(paste0("body_", model_id()), envir = epiworldRenv())
        body_fun(input, model_output, output)

      } else {

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

        shiny::tagList(
          shiny::fluidRow(
            shiny::column(12, shiny::htmlOutput("model_description"))
          ),
          shiny::fluidRow(
            shiny::column(6, plotly::plotlyOutput("model_plot")),
            shiny::column(6, plotly::plotlyOutput("model_reproductive_plot"))
          ),
          shiny::HTML("<br>"),
          shiny::fluidRow(
            shiny::column(6, shiny::verbatimTextOutput("model_summary")),
            shiny::column(6, DT::dataTableOutput("model_table"))
          )
        )
      }
    })  

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
