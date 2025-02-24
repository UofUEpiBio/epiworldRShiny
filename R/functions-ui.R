#' epiworldRShiny UI builder functions
#'
#' All functions in this section are for internal use only. They are used to
#' build the UI for the epiworldRShiny app.
#'
#' @param model_name Name of the epiworldR model.
#' @export
#' @return
#' - Unless otherwise specified, returns an object of class shiny.tag.
#' @name epiworldrshiny-ui
#' @examples
#' text_input_disease_name("SEIRD")
text_input_disease_name <- function(model_name) {
  shiny::textInput(
    inputId     = paste0(model_name, "_disease_name"),
    label       = "Disease",
    value       = "",
    placeholder = "Please enter a disease name"
  )
}

#' @rdname epiworldrshiny-ui
#' @export
#' @examples
#' slider_prevalence("SEIRD")
slider_prevalence <- function(model_name) {
  shiny::sliderInput(
    paste0(model_name, "_prevalence"),
    label = "% of population infected",
    value = "0.1",
    min = 0,
    max = 1,
    step = 0.01,
    ticks = FALSE
    )
}

#' @export
#' @rdname epiworldrshiny-ui
#' @examples
#' numeric_input_ndays("SEIRD")
numeric_input_ndays <- function(model_name) {
  shiny::numericInput(
    inputId = paste0(model_name, "_n_days"),
    label   = "Simulation Time (Days)",
    value   = "100",
    min     = 0,
    max     = NA,
    step    = 1
    )
}

#' @param rate_name Name of the rate.
#' @param value Initial value for the slider.
#' @param maxval Maxiumum value for the slider.
#' @param input_label Aids in creating the appropriate slider name.
#' @export
#' @rdname epiworldrshiny-ui
#' @examples
#' slider_input_rate("SEIRD", "transmission", value = 0.3, maxval = 1,
#' input_label = NULL)
slider_input_rate <- function(
  model_name, rate_name, value, maxval = 1, input_label = NULL
  ) {

  if (is.null(input_label)) {
    input_label <- gsub("[^a-z0-9]", "_", tolower(rate_name))
  }

  shiny::sliderInput(
    inputId = paste(
      model_name, input_label,
      sep = "_"
    ),
    label = rate_name,
    value = value,
    min   = 0,
    max   = maxval,
    step  = 0.01,
    ticks = FALSE
  )
}

#' @export
#' @return
#' - `network_input` returns an object of class [shiny::tagList] (`shiny.tag.list`).
#' @rdname epiworldrshiny-ui
#' @examples
#' network_input("SEIRD")
network_input <- function(model_name) {

  shiny::tagList(
    shiny::div(
      id = paste0("network_header_", model_name),
      shiny::headerPanel(
        shiny::h4(
          shiny::tagList(
            shiny::icon("circle-info"),
            "Population structure"
          )
        )
      )
    ),
    shinyjs::hidden(
      shiny::div(
        id = paste0("network_inputs_", model_name),
        shiny::tagList(
          shiny::tags$style("p { padding: 0 20px; }"),
          shiny::p("The below parameters affect the network structure and
                   behavior of agents within the simulation."),
          shiny::sliderInput(
            inputId = paste0(model_name, "_population_size"),
            label   = "Population Size",
            min     = 0,
            max     = 100000,
            value   = 50000,
            step    = 1000,
            ticks   = FALSE
          ),
          shiny::numericInput(
            inputId = paste0(model_name, "_k"),
            label   = "Number of Ties",
            min     = 0,
            max     = 500,
            step    = 1,
            value   = 20
          ),
          shiny::p("The number of
                   agents each individual agent is expected to meet, on
                   average."),
          shiny::selectInput(
            inputId  = paste0(model_name, "_directed"),
            label    = "Directed",
            choices  = c("TRUE", "FALSE"),
            selected = "FALSE"
          ),
          shiny::p("Whether or not an interaction between two agents
                   is one-way or two-way."),
          shiny::sliderInput(
            inputId = paste0(model_name, "_prob_rewiring"),
            label   = "Probability of Rewiring",
            value   = "0.20",
            min     = 0,
            max     = 1,
            step    = 0.01,
            ticks   = FALSE
          ),
           shiny::p("The probability that an agent becomes disconnected from
                    their location within the network, and relocates to another
                    location within the same network")
        )
      )
    )
  )
}

#' @export
#' @return
#' - `npis_input` returns an object of class [shiny::tagList] (`shiny.tag.list`).
#' @rdname epiworldrshiny-ui
#' @examples
#' npis_input("SEIRD")
npis_input <- function(model_name) {
  shiny::tagList(
    shiny::div(
      id = paste0("npis_header_", model_name),
      shiny::headerPanel(
        shiny::h4(
          shiny::tagList(
            shiny::icon("circle-info"),
            "Interventions"
          )
        )
      )
    ),
    shinyjs::hidden(
      shiny::div(
        id = paste0("npis_inputs_", model_name),
        shiny::tagList(
          shiny::tags$style("p { padding: 0 20px; }"),
          tags$p("More details about the implementation of the below
                interventions can be found in the epiworldRShiny",
                 tags$a("reference manual",
                 href = "https://uofuepibio.github.io/epiworldRShiny/reference/index.html"),
                 "."
                 ),
          shiny::headerPanel(shiny::h4("Vaccination")),
          shiny::sliderInput(
            inputId = paste0(model_name, "_vaccine_prevalence"),
            label   = "% of agents vaccinated",
            min     = 0,
            max     = 1,
            value   = 0,
            step    = 0.01,
            ticks   = FALSE
          ),
          shiny::sliderInput(
            inputId = paste0(model_name, "_vaccine_susceptibility_reduction"),
            label   = "probability reduction of susceptibility",
            min     = 0,
            max     = 1,
            value   = 0,
            step    = 0.01,
            ticks   = FALSE
          ),
          shiny::sliderInput(
            inputId = paste0(model_name, "_vaccine_transmission_reduction"),
            label   = "probability reduction of transmission",
            min     = 0,
            max     = 1,
            value   = 0,
            step    = 0.01,
            ticks   = FALSE
          ),
          shiny::sliderInput(
            inputId = paste0(model_name, "_vaccine_recovery_enhancer"),
            label   = "probability increase of recovery",
            min     = 0,
            max     = 1,
            value   = 0,
            step    = 0.01,
            ticks   = FALSE
          ),
          shiny::sliderInput(
            inputId = paste0(model_name, "_vaccine_death_reduction"),
            label   = "probability reduction of death",
            min     = 0,
            max     = 1,
            value   = 0,
            step    = 0.01,
            ticks   = FALSE
          ),
          shiny::headerPanel(shiny::h4("Masking")),
          shiny::sliderInput(
            inputId = paste0(model_name, "_masking_prevalence"),
            label   = "% of agents using masks",
            value   = "0",
            min     = 0,
            max     = 1,
            step    = 0.01,
            ticks   = FALSE
          ),
          shiny::sliderInput(
            inputId = paste0(model_name, "_masking_transmission_reduction"),
            label   = "probability reduction of transmission",
            value   = "0",
            min     = 0,
            max     = 1,
            step    = 0.01,
            ticks   = FALSE
          ),
          shiny::headerPanel(shiny::h4("School Closure")),
          shiny::sliderInput(
            inputId = paste0(model_name, "_school_closure_prevalence"),
            label   = "prevalence",
            value   = "0",
            min     = 0,
            max     = 1,
            step    = 0.01,
            ticks   = FALSE
          ),
          shiny::numericInput(
            inputId = paste0(model_name, "_school_closure_day"),
            label   = "implementation day",
            value = "0",
            min = 0,
            max = 100,
            step = 1
          ),
          shiny::sliderInput(
            inputId = paste0(model_name, "_school_closure_transmission_reduction"),
            label   = "probability reduction of transmission",
            value   = "0",
            min     = 0,
            max     = 1,
            step    = 0.01,
            ticks   = FALSE
          )
        )
      )
    )
  )
}


#' @export
#' @rdname epiworldrshiny-ui
#' @examples
#' seed_input("SEIRD")
seed_input <- function(model_name) {
  shiny::numericInput(
    inputId = paste0(model_name, "_seed"),
    label   = "Seed (Optional)",
    min     = 0,
    max     = NA,
    step    = 1,
    value   = 2023
    )
}

#' @export
#' @param custom_models_path Optional path to custom model files (see details).
#' @details
#' When `custom_models_path` is specified, the function will look for valid model files
#' at the specified path. These will be added to the list of available models.
#' The function expects R files named `shiny_<model_name>.R` which contain the model.
#' @return
#' - `models_setup` returns an object of class list.
#' @rdname epiworldrshiny-ui
#' @examples
#' # Setup with default models only:
#' models_setup()
#' # Setup with default and custom models:
#' \dontrun{models_setup(custom_models_path = "path/to/custom/models")}
models_setup <- function(custom_models_path = NULL) {

  # Getting the environment
  env <- parent.frame()

  eval({

    # Get a list of all model files in the "models" directory
    models <- list.files(
      system.file("models", package = "epiworldRShiny"),
      pattern = "shiny_[a-z]+\\.R$",
      full.names = TRUE
      )


    # Read in custom models
    num_custom_models <- 0
    if (!is.null(custom_models_path)) {
      custom_models <- list.files(
        custom_models_path,
        pattern = "shiny_[a-z]+\\.R$",
        full.names = TRUE
        )

      num_custom_models <- length(custom_models)
      models <- c(custom_models, models)
    }

    # Source each model file
    for (f in models) {
      source(f, local = epiworldRenv())
    }

    # Capturing alt names
    models_names <- sapply(models, \(f) {

      # Only on the first line
      altname <- readLines(f, n = 1)

      # Is there any alt name?
      altname <- gsub("^#\\s*alt-name[:]\\s*(.+)\\s*$", "\\1", altname, perl = TRUE)

      # If there is no alt name, use the file name
      if (altname == "") {
        altname <- toupper(gsub("shiny_([^.]+).R", "\\1", basename(f)))
      }
      altname

    })

    # If model is custom (user-defined), prepend "(custom)"
    if (num_custom_models > 0) {
      for (i in 1:num_custom_models) 
        models_names[i] <- paste("(custom)", models_names[i], sep = " ")
    }    

    # Get the model names from the file names
    models <- gsub("^.+shiny_([^.]+).R$", "\\1", models)

    # Set the names of the models to their alt names (if available)
    names(models_names) <- models
    names(models) <- models_names

    # Add the model names and models to the epiworldR environment
    assign("models_names", models_names, envir = epiworldRenv())
    assign("models", models, envir = epiworldRenv())

    # Doing some hacking
    models_panels <- mget(paste0(models, "_panel"), envir = epiworldRenv())
    invisible({
      Map(\(pfun, nam, id) {
        assign(paste0(id, "_panel"), pfun(nam), envir = epiworldRenv())
      }, pfun = models_panels, nam = models_names, id = models
      )
    })

  }, envir = env)

}

#' @export
#' @return
#' - `population_input` returns an object of class shiny.tag.list.
#' @rdname epiworldrshiny-ui
#' @examples
#' population_input("SEIRD")
population_input <- function(model_name) {
  shiny::tagList(
    shiny::div(
      id = paste0("population_header_", model_name),
      shiny::headerPanel(
        shiny::h4(
          shiny::tagList(
            shiny::icon("circle-info"),
            "Population (equity)"
            )
      ))
    ),
    shinyjs::hidden(
      shiny::div(
        id = paste0("population_inputs_", model_name),
        shiny::sliderInput(
          inputId = paste0(model_name, "_prop_hispanic"),
          label   = "% Hispanic",
          value   = "0.5",
          min     = 0,
          max     = 1,
          step    = 0.01,
          ticks   = FALSE
          ),
        shiny::sliderInput(
          inputId = paste0(model_name, "_prop_female"),
          label   = "% Female",
          value   = "0.5",
          min     = 0,
          max     = 1,
          step    = 0.01,
          ticks   = FALSE
          ),
        shiny::sliderInput(
          inputId = paste0(model_name, "_prop_ages"),
          min     = 0,
          max     = 1,
          value   = c(.3, .6),
          label   = "Age distribution (< 20, < 60, 60+)",
          dragRange = TRUE
        )
      )
    )
  )

}

#' @export
#' @rdname epiworldrshiny-ui
#' @examples
#' simulate_button("SEIRD")
simulate_button <- function(model_name) {
  shiny::actionButton(
    inputId = paste0("simulate_", model_name),
    label   = "Run Simulation"
    )
}
