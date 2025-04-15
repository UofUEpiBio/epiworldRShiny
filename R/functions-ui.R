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

  bslib::accordion(
    open = FALSE,
    bslib::accordion_panel(
      title = "Network",
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
}

#' @export
#' @return
#' - `npis_input` returns an object of class [shiny::tagList] (`shiny.tag.list`).
#' @rdname epiworldrshiny-ui
#' @examples
#' npis_input("SEIRD")
npis_input <- function(model_name) {

  bslib::accordion(
    open = FALSE,
    bslib::accordion_panel(
      title = "Non-Pharmaceutical Interventions",
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

# Helper function to validate a given model file
#' @noRd
#' @param model_filename The name of the model file to be validated.
#' @return
#' - `validate_model` returns TRUE if the model file is valid, FALSE otherwise.
validate_model <- function(model_filename) {

  validate_env <- new.env()
  file_basename <- basename(model_filename)
  
  # Check if file exists
  if (!file.exists(model_filename)) {
    message(paste("File not found:", file_basename))
    return(FALSE)
  }
  
  # Check if file has valid name
  if (!grepl("^shiny_[a-z]+\\.R$", file_basename)) {
    message(paste("Invalid model file name:", file_basename))
    return(FALSE)
  }

  # Source the file to temp environment and extract function names
  source(model_filename, local = validate_env)

  model_name <- gsub("shiny_([^.]+).R", "\\1", file_basename)
  model_fun_name <- paste0("shiny_", model_name)
  model_panel_name <- paste0(model_name, "_panel")

  # Check if valid model function exists
  if (!exists(model_fun_name, envir = validate_env)) {
    message(paste0(
      file_basename,
      " must have a model function named '",
      model_fun_name,
      "'.")
    )
    return(FALSE)
  }

  # Check if valid panel function exists
  if (!exists(model_panel_name, envir = validate_env)) {
    message(paste0(
      file_basename,
      " must have a panel function named '",
      model_panel_name,
      "'.")
    )
    return(FALSE)
  }

  # Clean up
  validate_env <- NULL

  # Model validated
  return(TRUE)
}

# Helper function to get valid model files from given directory
#' @noRd
#' @param path_to_models The path to the directory containing model files.
#' @return
#' - `get_valid_models` returns a character vector of valid model file paths.
get_valid_models <- function(path_to_models) {
  if (is.null(path_to_models)) {
    return(NULL)
  }

  # Get list of all model files at path
  model_files <- list.files(
    path_to_models,
    pattern = "shiny_[a-z]+\\.R$",
    full.names = TRUE
  )

  # Source each model file
  validated_models <- c()
  for (f in model_files) {
    if (validate_model(f)) {
      validated_models <- c(validated_models, f)
    }
  }

  return(validated_models)
}

# Helper function to remove duplicate file names from custom models
#' @noRd
#' @param custom_models A character vector of custom model file paths.
#' @param system_models A character vector of system model file paths.
#' @return
#' - `remove_duplicates` returns a character vector of custom model file paths
#'   with duplicates removed.
remove_duplicates <- function(custom_models, system_models) {
  # Get duplicates (if any)
  duplicate_models <- custom_models[
    basename(custom_models) %in% basename(system_models)
  ]

  # Remove duplicates from custom models
  custom_models <- custom_models[
    !basename(custom_models) %in% basename(system_models)
  ]

  # Print warning if any duplicates were found
  if (length(custom_models) < length(system_models)) {
    message("Custom model path contains the following duplicated system models which have not been imported:\n")
    message(paste(duplicate_models, collapse = "\n"))
    message("\nOnly unique models will be used, with preference given to system models.")
  }

  return(custom_models)
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

    # Get a list of all valid model files in the "models" directory
    # - Print warnings and don't include invalid files
    system_model_filenames <- get_valid_models(
      path_to_models = system.file("models", package = "epiworldRShiny")
    )

    # Get a list of all valid model files in the "custom file path" directory
    # - Print warnings and don't include invalid files
    custom_model_filenames <- get_valid_models(
      path_to_models = custom_models_path
    )

    all_model_filenames <- system_model_filenames

    num_custom_models <- 0

    # If custom models are provided, remove duplicates and add to total list
    if (!is.null(custom_model_filenames)) {
      custom_model_filenames <- remove_duplicates(custom_model_filenames, system_model_filenames)
      num_custom_models <- length(custom_model_filenames)
      all_model_filenames <- c(custom_model_filenames, system_model_filenames)
    }

    # Source valided models
    for (f in all_model_filenames) {
      source(f, local = epiworldRenv())
    }

    # Capture model display names (for UI)
    model_display_names <- sapply(all_model_filenames, \(f) {

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

    # If model is custom (user-defined), prepend "(custom)" to model name
    if (num_custom_models > 0) {
      for (i in 1:num_custom_models) {
        model_display_names[i] <- paste("(custom)", model_display_names[i], sep = " ")
      }
    }    

    # Get the model names from the file names
    models <- gsub("^.+shiny_([^.]+).R$", "\\1", all_model_filenames)

    # Set the names of the models to their alt names (if available)
    names(model_display_names) <- models
    names(models) <- model_display_names

    # Add the model names and models to the epiworldR environment
    assign("model_display_names", model_display_names, envir = epiworldRenv())
    assign("models", models, envir = epiworldRenv())

    # Generate model panels and add to the epiworldR environment
    model_panels <- mget(paste0(models, "_panel"), envir = epiworldRenv())
    invisible({
      Map(\(pfun, nam, id) {
        assign(paste0(id, "_panel"), pfun(nam), envir = epiworldRenv())
      }, pfun = model_panels, nam = model_display_names, id = models
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

  bslib::accordion(
    open = FALSE,
    bslib::accordion_panel(
      title = "Population",
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

}

#' @export
#' @rdname epiworldrshiny-ui
#' @examples
#' simulate_button("SEIRD")
simulate_button <- function(model_name) {
  bslib::input_task_button(
    id = paste0("simulate_", model_name),
    label = "Run Simulation"
    )
}
