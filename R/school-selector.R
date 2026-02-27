#' Validate an uploaded school CSV data frame
#'
#' Checks required columns, data types, value ranges, and row count.
#'
#' @param data A data.frame to validate.
#' @param required_cols Character vector of required column names.
#' @param max_rows Maximum allowed rows.
#' @return A list with components \code{valid} (logical) and \code{message}
#'   (character or NULL).
#' @noRd
validate_school_csv <- function(data, required_cols, max_rows) {

  if (!all(required_cols %in% colnames(data))) {
    return(list(
      valid   = FALSE,
      message = paste("CSV must contain columns:",
                      paste(required_cols, collapse = ", "))
    ))
  }

  if ("vaccination_rate" %in% required_cols) {
    if (!is.numeric(data$vaccination_rate) ||
        any(data$vaccination_rate < 0 | data$vaccination_rate > 1,
            na.rm = TRUE)) {
      return(list(
        valid   = FALSE,
        message = "vaccination_rate must be numeric values between 0 and 1"
      ))
    }
  }

  if ("num_students" %in% required_cols) {
    if (!is.numeric(data$num_students) ||
        any(data$num_students < 0 | data$num_students > 50000,
            na.rm = TRUE)) {
      return(list(
        valid   = FALSE,
        message = "num_students must be numeric values between 0 and 50000"
      ))
    }
  }

  if (nrow(data) > max_rows) {
    return(list(
      valid   = FALSE,
      message = sprintf(
        "CSV contains too many schools. Maximum %d schools allowed.", max_rows
      )
    ))
  }

  list(valid = TRUE, message = NULL)
}

#' Cascading school selector server logic
#'
#' Registers Shiny observers for a state -> county -> school cascading
#' dropdown set, including CSV upload with validation and a reset button.
#' The function is parameterized by \code{prefix} so that multiple models
#' can each have independent selector instances.
#'
#' Expected input IDs (created by the caller's UI):
#' \code{{prefix}_state_selector}, \code{{prefix}_county_selector},
#' \code{{prefix}_school_selector}, \code{{prefix}_school_csv},
#' \code{{prefix}_reset_school_data}.
#'
#' @param input   The Shiny \code{input} object.
#' @param session The Shiny \code{session} object.
#' @param prefix  Character prefix for input IDs, typically the model name
#'   (e.g. \code{"measles"}).
#' @param default_csv Path to the default CSV file (already resolved, e.g.
#'   via \code{\link[base]{system.file}}).
#' @param required_cols Character vector of required CSV column names.
#' @param on_school_select Callback with signature
#'   \code{function(school_row, session, prefix)} invoked when a valid
#'   school is selected.  Use this to populate model-specific form fields.
#' @param max_file_size_mb Maximum upload size in megabytes (default 5).
#' @param max_rows Maximum rows allowed in an uploaded CSV (default 10000).
#'
#' @return The \code{\link[shiny]{reactiveVal}} holding the current school
#'   data frame (invisibly).
#'
#' @export
school_selector_server <- function(
    input,
    session,
    prefix,
    default_csv,
    required_cols = c("state", "county", "school_name", "school_id",
                      "vaccination_rate", "num_students"),
    on_school_select,
    max_file_size_mb = 5,
    max_rows = 10000
) {

  # -- helpers (closed over prefix + session) --------------------------------

  pid <- function(suffix) paste0(prefix, "_", suffix)

  reset_dropdown <- function(inputId) {
    shiny::updateSelectInput(
      session  = session,
      inputId  = inputId,
      choices  = c("Select..." = ""),
      selected = ""
    )
  }

  # -- shared reactive state (survives renderUI re-fires) --------------------

  ud_key <- paste0(prefix, "_school_data")
  if (is.null(session$userData[[ud_key]])) {
    session$userData[[ud_key]] <- shiny::reactiveVal(NULL)
  }
  school_data <- session$userData[[ud_key]]

  # -- load default data -----------------------------------------------------

  load_default <- function() {
    if (file.exists(default_csv)) {
      data <- utils::read.csv(default_csv, stringsAsFactors = FALSE)
      school_data(data)
      states <- sort(unique(data$state))
      shiny::updateSelectInput(
        session = session,
        inputId = pid("state_selector"),
        choices = c("Select..." = "", states)
      )
    }
  }

  # -- one-time observer registration ----------------------------------------

  guard_key <- paste0(prefix, "_school_selector_registered")
  if (!isTRUE(session$userData[[guard_key]])) {
    session$userData[[guard_key]] <- TRUE

    load_default()

    # Reset button
    shiny::observeEvent(input[[pid("reset_school_data")]], {
      load_default()
      reset_dropdown(pid("county_selector"))
      reset_dropdown(pid("school_selector"))
      shiny::showNotification("Reset to default school data", type = "message")
    })

    # CSV upload
    shiny::observeEvent(input[[pid("school_csv")]], {
      req(input[[pid("school_csv")]])

      tryCatch({
        file_info <- input[[pid("school_csv")]]

        if (!grepl("\\.csv$", file_info$name, ignore.case = TRUE)) {
          shiny::showNotification(
            "Invalid file type. Please upload a file with a .csv extension.",
            type = "error"
          )
          return()
        }

        if (file_info$size > max_file_size_mb * 1024 * 1024) {
          shiny::showNotification(
            sprintf("CSV file size must be less than %dMB", max_file_size_mb),
            type = "error"
          )
          return()
        }

        data <- utils::read.csv(file_info$datapath, stringsAsFactors = FALSE)

        validation <- validate_school_csv(data, required_cols, max_rows)
        if (!validation$valid) {
          shiny::showNotification(validation$message, type = "error")
          return()
        }

        school_data(data)

        states <- sort(unique(data$state))
        shiny::updateSelectInput(
          session  = session,
          inputId  = pid("state_selector"),
          choices  = c("Select..." = "", states),
          selected = ""
        )
        reset_dropdown(pid("county_selector"))
        reset_dropdown(pid("school_selector"))

        shiny::showNotification(
          "School data loaded successfully!", type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("Error reading CSV file:", e$message), type = "error"
        )
      })
    })

    # State -> counties
    shiny::observeEvent(input[[pid("state_selector")]], {
      state_val <- input[[pid("state_selector")]]
      if (!is.null(state_val) && state_val != "" &&
          !is.null(school_data())) {

        filtered <- school_data()[school_data()$state == state_val, ]
        if (nrow(filtered) > 0) {
          counties <- sort(unique(filtered$county))
          shiny::updateSelectInput(
            session  = session,
            inputId  = pid("county_selector"),
            choices  = c("Select..." = "", counties),
            selected = ""
          )
        } else {
          reset_dropdown(pid("county_selector"))
        }
        reset_dropdown(pid("school_selector"))
      } else {
        reset_dropdown(pid("county_selector"))
        reset_dropdown(pid("school_selector"))
      }
    })

    # County -> schools
    shiny::observeEvent(input[[pid("county_selector")]], {
      county_val <- input[[pid("county_selector")]]
      state_val  <- input[[pid("state_selector")]]

      if (!is.null(county_val) && county_val != "" &&
          !is.null(state_val)  && state_val  != "" &&
          !is.null(school_data())) {

        data <- school_data()
        filtered <- data[
          (data$state == state_val) & (data$county == county_val), ]

        if (nrow(filtered) > 0) {
          school_choices <- stats::setNames(
            filtered$school_id,
            filtered$school_name
          )
          shiny::updateSelectInput(
            session  = session,
            inputId  = pid("school_selector"),
            choices  = c("Select..." = "", school_choices),
            selected = ""
          )
        } else {
          reset_dropdown(pid("school_selector"))
        }
      } else {
        reset_dropdown(pid("school_selector"))
      }
    })

    # School -> callback
    shiny::observeEvent(input[[pid("school_selector")]], {
      school_val <- input[[pid("school_selector")]]
      state_val  <- input[[pid("state_selector")]]
      county_val <- input[[pid("county_selector")]]

      if (!is.null(school_val) && school_val != "" &&
          !is.null(state_val)  && state_val  != "" &&
          !is.null(county_val) && county_val != "" &&
          !is.null(school_data())) {

        data <- school_data()
        school_row <- data[data$school_id == school_val, ]

        if (nrow(school_row) == 1) {
          if (!is.na(school_row$num_students) &&
              !is.na(school_row$vaccination_rate)) {
            on_school_select(school_row, session, prefix)
          } else {
            shiny::showNotification(
              "Selected school has invalid data", type = "error"
            )
          }
        } else if (nrow(school_row) > 1) {
          shiny::showNotification(
            "Multiple schools found with this ID. Please contact support.",
            type = "error"
          )
        } else {
          shiny::showNotification(
            "School not found in database", type = "error"
          )
        }
      }
    })

  } # end one-time observer registration

  invisible(school_data)
}
