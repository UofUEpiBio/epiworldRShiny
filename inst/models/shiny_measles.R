# alt-name: Measles in Schools

model_builder <- function(input, quarantine = TRUE) {
  epiworldR::ModelMeaslesQuarantine(
    n                      = as.integer(input$measles_population_size),
    contact_rate           = input$measles_contact_rate,
    prevalence             = as.integer(input$measles_prevalence),
    transmission_rate      = input$measles_transmission_rate,
    vax_efficacy           = input$measles_vax_efficacy,
    vax_improved_recovery  = input$measles_vax_improved_recovery,
    incubation_period      = input$measles_incubation_days,
    prodromal_period       = input$measles_prodromal_period,
    rash_period            = input$measles_rash_period,
    days_undetected        = input$measles_days_undetected,
    hospitalization_rate   = input$measles_hospitalization_rate,
    hospitalization_period = input$measles_hospitalization_duration,
    prop_vaccinated        = input$measles_prop_vaccinated,
    quarantine_period      = if (quarantine)
      input$measles_quarantine_days
    else -1L,
    quarantine_willingness = input$measles_quarantine_willingness,
    isolation_period       = input$measles_isolation_days
  )
}

#' Generates a string with the corresponding CI
#' @param x A vector of numbers
#' @param lb,ub Lower and upper bounds of the CI.
get_ci_pretty <- function(x, lb = .025, ub = .975) {
  sprintf("[%1.0f, %1.0f]", quantile(x, lb), quantile(x, ub))
}

format_counts <- function(histories) {
  exposed <- c(
    active_cases_statuses,
    "Isolated Recovered",
    "Recovered"
  )

  counts <- subset(
    histories, (state %in% exposed) & (date == max(date))
  )

  counts <- stats::aggregate(counts ~ sim_num, data = counts, FUN = sum)
  colnames(counts) <- c("Simulation", "Total")
  return(counts)
}

tabulator <- function(no_quarantine_histories, quarantine_histories) {

  no_quarantine_counts <- format_counts(no_quarantine_histories)
  quarantine_counts <- format_counts(quarantine_histories)

  sizes <- c(2, 10, 25, 50, 80)

  outbreak_table <- data.frame(
    "Outbreak Size" = sprintf("≥ %1.0f cases", sizes),
    "Probability WITHOUT Quarantine" = sapply(sizes, \(x) {
      prob <- sum(no_quarantine_counts$Total >= x)/nrow(no_quarantine_counts)

      ifelse(
        prob <= 0.01,
        "< 1%",
        sprintf("%1.0f%%", prob * 100)
      )
    }),
    "Probability WITH Quarantine" = sapply(sizes, \(x){
      prob <- sum(quarantine_counts$Total >= x)/nrow(quarantine_counts)

      ifelse(
        prob <= 0.01,
        "< 1%",
        sprintf("%1.0f%%", prob * 100)
      )
    }),
    check.names = FALSE
  )

  outbreak_table
}

get_takehome_stats <- function(histories_no_quarantine, histories) {

  no_quarantine_counts <- format_counts(histories_no_quarantine)
  quarantine_counts <- format_counts(histories)

  no_quarantine_mean_cases <- mean(no_quarantine_counts$Total, probs = .5)
  quarantine_mean_cases <- mean(quarantine_counts$Total, probs = .5)

  list(
    no_quarantine_mean_cases = no_quarantine_mean_cases,
    quarantine_mean_cases = quarantine_mean_cases
  )
}

#' Analyzes the hospitalizations
#' @param transitions A data frame with the transitions
#' @return A list with the mean, lower and upper bounds of the
#' hospitalizations
analyze_hospitalizations <- function(transitions) {

  # Counting hospitalizations
  transitions <- subset(
    transitions,
    counts > 0 &
    (!grepl("Hospitalized", from) & grepl("Hospitalized", to)) &
    from != to
    )

  # Aggregating
  transitions <- stats::aggregate(
    counts ~ sim_num,
    data = transitions,
    FUN = sum
  )

  # Computing the number of hospitalizations
  list(
    mean = mean(transitions$counts),
    lb   = quantile(transitions$counts, .025),
    ub   = quantile(transitions$counts, .975)
  )

}

# List of cases that are considered active in the
# model. Mostly removes susceptible and recovered.
active_cases_statuses <- c(
  "Exposed",
  "Prodromal",
  "Rash",
  "Isolated",
  "Detected Hospitalized",
  "Quarantined Exposed",
  "Quarantined Prodromal",
  "Quarantined Recovered",
  "Hospitalized"
  )

shiny_measles <- function(input) {

  # # # For debugging
  # saveRDS(as.list(input), "~/Downloads/input.rds")
  # input <- readRDS("~/Downloads/input.rds")

  model_measles <- model_builder(input, quarantine = TRUE)
  model_measles_no_quarantine <- model_builder(input, quarantine = FALSE)

  # NPIs -----------------------------------------------------------------------
  # interventions_add_all(model_measles, "measles", input)

  # Running and printing
  epiworldR::verbose_off(model_measles)
  epiworldR::verbose_off(model_measles_no_quarantine)

  epiworldR::run_multiple(
    m = model_measles,
    ndays = input$measles_n_days,
    nsims = input$measles_n_sims,
    seed = input$measles_seed,
    saver = make_saver("total_hist", "transition")
  )

  # Running the model without quarantine
  epiworldR::run_multiple(
    m = model_measles_no_quarantine,
    ndays = input$measles_n_days,
    nsims = input$measles_n_sims,
    seed = input$measles_seed,
    saver = make_saver("total_hist", "transition")
  )

  res_quarantine <- run_multiple_get_results(model_measles, nthreads = 1L)
  res_no_quarantine <- run_multiple_get_results(
    model_measles_no_quarantine,
    nthreads = 1L
    )

  histories <- res_quarantine$total_hist
  histories_no_quarantine <- res_no_quarantine$total_hist

  # Total number of hospitalizations
  table_hospitalizations <- function() {
    list(
      quarantine = analyze_hospitalizations(
        res_quarantine$transition
        ),
      no_quarantine = analyze_hospitalizations(
        res_no_quarantine$transition
        )
    )
  }

  # Table with total outbreak size
  table_summary_measles <- function() {
    tabulator(histories_no_quarantine, histories)
  }

  # Take home statistics
  takehome_stats <- function() {
    get_takehome_stats(histories_no_quarantine, histories)
  }

  # epiworldR::run(model_measles, ndays = input$measles_n_days, seed = input$measles_seed)
  # Plot
  plot_measles <- function() {

    # Getting the infected cases
    dat <- subset(histories, state %in% active_cases_statuses)
    dat <- stats::aggregate(counts ~ sim_num + date, data=dat, FUN=sum)
    dat <- stats::aggregate(
      counts ~ date,
      data = dat,
      FUN = function(x) {
        c(
          p50 = stats::quantile(x, .5),
          lower = stats::quantile(x, .025),
          upper = stats::quantile(x, .975)
        )
      }
    )

    dat <- cbind(data.frame(dat[[1]]), data.frame(dat[[2]]))

    colnames(dat) <- c("date", "p50", "lower", "upper")

    # Now, without quarantine
    dat_no_quarantine <- subset(histories_no_quarantine, state %in% active_cases_statuses)
    dat_no_quarantine <- stats::aggregate(counts ~ sim_num + date, data=dat_no_quarantine, FUN=sum)
    dat_no_quarantine <- stats::aggregate(
      counts ~ date,
      data = dat_no_quarantine,
      FUN = function(x) {
        c(
          p50 = stats::quantile(x, .5),
          lower = stats::quantile(x, .025),
          upper = stats::quantile(x, .975)
        )
      }
    )

    dat_no_quarantine <- cbind(
      data.frame(dat_no_quarantine[[1]]),
      data.frame(dat_no_quarantine[[2]])
      )

    colnames(dat_no_quarantine) <- c("date", "p50", "lower", "upper")

    # Greating figure with plotly
    plotly::plot_ly(
      data = dat,
      x = ~date,
      y = ~p50,
      type = 'scatter',
      mode = 'lines',
      name = "Median (quarantine)"
    ) |>
      plotly::add_ribbons(
        ymin = ~lower,
        ymax = ~upper,
        name = "95% CI (quarantine)",
        fillcolor = "rgba(48, 123, 194, 0.25)",
        line = list(width = 0)
      ) |>
      plotly::layout(
        title  = NULL,
        xaxis  = list(title = 'Day'),
        yaxis  = list(title = 'Active cases')
      ) |>
      plotly::add_lines(
        data = dat_no_quarantine,
        x = ~date,
        y = ~p50,
        name = "Median (no quarantine)",
        line = list(color = "red")
      ) |>
      plotly::add_ribbons(
        data = dat_no_quarantine,
        ymin = ~lower,
        ymax = ~upper,
        name = "95% CI (no quarantine)",
        fillcolor = "rgba(193, 26, 1, 0.25)",
        line = list(width = 0)
      ) |>
      plotly::layout(legend = list(x = 0, y = -0.3, orientation = "h"))
  }
  # Summary
  summary_measles <- function() {
    if (!input$measles_show_debug)
      return(NULL)
    summary(model_measles)
  }
  # Data
  model_data <- function() {
    rbind(
      cbind(histories, quarantine=TRUE),
      cbind(histories_no_quarantine, quarantine=FALSE)
    )
  }

  # Output list
  return(
    list(
      epicurves_plot   = plot_measles,
      model_summary    = summary_measles,
      summary_table    = table_summary_measles,
      model_table      = model_data,
      hospitalizations = table_hospitalizations,
      takehome_stats   = takehome_stats
    )
  )

}

measles_panel <- function(model_alt) {

  shiny::conditionalPanel(
    simulate_button("measles"),
    condition = sprintf("input.model == '%s'", model_alt),
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        title = "School Selector",
        shiny::p("Select a school from the database to populate vaccination rate and school size."),
        bslib::tooltip(
          shiny::selectInput(
            inputId = "measles_state_selector",
            label = "Select State",
            choices = c("Select..." = "")
          ),
          placement = "right",
          "First, select a state to filter counties"
        ),
        bslib::tooltip(
          shiny::selectInput(
            inputId = "measles_county_selector",
            label = "Select County",
            choices = c("Select..." = "")
          ),
          placement = "right",
          "Second, select a county to filter schools"
        ),
        bslib::tooltip(
          shiny::selectInput(
            inputId = "measles_school_selector",
            label = "Select School",
            choices = c("Select..." = "")
          ),
          placement = "right",
          "Finally, select a school to populate vaccination rate and school size"
        ),
        shiny::hr(),
        bslib::tooltip(
          shiny::fileInput(
            inputId = "measles_school_csv",
            label = "Upload Custom School Data (Optional)",
            accept = c(".csv")
          ),
          placement = "right",
          "Upload a CSV file with columns: state, county, school_name, school_id, vaccination_rate, num_students"
        ),
        shiny::actionButton(
          inputId = "measles_reset_school_data",
          label = "Reset to Default Data",
          width = "100%"
        )
      )
    ),
    bslib::tooltip(
      shiny::numericInput(
        inputId = "measles_population_size",
        label   = "Population Size",
        min     = 0,
        max     = 50000,
        value   = 500
      ),
      placement = "right",
      "# of students in the school"
    ),
    bslib::tooltip(
      shiny::numericInput(
        inputId = "measles_prevalence",
        label   = "Initial cases",
        value   = 1,
        min     = 1,
        max     = NA,
        step    = 1
      ),
      placement = "right",
      "# of students infected with measles at the start of the simulation"
    ),
    bslib::tooltip(
      slider_input_rate(
        "measles",
        "Proportion Vaccinated",
        0.85,
        maxval = 1,
        input_label = "prop_vaccinated"
      ),
      placement = "right",
      "Proportion of students in the school who are vaccinated against measles"
    ),
    bslib::tooltip(
      numeric_input_ndays("measles"),
      placement = "right",
      "# of days to run the simulation"
    ),
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        title = "Quarantine",
        bslib::tooltip(
          slider_input_rate(
            "measles",
            "Quarantine Willingness",
            1.0,
            maxval = 1,
            input_label = "quarantine_willingness"
          ),
          placement = "right",
          "How willing people are to stay home from school when asked to quarantine (1 = 100% willing, 0 = 0% willing)"
        ),
        bslib::tooltip(
          shiny::numericInput(
            inputId = "measles_days_undetected",
            label   = "Days Undetected",
            value   = "2",
            min     = 0,
            max     = NA,
            step    = .5
          ),
          placement = "right",
          "Average # of days after the rash manifests before a person is detected as infected with measles"
        ),
        bslib::tooltip(
          shiny::numericInput(
            inputId = "measles_quarantine_days",
            label   = "Quarantine Days",
            value   = "21",
            min     = 0,
            max     = NA,
            step    = 1
          ),
          placement = "right",
          "# of days after potential exposure a quarantined person will stay home from school, if willing. This is a fixed value, not an average, and is the same for all quarantined individuals. 21 days is the CDC recommendation for measles quarantine."
        ),
        bslib::tooltip(
          shiny::numericInput(
            inputId = "measles_isolation_days",
            label   = "Isolation Days",
            value   = "4",
            min     = 0,
            max     = NA,
            step    = 1
          ),
          placement = "right",
          "# of days an infected person is isolated after rash is detected. This is a fixed value, not an average, and is the same for all isolated individuals."
        )
      )
    ),
    # Adding a hidden input to keep most parameters
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        "Advanced parameters",
        shiny::p("The below parameters are advanced and control disease dynamics."),
        bslib::tooltip(
          shiny::numericInput(
            inputId = "measles_hospitalization_duration",
            label   = "Hospitalization Duration (days)",
            value   = "7",
            min     = 0,
            max     = NA,
            step    = 1
          ),
          placement = "right",
          "Average # of days an infected person is hospitalized"
        ),
        bslib::tooltip(
          shiny::numericInput(
            inputId = "measles_n_sims",
            label   = "Number of simulations",
            value   = "100",
            min     = 1,
            max     = 1000,
            step    = 1
          ),
          placement = "right",
          "# of simulations to run - displayed results are averaged across all simulations"
        ),
        bslib::tooltip(
          slider_input_rate(
            "measles",
            "Contact Rate",
            15/.99/4,
            maxval = 20
          ),
          placement = "right",
          "# of people a given person interacts with per day of the simulation. The value was calculated to match the R0 of measles (15), with a transmission rate of 0.99 and a prodromal period of 4 days."
        ),
        bslib::tooltip(
          slider_input_rate(
            "measles", "Hospitalization Rate", 0.2, maxval = 1
          ),
          placement = "right",
          "Rate of hospitalization for infected individuals per day of the simulation"
        ),
        bslib::tooltip(
          slider_input_rate(
            "measles", "Transmission probability", "0.99", input_label = "transmission_rate"),
          placement = "right",
          "The chance an infected individual transmits the disease to a contacted susceptible individual per day of the simulation"
        ),
        bslib::tooltip(
          slider_input_rate(
            "measles", "Vaccination Efficacy", "0.99", input_label = "vax_efficacy"),
          placement = "right",
          "How effective the vaccine is at preventing infection"
        ),
        bslib::tooltip(
          slider_input_rate(
            "measles", "Vaccination Improved Recovery", "0.5", input_label = "vax_improved_recovery"),
          placement = "right",
          "How much faster a vaccinated infected individual recovers compared to an unvaccinated infected individual"
        ),
        bslib::tooltip(
          shiny::numericInput(
            inputId = "measles_incubation_days",
            label   = "Incubation Days",
            value   = "12",
            min     = 0,
            max     = NA,
            step    = 1
          ),
          placement = "right",
          "Average # of days the disease incubates before the individual becomes symptomatic"
        ),
        bslib::tooltip(
          shiny::numericInput(
            inputId = "measles_prodromal_period",
            label   = "Prodromal Period (days)",
            value   = "4",
            min     = 0,
            max     = NA,
            step    = 1
          ),
          placement = "right",
          "Average # of days the prodromal period lasts before the individual develops a rash"
        ),
        bslib::tooltip(
          shiny::numericInput(
            inputId = "measles_rash_period",
            label   = "Rash Period (days)",
            value   = "3",
            min     = 0,
            max     = NA,
            step    = 1
          ),
          placement = "right",
          "Average # of days the rash lasts before the individual recovers"
        ),
        bslib::tooltip(
          seed_input("measles"),
          placement = "right",
          "Random seed for the simulation, use a specific seed to reproduce results"
        ),
        bslib::tooltip(
          shiny::checkboxInput(
            inputId = "measles_show_debug",
            label   = "Show Debugging Information",
            value   = FALSE
          ),
          placement = "right",
          "Shows detailed information of the simulation run for debugging purposes"
        ),
      )
    )
  )  # npis_input("measles")
}

body_measles <- function(input, model_output, output, session = shiny::getDefaultReactiveDomain()) {

  # Reactive value to hold school data
  school_data <- shiny::reactiveVal(NULL)
  
  # Load default school data on initialization
  shiny::observe({
    default_csv <- system.file("data/schools_measles.csv", package = "epiworldRShiny")
    if (file.exists(default_csv)) {
      data <- utils::read.csv(default_csv, stringsAsFactors = FALSE)
      school_data(data)
      
      # Update state dropdown with unique states
      states <- sort(unique(data$state))
      shiny::updateSelectInput(
        session = session,
        inputId = "measles_state_selector",
        choices = c("Select..." = "", states)
      )
    }
  })
  
  # Handle Reset button
  shiny::observeEvent(input$measles_reset_school_data, {
    default_csv <- system.file("data/schools_measles.csv", package = "epiworldRShiny")
    if (file.exists(default_csv)) {
      data <- utils::read.csv(default_csv, stringsAsFactors = FALSE)
      school_data(data)
      
      # Reset all dropdowns
      states <- sort(unique(data$state))
      shiny::updateSelectInput(
        session = session,
        inputId = "measles_state_selector",
        choices = c("Select..." = "", states),
        selected = ""
      )
      shiny::updateSelectInput(
        session = session,
        inputId = "measles_county_selector",
        choices = c("Select..." = ""),
        selected = ""
      )
      shiny::updateSelectInput(
        session = session,
        inputId = "measles_school_selector",
        choices = c("Select..." = ""),
        selected = ""
      )
      
      shiny::showNotification("Reset to default school data", type = "message")
    }
  })
  
  # Handle CSV upload
  shiny::observeEvent(input$measles_school_csv, {
    req(input$measles_school_csv)
    
    tryCatch({
      # Check file size (limit to 5MB)
      if (input$measles_school_csv$size > 5 * 1024 * 1024) {
        shiny::showNotification(
          "CSV file size must be less than 5MB",
          type = "error"
        )
        return()
      }
      
      data <- utils::read.csv(input$measles_school_csv$datapath, stringsAsFactors = FALSE)
      
      # Validate columns
      required_cols <- c("state", "county", "school_name", "school_id", 
                         "vaccination_rate", "num_students")
      if (!all(required_cols %in% colnames(data))) {
        shiny::showNotification(
          "CSV must contain columns: state, county, school_name, school_id, vaccination_rate, num_students",
          type = "error"
        )
        return()
      }
      
      # Validate data types and ranges
      if (!is.numeric(data$vaccination_rate) || any(data$vaccination_rate < 0 | data$vaccination_rate > 1, na.rm = TRUE)) {
        shiny::showNotification(
          "vaccination_rate must be numeric values between 0 and 1",
          type = "error"
        )
        return()
      }
      
      if (!is.numeric(data$num_students) || any(data$num_students < 0 | data$num_students > 50000, na.rm = TRUE)) {
        shiny::showNotification(
          "num_students must be numeric values between 0 and 50000",
          type = "error"
        )
        return()
      }
      
      # Limit number of schools to prevent UI issues
      if (nrow(data) > 1000) {
        shiny::showNotification(
          "CSV contains too many schools. Maximum 1000 schools allowed.",
          type = "error"
        )
        return()
      }
      
      school_data(data)
      
      # Update state dropdown with new data
      states <- sort(unique(data$state))
      shiny::updateSelectInput(
        session = session,
        inputId = "measles_state_selector",
        choices = c("Select..." = "", states),
        selected = ""
      )
      
      # Reset county and school dropdowns
      shiny::updateSelectInput(
        session = session,
        inputId = "measles_county_selector",
        choices = c("Select..." = ""),
        selected = ""
      )
      shiny::updateSelectInput(
        session = session,
        inputId = "measles_school_selector",
        choices = c("Select..." = ""),
        selected = ""
      )
      
      shiny::showNotification("School data loaded successfully!", type = "message")
      
    }, error = function(e) {
      shiny::showNotification(
        "Error reading CSV file. Please check the file format.",
        type = "error"
      )
    })
  })
  
  # Handle state selection - update counties
  shiny::observeEvent(input$measles_state_selector, {
    if (!is.null(input$measles_state_selector) && 
        input$measles_state_selector != "" &&
        !is.null(school_data())) {
      
      data <- school_data()
      filtered_data <- data[data$state == input$measles_state_selector, ]
      
      if (nrow(filtered_data) > 0) {
        counties <- sort(unique(filtered_data$county))
        shiny::updateSelectInput(
          session = session,
          inputId = "measles_county_selector",
          choices = c("Select..." = "", counties),
          selected = ""
        )
      } else {
        shiny::updateSelectInput(
          session = session,
          inputId = "measles_county_selector",
          choices = c("Select..." = ""),
          selected = ""
        )
      }
      
      # Reset school dropdown
      shiny::updateSelectInput(
        session = session,
        inputId = "measles_school_selector",
        choices = c("Select..." = ""),
        selected = ""
      )
    } else {
      # Reset county and school dropdowns if no state selected
      shiny::updateSelectInput(
        session = session,
        inputId = "measles_county_selector",
        choices = c("Select..." = ""),
        selected = ""
      )
      shiny::updateSelectInput(
        session = session,
        inputId = "measles_school_selector",
        choices = c("Select..." = ""),
        selected = ""
      )
    }
  })
  
  # Handle county selection - update schools
  shiny::observeEvent(input$measles_county_selector, {
    if (!is.null(input$measles_county_selector) && 
        input$measles_county_selector != "" &&
        !is.null(input$measles_state_selector) &&
        input$measles_state_selector != "" &&
        !is.null(school_data())) {
      
      data <- school_data()
      filtered_data <- data[data$state == input$measles_state_selector & 
                            data$county == input$measles_county_selector, ]
      
      if (nrow(filtered_data) > 0) {
        # Create named vector with school names as both names and values
        school_choices <- setNames(filtered_data$school_name, filtered_data$school_name)
        shiny::updateSelectInput(
          session = session,
          inputId = "measles_school_selector",
          choices = c("Select..." = "", school_choices),
          selected = ""
        )
      } else {
        shiny::updateSelectInput(
          session = session,
          inputId = "measles_school_selector",
          choices = c("Select..." = ""),
          selected = ""
        )
      }
    } else {
      # Reset school dropdown if no county selected
      shiny::updateSelectInput(
        session = session,
        inputId = "measles_school_selector",
        choices = c("Select..." = ""),
        selected = ""
      )
    }
  })
  
  # Handle school selection - populate form fields
  shiny::observeEvent(input$measles_school_selector, {
    if (!is.null(input$measles_school_selector) && 
        input$measles_school_selector != "" &&
        !is.null(school_data())) {
      
      data <- school_data()
      school_row <- data[data$school_name == input$measles_school_selector & 
                         data$state == input$measles_state_selector &
                         data$county == input$measles_county_selector, ]
      
      if (nrow(school_row) == 1) {
        # Validate that the selected school has valid data
        if (!is.na(school_row$num_students) && !is.na(school_row$vaccination_rate)) {
          # Update population size
          shiny::updateNumericInput(
            session = session,
            inputId = "measles_population_size",
            value = school_row$num_students
          )
          
          # Update vaccination rate
          shiny::updateSliderInput(
            session = session,
            inputId = "measles_prop_vaccinated",
            value = school_row$vaccination_rate
          )
          
          shiny::showNotification(
            paste("Populated data for:", school_row$school_name),
            type = "message"
          )
        } else {
          shiny::showNotification(
            "Selected school has invalid data",
            type = "error"
          )
        }
      }
    }
  })

  output$summary_table <- shiny::renderTable({
      model_output()$summary_table()
  })

  output$model_summary <- shiny::renderPrint({
    model_output()$model_summary()
  })

  output$epicurves_plot <- plotly::renderPlotly({
    model_output()$epicurves_plot()
  })

  # Take-home Messages

  output$takehome_message <- shiny::renderText({
    if (input$measles_prevalence == 1) {
      sprintf(
        "When 1 case of measles is introduced into a school with %1.0f students, we expect the following outbreak sizes and number of hospitalizations based on whether quarantine procedures were implemented:",
        input$measles_population_size
      )
    } else {
      sprintf(
        "When %1.0f cases of measles are introduced into a school with %1.0f students, we expect the following outbreak sizes and number of hospitalizations based on whether quarantine procedures were implemented:",
        input$measles_prevalence,
        input$measles_population_size
      )
    }
  })

  # Outbreak Size
  output$thm_noquarantine_outbreak_value <- shiny::renderText({
    sprintf(
      "%1.0f cases",
      round(model_output()$takehome_stats()$no_quarantine_mean_cases, digits = 0)
    )
  })

  output$thm_quarantine_outbreak_value <- shiny::renderText({
    sprintf(
      "%1.0f cases",
      round(model_output()$takehome_stats()$quarantine_mean_cases, digits = 0)
    )
  })

  # Hospitalizations
  output$thm_noquarantine_hospitalizations_value <- shiny::renderText({
    sprintf(
      "%1.0f hospitalizations",
      round(model_output()$hospitalizations()$no_quarantine$mean, digits = 0)
    )
  })

  output$thm_quarantine_hospitalizations_value <- shiny::renderText({
    sprintf(
      "%1.0f hospitalizations",
      round(model_output()$hospitalizations()$quarantine$mean, digits = 0)
    )
  })

  # Logos
  output$dhhs_logo <- shiny::renderImage(
    {
      logo <- system.file("assets/udhhs-logo.png", package = "epiworldRShiny")

      list(
        src = logo,
        width = "150px"
      )
    },
    deleteFile = FALSE
  )

  output$foresite_logo <- shiny::renderImage(
    {
      logo <- system.file("assets/foresite-logo.png", package = "epiworldRShiny")

      list(
        src = logo,
        width = "150px"
      )
    },
    deleteFile = FALSE
  )

  list(
    bslib::card(
      shiny::htmlOutput("model_description")
    ),
    bslib::card(
      bslib::card_header("Summary"),
      shiny::textOutput("takehome_message"),
      bslib::layout_columns(
        bslib::value_box(
          title = "WITHOUT quarantine",
          value = shiny::textOutput("thm_noquarantine_outbreak_value"),
          shiny::textOutput("thm_noquarantine_hospitalizations_value"),
          theme = "red"
        ),
        bslib::value_box(
          title = "WITH quarantine",
          value = shiny::textOutput("thm_quarantine_outbreak_value"),
          shiny::textOutput("thm_quarantine_hospitalizations_value"),
          theme = "blue"
        )
      )
    ),
    bslib::card(
      bslib::card_header("Epidemic Curve"),
      shiny::p(
        sprintf(
          "The figure shows the potential outbreak sizes after running
        %i simulations. The solid line represents the 50%% quantile",
        input$measles_n_sims
        )
      ),
      plotly::plotlyOutput("epicurves_plot")
    ),
    bslib::card(
      bslib::card_header("Outbreak Size"),
      shiny::p(
          "The table below shows the probability of seeing outbreak sizes above a given threshold WITH and WITHOUT quarantine."
        ),
      shiny::tableOutput("summary_table")
    ),
    bslib::card(
      bslib::card_header("Acknowledgements"),
      shiny::p("Made in collaboration with Utah DHHS and ForeSITE"),
      bslib::layout_columns(
        shiny::imageOutput("dhhs_logo", height = "150px"),
        shiny::div(
          shiny::imageOutput("foresite_logo", height = "130px"),
          style = "display: flex;
            justify-content: center;
            align-items: center;
            height: 150px;"
        )
      )
    ),
    if (length(input$measles_show_debug) && input$measles_show_debug) {
      bslib::card(
        width = 6,
        shiny::verbatimTextOutput("model_summary")
      )
    } else NULL
  )
}
