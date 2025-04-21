# alt-name: Measles

model_builder <- function(input, quarantine = TRUE) {
  epiworldR::ModelMeaslesQuarantine(
    n                        = as.integer(input$measles_population_size),
    contact_rate             = input$measles_contact_rate,
    prevalence               = as.integer(input$measles_prevalence),
    transmission_rate        = input$measles_transmission_rate,
    vax_efficacy             = input$measles_vax_efficacy,
    vax_improved_recovery    = input$measles_vax_improved_recovery,
    incubation_period        = input$measles_incubation_days,
    prodromal_period         = input$measles_prodromal_period,
    rash_period              = input$measles_rash_period,
    days_undetected          = if (quarantine)
      input$measles_days_undetected
    else
      -1,
    hospitalization_rate     = input$measles_hospitalization_rate,
    hospitalization_duration = input$measles_hospitalization_duration,
    prop_vaccinated          = input$measles_prop_vaccinated,
    quarantine_days          = input$measles_quarantine_days,
    quarantine_willingness    = input$measles_quarantine_willingness
  )
}

#' Generates a string with the corresponding CI
#' @param x A vector of numbers
#' @param lb,ub Lower and upper bounds of the CI.
get_ci_pretty <- function(x, lb = .025, ub = .975) {
  sprintf("[%.2f, %.2f]", quantile(x, lb), quantile(x, ub))
}

tabulator <- function(histories) {
  exposed <- c(
    active_cases_statuses,
    "Recovered"
    )

  counts <- subset(
    histories, (state %in% exposed) & (date == max(date))
  )

  # data.table::fwrite(counts, file = "counts.csv")

  counts <- stats::aggregate(counts ~ sim_num, data = counts, FUN = sum)
  colnames(counts) <- c("Simulation", "Total")

  sizes <- c(2, 5, 10, 20)
  sizes <- data.frame(
    Size = sizes,
    Probability = sapply(sizes, \(x) {
      sum(counts$Total >= x)/nrow(counts)
    }),
    "Likely size (if > Size)" = sapply(sizes, \(s){
      get_ci_pretty(counts$Total[counts$Total >= s])
      }),
      check.names = FALSE
    )

  sizes$Probability <- ifelse(
    sizes$Probability <= 0.01,
    "< 0.01",
    sprintf("%.2f", sizes$Probability)
  )

  # Replaces NAs
  sizes$`Likely size (if > Size)` <- ifelse(
    grepl("NA", sizes$`Likely size (if > Size)`),
    "-",
    sizes$`Likely size (if > Size)`
  )


  median_cases <- quantile(counts$Total, probs=.5)
  mean_cases <- mean(counts$Total, probs=.5)

  sizes <- rbind(
    sizes,
    data.frame(
      Size = median_cases,
      Probability = "Median (50%>)",
      "Likely size (if > Size)" = get_ci_pretty(
        counts$Total[counts$Total > median_cases]
        ),
      check.names = FALSE
    ),
    data.frame(
      Size = mean_cases,
      Probability = "Mean (average)",
      "Likely size (if > Size)" = get_ci_pretty(
        counts$Total[counts$Total > mean_cases]
        ),
      check.names = FALSE
    )
  )

  sizes
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
    from != "Hospitalized" & to == "Hospitalized"
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
  "Quarantined Exposed",
  "Quarantined Prodromal",
  "Quarantined Recovered",
  "Hospitalized"
  )

shiny_measles <- function(input) {

  # For debugging
  # saveRDS(as.list(input), "~/Downloads/input.rds")

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

  res_quarantine <- run_multiple_get_results(model_measles)
  res_no_quarantine <- run_multiple_get_results(
    model_measles_no_quarantine
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

    list(
      quarantine = tabulator(histories),
      no_quarantine = tabulator(histories_no_quarantine)
    )

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
      mode = 'lines+markers',
      name = "Median (quarantine)"
    ) |>
      plotly::add_ribbons(
        ymin = ~lower,
        ymax = ~upper,
        name = "95% CI",
        fillcolor = "rgba(0, 100, 80, 0.5)",
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
        fillcolor = "rgba(100, 0, 80, 0.5)",
        line = list(width = 0)
      )
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
      hospitalizations = table_hospitalizations
    )
  )

}

measles_panel <- function(model_alt) {

  shiny::conditionalPanel(
    simulate_button("measles"),
    condition = sprintf("input.model == '%s'", model_alt),
    shiny::numericInput(
      inputId = "measles_population_size",
      label   = "Population Size",
      min     = 0,
      max     = 50000,
      value   = 500
    ),
    shiny::numericInput(
      inputId = "measles_prevalence",
      label   = "Initial cases",
      value   = 1,
      min     = 1,
      max     = NA,
      step    = 1
    ),
    slider_input_rate(
      "measles", "Proportion Vaccinated", 0.85,
      maxval = 1, input_label = "prop_vaccinated"
      ),
    numeric_input_ndays("measles"),
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        title = "Quarantine",
        shiny::numericInput(
          inputId = "measles_days_undetected",
          label   = "Days Undetected",
          value   = "2",
          min     = 0,
          max     = NA,
          step    = .5
        ),
        shiny::numericInput(
          inputId = "measles_quarantine_days",
          label   = "Quarantine Days",
          value   = "21",
          min     = 0,
          max     = NA,
          step    = 1
        ),
        slider_input_rate(
          "measles", "Quarantine Willingness", 1.0,
          maxval = 1, input_label = "quarantine_willingness"
        )
      )
    ),
    # Adding a hidden input to keep most parameters
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        "Advanced parameters",
        shiny::p("The below parameters are advanced and control disease dynamics."),
        shiny::numericInput(
          inputId = "measles_hospitalization_duration",
          label   = "Hospitalization Duration (days)",
          value   = "7",
          min     = 0,
          max     = NA,
          step    = 1
          ),
        shiny::numericInput(
          inputId = "measles_n_sims",
          label   = "Number of simulations",
          value   = "100",
          min     = 1,
          max     = 1000,
          step    = 1
        ),
        slider_input_rate(
          "measles",
          "Contact Rate",
          15/.99/(4 + 3),
          maxval = 20
        ),
        slider_input_rate(
          "measles", "Hospitalization Rate", 0.2, maxval = 1
        ),
        slider_input_rate(
          "measles", "Transmission probability", "0.99", input_label = "transmission_rate"),
        slider_input_rate(
          "measles", "Vaccination Efficacy", "0.99", input_label = "vax_efficacy"),
        slider_input_rate(
          "measles", "Vaccination Improved Recovery", "0.5", input_label = "vax_improved_recovery"),
        slider_input_rate(
          "measles", "Recovery probability (daily)", "0.14", input_label = "recovery_rate"),
        shiny::numericInput(
          inputId = "measles_incubation_days",
          label   = "Incubation Days",
          value   = "12",
          min     = 0,
          max     = NA,
          step    = 1
          ),
        shiny::numericInput(
          inputId = "measles_prodromal_period",
          label   = "Prodromal Period (days)",
          value   = "4",
          min     = 0,
          max     = NA,
          step    = 1
        ),
        shiny::numericInput(
          inputId = "measles_rash_period",
          label   = "Rash Period (days)",
          value   = "3",
          min     = 0,
          max     = NA,
          step    = 1
        ),
        seed_input("measles"),
        shiny::checkboxInput(
          inputId = "measles_show_debug",
          label   = "Show Debugging Information",
          value   = FALSE
        )
      )
    )
  )  # npis_input("measles")
}

body_measles <- function(input, model_output, output) {

  output$summary_table_quarantine <- shiny::renderTable({
      model_output()$summary_table()$quarantine
  })

  output$summary_table_no_quarantine <- shiny::renderTable({
      model_output()$summary_table()$no_quarantine
  })

  output$model_summary <- shiny::renderPrint({
    model_output()$model_summary()
  })

  output$epicurves_plot <- plotly::renderPlotly({
    model_output()$epicurves_plot()
  })

  output$hospitalizations <- shiny::renderText({
    hosps <- model_output()$hospitalizations()

    sprintf(
      "With quarantine: %.2f (%.2f, %.2f) hospitalizations. Without quarantine: %.2f (%.2f, %.2f) hospitalizations.",
      hosps$quarantine$mean,
      hosps$quarantine$lb,
      hosps$quarantine$ub,
      hosps$no_quarantine$mean,
      hosps$no_quarantine$lb,
      hosps$no_quarantine$ub
    )
  })

  list(
    bslib::card(
      shiny::htmlOutput("model_description")
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
          "The table below shows the number of cases at the end of the simulation. The first column is the size of the outbreak, and the second column is the probability of that size occurring. The third column is the likely size of the outbreak if it exceeds a certain threshold."
        ),
      shiny::p("With quarantine"),
      shiny::tableOutput("summary_table_quarantine"),
      shiny::p("Without quarantine"),
      shiny::tableOutput("summary_table_no_quarantine"),
      shiny::htmlOutput("hospitalizations")
    ),
    if (length(input$measles_show_debug) && input$measles_show_debug) {
      bslib::card(
        width = 6,
        shiny::verbatimTextOutput("model_summary")
      )
    } else NULL
  )
}
