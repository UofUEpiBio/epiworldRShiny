# alt-name: Measles Quarantine
shiny_measlesquarantine <- function(input) {

  # For debugging
  # saveRDS(as.list(input), "~/Downloads/input.rds")

  model_measlesquarantine <- epiworldR::ModelMeaslesQuarantine(
    n                 = as.integer(input$measlesquarantine_population_size),
    contact_rate      = input$measlesquarantine_contact_rate, 
    prevalence        = as.integer(input$measlesquarantine_prevalence),
    transmission_rate = input$measlesquarantine_transmission_rate,
    vax_efficacy      = input$measlesquarantine_vax_efficacy,
    vax_improved_recovery = input$measlesquarantine_vax_improved_recovery,
    incubation_period = input$measlesquarantine_incubation_days,
    prodromal_period = input$measlesquarantine_prodromal_period,
    rash_period       = input$measlesquarantine_rash_period,
    days_undetected   = input$measlesquarantine_days_undetected,
    hospitalization_rate = input$measlesquarantine_hospitalization_rate,
    hospitalization_duration = input$measlesquarantine_hospitalization_duration,
    prop_vaccinated   = input$measlesquarantine_prop_vaccinated,
    quarantine_days   = input$measlesquarantine_quarantine_days,
    quarantine_willigness = input$measlesquarantine_quarantine_willigness
    )

  # NPIs -----------------------------------------------------------------------
  # interventions_add_all(model_measlesquarantine, "measlesquarantine", input)

  # Running and printing
  epiworldR::verbose_off(model_measlesquarantine)
  epiworldR::run(model_measlesquarantine, ndays = input$measlesquarantine_n_days, seed = input$measlesquarantine_seed)
  # Plot
  plot_measlesquarantine <- function() plot_epi(
    model_measlesquarantine,
    mark_max = c("Exposed", "Quarantine Exposed")
    )
  # Summary
  summary_measlesquarantine <- function() summary(model_measlesquarantine)
  # Reproductive Number
  reproductive_measlesquarantine <- function() plot_reproductive_epi(model_measlesquarantine)
  # Table
  table_measlesquarantine <- function() {

    df <- as.data.frame(epiworldR::get_hist_total(model_measlesquarantine))

    # Subset to only include "infection" state
    infection_data <- df[!df$state %in% c("Susceptible", "Quarantine Susceptible", "Recovered", "Quarantine Recovered"), ]

    # Row with the maximum count
    max_infection_row <- infection_data[which.max(infection_data$counts), ]

    # Row number of the maximum count in the original data frame
    max_row_number <- which(
      df$date == max_infection_row$date & df$state == "Infected"
      )

    df[max_row_number,"counts"] <- sprintf(
      "<strong>%s</strong>",
      df[max_row_number, "counts"]
      )

    # Making sure factor variables are ordered
    df$state <- factor(
      x      = df$state,
      levels = c(
        "Susceptible",
        "Exposed",
        "Prodromal",
        "Rash",
        "Isolated",
        "Quarantined Exposed",
        "Quarantined Susceptible",
        "Quarantined Prodromal",
        "Quarantined Recovered",
        "Hospitalized",
        "Recovered"
        )
      )

    # Reshaping the data to wide format
    df <- reshape(df, idvar = "date", timevar = "state", direction = "wide")

    colnames(df) <- gsub(colnames(df), pattern = "counts.", replacement = "")
    df

  }

  # Output list
  return(
    list(
      epicurves_plot     = plot_measlesquarantine,
      reproductive_plot  = reproductive_measlesquarantine,
      model_summary      = summary_measlesquarantine,
      model_table        = table_measlesquarantine
    )
  )

}

measlesquarantine_panel <- function(model_alt) {
  shiny::conditionalPanel(
    simulate_button("measlesquarantine"),
    condition = sprintf("input.model == '%s'", model_alt),
    shiny::sliderInput(
      inputId = "measlesquarantine_population_size",
      label   = "Population Size",
      min     = 0,
      max     = 50000,
      value   = 2000,
      step    = 1000,
      ticks   = FALSE
    ),
    shiny::numericInput(
      inputId = "measlesquarantine_prevalence",
      label   = "Prevalence",
      value   = 1,
      min     = 1,
      max     = NA,
      step    = 1
    ),
    shiny::numericInput(
      inputId = "measlesquarantine_days_undetected",
      label   = "Days Undetected",
      value   = "2",
      min     = -1,
      max     = NA,
      step    = 1
    ),
    slider_input_rate(
      "measlesquarantine", "Proportion Vaccinated", 0.7, 
      maxval = 1, input_label = "prop_vaccinated"
      ),
    shiny::numericInput(
      inputId = "measlesquarantine_quarantine_days",
      label   = "Quarantine Days",
      value   = "21",
      min     = 0,
      max     = NA,
      step    = 1
    ),
    slider_input_rate(
      "measlesquarantine", "Quarantine Willingness", 1.0, 
      maxval = 1, input_label = "quarantine_willigness"
      ),
    numeric_input_ndays("measlesquarantine"),
    seed_input("measlesquarantine"),
    # Adding a hidden input to keep most parameters
    shiny::tagList(
      shiny::div(
        id = paste0("advanced_header_", "measlesquarantine"),
        shiny::headerPanel(
          shiny::h4(
            shiny::tagList(
              shiny::icon("circle-info"),
              "Advanced Parameters"
            )
          )
        )
      ),
      shinyjs::hidden(
        shiny::div(
          id = paste0("advanced_inputs_", "measlesquarantine"),
          shiny::tagList(
            shiny::tags$style("p { padding: 0 20px; }"),
            shiny::p("The below parameters are advanced and control disease dynamics."),
            shiny::numericInput(
              inputId = "measlesquarantine_hospitalization_duration",
              label   = "Hospitalization Duration (days)",
              value   = "7",
              min     = 0,
              max     = NA,
              step    = 1
              ),
            slider_input_rate(
              "measlesquarantine",
              "Contact Rate",
              15/.99/(4 + 3),
              maxval = 20
            ),
            slider_input_rate(
              "measlesquarantine", "Hospitalization Rate", 0.2, maxval = 1
            ),
            slider_input_rate(
              "measlesquarantine", "Transmission probability", "0.99", input_label = "transmission_rate"),
            slider_input_rate(
              "measlesquarantine", "Vaccination Efficacy", "0.99", input_label = "vax_efficacy"),
            slider_input_rate(
              "measlesquarantine", "Vaccination Improved Recovery", "0.5", input_label = "vax_improved_recovery"),
            slider_input_rate(
              "measlesquarantine", "Recovery probability (daily)", "0.14", input_label = "recovery_rate"),
            shiny::numericInput(
              inputId = "measlesquarantine_incubation_days",
              label   = "Incubation Days",
              value   = "12",
              min     = 0,
              max     = NA,
              step    = 1
              ),
            shiny::numericInput(
              inputId = "measlesquarantine_prodromal_period",
              label   = "Prodromal Period (days)",
              value   = "4",
              min     = 0,
              max     = NA,
              step    = 1
            ),
            shiny::numericInput(
              inputId = "measlesquarantine_rash_period",
              label   = "Rash Period (days)",
              value   = "3",
              min     = 0,
              max     = NA,
              step    = 1
            )
          )
        )
      )
    ) #,
    # npis_input("measlesquarantine")
  )
}
