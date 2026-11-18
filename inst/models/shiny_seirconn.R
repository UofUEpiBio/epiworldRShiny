# alt-name: SEIR
shiny_seirconn <- function(input) {

  model_seirconn <- epiworldR::ModelSEIR(
    name              = input$seirconn_disease_name,
    n                 = input$seirconn_population_size,
    prevalence        = input$seirconn_prevalence,
    transmission_prob = input$seirconn_transmission_rate,
    recovery_rate     = input$seirconn_recovery_rate,
    latent_period     = input$seirconn_incubation_days
    )

  # Random mixing: everyone meets `contact_rate` others per day
  add_random_mixing(
    model_seirconn,
    n            = input$seirconn_population_size,
    contact_rate = input$seirconn_contact_rate
  )

  # NPIs -----------------------------------------------------------------------
  interventions_add_all(model_seirconn, "seirconn", input)

  # Running and printing
  epiworldR::verbose_off(model_seirconn)
  epiworldR::run(model_seirconn, ndays = input$seirconn_n_days, seed = input$seirconn_seed)

  # Plot, summary, and reproductive number
  plot_seirconn <- function() plot_epi(model_seirconn)
  summary_seirconn <- function() summary(model_seirconn)
  reproductive_seirconn <- function() plot_reproductive_epi(model_seirconn)
  # Table
  table_seirconn <- function() hist_table(model_seirconn)
  # Output list
  return(
    list(
      epicurves_plot     = plot_seirconn,
      reproductive_plot  = reproductive_seirconn,
      model_summary      = summary_seirconn,
      model_table        = table_seirconn
    )
  )
}

seirconn_panel <- function(model_alt) {
  shiny::conditionalPanel(
    simulate_button("seirconn"),
    condition = sprintf("input.model == '%s'", model_alt),
    text_input_disease_name("seirconn"),
    slider_prevalence("seirconn"),
    slider_input_rate("seirconn", "Transmission probability", 0.1, input_label = "transmission_rate"),
    slider_input_rate("seirconn", "Recovery probability (daily)", 0.14, input_label = "recovery_rate"),
    slider_input_rate("seirconn", "Contact Rate", 4, maxval = 20),
    shiny::numericInput(
      inputId = "seirconn_incubation_days",
      label   = "Incubation Days",
      value   = "7",
      min     = 0,
      max     = NA,
      step    = 1
      ),
    shiny::sliderInput(
      inputId = "seirconn_population_size",
      label   = "Population Size",
      min     = 0,
      max     = 100000,
      value   = 50000,
      step    = 1000,
      ticks   = FALSE
    ),
    numeric_input_ndays("seirconn"),
    seed_input("seirconn"),
    npis_input("seirconn")
  )
}

