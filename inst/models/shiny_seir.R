# alt-name: Network SEIR
shiny_seir <- function(input) {

  model_seir <- epiworldR::ModelSEIR(
    name              = input$seir_disease_name,
    n                 = input$seir_population_size,
    prevalence        = input$seir_prevalence,
    transmission_prob = input$seir_transmission_rate,
    recovery_rate     = input$seir_recovery_rate,
    latent_period     = input$seir_incubation_days
    )

  # Generating random graph
  epiworldR::add_layer(
    model_seir,
    epiworldR::layer_network(
      "network",
      epiworldR::graph_smallworld(
        k        = input$seir_k,
        p        = input$seir_prob_rewiring,
        directed = as.logical(input$seir_directed)
      )
    )
  )

  # NPIs -----------------------------------------------------------------------
  interventions_add_all(model_seir, "seir", input)

  # Running and printing
  epiworldR::verbose_off(model_seir)
  epiworldR::run(model_seir, ndays = input$seir_n_days, seed = input$seir_seed)
  # Plot
  plot_seir <- function() plot_epi(model_seir)
  # Summary
  summary_seir <- function() summary(model_seir)
  # Reproductive Number
  reproductive_seir <- function() plot_reproductive_epi(model_seir)
  # Table
  table_seir <- function() hist_table(model_seir)

  # Output list
  return(
    list(
      epicurves_plot     = plot_seir,
      reproductive_plot  = reproductive_seir,
      model_summary      = summary_seir,
      model_table        = table_seir
    )
  )

}

seir_panel <- function(model_alt) {
  shiny::conditionalPanel(
    simulate_button("seir"),
    condition = sprintf("input.model == '%s'", model_alt),
    text_input_disease_name("seir"),
    slider_prevalence("seir"),
    slider_input_rate("seir", "Transmission probability", "0.05", input_label = "transmission_rate"),
    slider_input_rate("seir", "Recovery probability (daily)", "0.14", input_label = "recovery_rate"),
    shiny::numericInput(
      inputId = "seir_incubation_days",
      label   = "Incubation Days",
      value   = "7",
      min     = 0,
      max     = NA,
      step    = 1
      ),
    numeric_input_ndays("seir"),
    seed_input("seir"),
    network_input("seir"),
    npis_input("seir")
  )
}
