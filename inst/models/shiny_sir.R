# alt-name: SIR Network
shiny_sir <- function(input) {

  # Creating model
  model_sir <- epiworldR::ModelSIR(
    name              = input$sir_disease_name,
    n                 = input$sir_population_size,
    prevalence        = input$sir_prevalence,
    transmission_prob = input$sir_transmission_rate,
    recovery_rate     = input$sir_recovery_rate
    )

  # Generating random graph
  epiworldR::add_layer(
    model_sir,
    epiworldR::layer_network(
      "network",
      epiworldR::graph_smallworld(
        k        = input$sir_k,
        p        = input$sir_prob_rewiring,
        directed = as.logical(input$sir_directed)
      )
    )
  )

  # NPIs -----------------------------------------------------------------------
  interventions_add_all(model_sir, "sir", input)

  # Running and printing
  epiworldR::verbose_off(model_sir)
  epiworldR::run(model_sir, ndays = input$sir_n_days, seed = input$sir_seed)

  # Plot, summary and repnum
  plot_sir <- function() plot_epi(model_sir)
  summary_sir <- function() summary(model_sir)
  reproductive_sir <- function() plot_reproductive_epi(model_sir)
  # Table
  table_sir <- function() hist_table(model_sir)
  # Output list
  return(
    list(
      epicurves_plot     = plot_sir,
      reproductive_plot  = reproductive_sir,
      model_summary      = summary_sir,
      model_table        = table_sir
    )
  )
}

sir_panel <- function(model_alt) {

  shiny::conditionalPanel(
    simulate_button("sir"),
    condition = sprintf("input.model == '%s'", model_alt),
    text_input_disease_name("sir"),
    slider_prevalence("sir"),
    slider_input_rate("sir", "Transmission probability", "0.05", input_label = "transmission_rate"),
    slider_input_rate("sir", "Recovery probability (daily)", "0.14", input_label = "recovery_rate"),
    numeric_input_ndays("sir"),
    seed_input("sir"),
    network_input("sir"),
    npis_input("sir")
  )

}
