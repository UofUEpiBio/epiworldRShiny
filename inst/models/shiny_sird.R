# alt-name: Network SIRD

shiny_sird <- function(input) {

  # Creating model
  model_sird <- epiworldR::ModelSIRD(
    name              = input$sird_disease_name,
    n                 = input$sird_population_size,
    prevalence        = input$sird_prevalence,
    transmission_prob = input$sird_transmission_rate,
    recovery_rate     = input$sird_recovery_rate,
    death_rate        = input$sird_death_rate
    )

  # Generating random graph
  epiworldR::add_layer(
    model_sird,
    epiworldR::layer_network(
      "network",
      epiworldR::graph_smallworld(
        k        = input$sird_k,
        p        = input$sird_prob_rewiring,
        directed = as.logical(input$sird_directed)
      )
    )
  )

  # NPIs -----------------------------------------------------------------------
  interventions_add_all(model_sird, "sird", input)

  # Running and printing
  epiworldR::verbose_off(model_sird)
  epiworldR::run(model_sird, ndays = input$sird_n_days, seed = input$sird_seed)

  # Plot, summary, and repnum
  plot_sird <- function() plot_epi(model_sird)
  summary_sird <- function() summary(model_sird)
  reproductive_sird <- function() plot_reproductive_epi(model_sird)
  # Table
  table_sird <- function() hist_table(model_sird)
  # Output list
  return(
    list(
      epicurves_plot     = plot_sird,
      reproductive_plot  = reproductive_sird,
      model_summary      = summary_sird,
      model_table        = table_sird
    )
  )
}

sird_panel <- function(model_alt) {

  shiny::conditionalPanel(
    simulate_button("sird"),
    condition = sprintf("input.model == '%s'", model_alt),
    text_input_disease_name("sird"),
    slider_prevalence("sird"),
    slider_input_rate("sird", "Transmission probability", 0.05, input_label = "transmission_rate"),
    slider_input_rate("sird", "Recovery probability (daily)", 0.14, input_label = "recovery_rate"),
    slider_input_rate("sird", "Probability of death (daily)", 0.01, input_label = "death_rate"),
    numeric_input_ndays("sird"),
    seed_input("sird"),
    network_input("sird"),
    npis_input("sird")
  )

}

