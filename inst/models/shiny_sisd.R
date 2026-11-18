# alt-name: Network SISD

shiny_sisd <- function(input) {

  # Creating model
  model_sisd <- epiworldR::ModelSISD(
    name              = input$sisd_disease_name,
    n                 = input$sisd_population_size,
    prevalence        = input$sisd_prevalence,
    transmission_prob = input$sisd_transmission_rate,
    recovery_rate     = input$sisd_recovery_rate,
    death_rate        = input$sisd_death_rate
    )

  # Generating random graph
  epiworldR::add_layer(
    model_sisd,
    epiworldR::layer_network(
      "network",
      epiworldR::graph_smallworld(
        k        = input$sisd_k,
        p        = input$sisd_prob_rewiring,
        directed = as.logical(input$sisd_directed)
      )
    )
  )

  # NPIs -----------------------------------------------------------------------
  interventions_add_all(model_sisd, "sisd", input)

  # Running and printing
  epiworldR::verbose_off(model_sisd)
  epiworldR::run(model_sisd, ndays = input$sisd_n_days, seed = input$sisd_seed)

  # Plot, summary, and repnum
  plot_sisd <- function() plot_epi(model_sisd)
  summary_sisd <- function() summary(model_sisd)
  reproductive_sisd <- function() plot_reproductive_epi(model_sisd)
  # Table
  table_sisd <- function() hist_table(model_sisd)
  # Output list
  return(
    list(
      epicurves_plot     = plot_sisd,
      reproductive_plot  = reproductive_sisd,
      model_summary      = summary_sisd,
      model_table        = table_sisd
    )
  )
}

sisd_panel <- function(model_alt) {

  shiny::conditionalPanel(
    simulate_button("sisd"),
    condition = sprintf("input.model == '%s'", model_alt),
    text_input_disease_name("sisd"),
    slider_prevalence("sisd"),
    slider_input_rate("sisd", "Transmission probability", 0.05, input_label = "transmission_rate"),
    slider_input_rate("sisd", "Recovery probability (daily)", 0.14, input_label = "recovery_rate"),
    slider_input_rate("sisd", "Death Rate", "0.01"),
    numeric_input_ndays("sisd"),
    seed_input("sisd"),
    network_input("sisd"),
    npis_input("sisd")
  )

}
