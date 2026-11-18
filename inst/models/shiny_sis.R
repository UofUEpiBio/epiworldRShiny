# alt-name: Network SIS

shiny_sis <- function(input) {

  # Creating model
  model_sis <- epiworldR::ModelSIS(
    name              = input$sis_disease_name,
    n                 = input$sis_population_size,
    prevalence        = input$sis_prevalence,
    transmission_prob = input$sis_transmission_rate,
    recovery_rate     = input$sis_recovery_rate
    )

  # Creating network
  epiworldR::add_layer(
    model_sis,
    epiworldR::layer_network(
      "network",
      epiworldR::graph_smallworld(
        k        = input$sis_k,
        p        = input$sis_prob_rewiring,
        directed = as.logical(input$sis_directed)
      )
    )
  )

  # NPIs -----------------------------------------------------------------------
  interventions_add_all(model_sis, "sis", input)
  
  # Running and printing
  epiworldR::verbose_off(model_sis)
  epiworldR::run(model_sis, ndays = input$sis_n_days, seed = input$sis_seed)

  # Plot, summary, and reproductive number
  plot_sis <- function() plot_epi(model_sis)

  summary_sis <- function() summary(model_sis)

  reproductive_sis <- function() plot_reproductive_epi(model_sis)

  # Table
  table_sis <- function() hist_table(model_sis)

  # Output list
  return(
    list(
      epicurves_plot     = plot_sis,
      reproductive_plot  = reproductive_sis,
      model_summary      = summary_sis,
      model_table        = table_sis
    )
  )

}

sis_panel <- function(model_alt) {

  shiny::conditionalPanel(
    simulate_button("sis"),
    condition = sprintf("input.model == '%s'", model_alt),
    text_input_disease_name("sis"),
    slider_prevalence("sis"),
    slider_input_rate("sis", "Transmission probability", 0.05, input_label = "transmission_rate"),
    slider_input_rate("sis", "Recovery probability (daily)", 0.14, input_label = "recovery_rate"),
    numeric_input_ndays("sis"),
    seed_input("sis"),
    network_input("sis"),
    npis_input("sis")
  )

}
