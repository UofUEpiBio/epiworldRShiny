# alt-name: SIR Network
shiny_sir <- function(input) {

  # Creating model
  model_sir <- epiworldR::ModelSIR(
    name              = input$sir_disease_name,
    prevalence        = input$sir_prevalence,
    transmission_rate = input$sir_transmission_rate,
    recovery_rate     = input$sir_recovery_rate
    )

  # Generating random graph
  epiworldR::agents_smallworld(
      model_sir,
      n = input$sir_population_size,
      k = input$sir_k,
      d = as.logical(input$sir_directed),
      p = input$sir_prob_rewiring
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
  table_sir <- function() {
    df <- as.data.frame(epiworldR::get_hist_total(model_sir))
    # Subset to only include "infection" state
    infection_data <- df[df$state == "Infected", ]
    # Row with the maximum count
    max_infection_row <- infection_data[which.max(infection_data$count), ]
    # Row number of the maximum count in the original data frame
    max_row_number <- which(df$date == max_infection_row$date &
                              df$state == "Infected")
    df[max_row_number,] <- sprintf("<strong>%s</strong>",
                                       df[max_row_number,])
    df
  }
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
    condition = sprintf("input.model == '%s'", model_alt),
    text_input_disease_name("sir"),
    slider_prevalence("sir"),
    slider_input_rate("sir", "Transmission probability", "0.05", input_label = "transmission_rate"),
    slider_input_rate("sir", "Recovery probability (daily)", "0.14", input_label = "recovery_rate"),
    numeric_input_ndays("sir"),
    seed_input("sir"),
    network_input("sir"),
    npis_input("sir"),
    simulate_button("sir")
  )

}
