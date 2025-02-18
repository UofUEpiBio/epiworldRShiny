# alt-name: Network SIRD

shiny_sird <- function(input) {

  # Creating model
  model_sird <- epiworldR::ModelSIRD(
    name              = input$sird_disease_name,
    prevalence        = input$sird_prevalence,
    transmission_rate = input$sird_transmission_rate,
    recovery_rate     = input$sird_recovery_rate,
    death_rate        = input$sird_death_rate
    )

  # Generating random graph
  epiworldR::agents_smallworld(
      model_sird,
      n = input$sird_population_size,
      k = input$sird_k,
      d = as.logical(input$sird_directed),
      p = input$sird_prob_rewiring
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
  table_sird <- function() {
    df <- as.data.frame(epiworldR::get_hist_total(model_sird))
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
      epicurves_plot     = plot_sird,
      reproductive_plot  = reproductive_sird,
      model_summary      = summary_sird,
      model_table        = table_sird
    )
  )
}

sird_panel <- function(model_alt) {

  shiny::conditionalPanel(
    condition = sprintf("input.model == '%s'", model_alt),
    text_input_disease_name("sird"),
    slider_prevalence("sird"),
    slider_input_rate("sird", "Transmission probability", 0.05, input_label = "transmission_rate"),
    slider_input_rate("sird", "Recovery probability (daily)", 0.14, input_label = "recovery_rate"),
    slider_input_rate("sird", "Probability of death (daily)", 0.01, input_label = "death_rate"),
    numeric_input_ndays("sird"),
    seed_input("sird"),
    network_input("sird"),
    npis_input("sird"),
    simulate_button("sird")
  )

}

