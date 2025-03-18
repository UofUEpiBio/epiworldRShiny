# alt-name: Network SEIRD

shiny_seird <- function(input) {

  model_seird <- epiworldR::ModelSEIRD(
    name              = input$seird_disease_name,
    prevalence        = input$seird_prevalence,
    transmission_rate = input$seird_transmission_rate,
    recovery_rate     = input$seird_recovery_rate,
    incubation_days   = input$seird_incubation_days,
    death_rate        = input$seird_death_rate
    )

  # Generating random graph
  epiworldR::agents_smallworld(
    model_seird,
    n = input$seird_population_size,
    k = input$seird_k,
    d = as.logical(input$seird_directed),
    p = input$seird_prob_rewiring
  )

  # NPIs -----------------------------------------------------------------------
  interventions_add_all(model_seird, "seird", input)

  # Running and printing
  epiworldR::verbose_off(model_seird)
  epiworldR::run(model_seird, ndays = input$seird_n_days, seed = input$seird_seed)
  # Plot
  plot_seird <- function() plot_epi(model_seird)
  # Summary
  summary_seird <- function() summary(model_seird)
  # Reproductive Number
  reproductive_seird <- function() plot_reproductive_epi(model_seird)
  # Table
  table_seird <- function() {
    df <- as.data.frame(epiworldR::get_hist_total(model_seird))
    # Subset to only include "infection" state
    infection_data <- df[df$state == "Infected", ]
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
      levels = c("Susceptible", "Exposed", "Infected", "Removed")
      )
    # Reshaping the data to wide format
    df <- reshape(df, idvar = "date", timevar = "state", direction = "wide")
    colnames(df) <- gsub(colnames(df), pattern = "counts.", replacement = "")
    df
  }

  # Output list
  return(
    list(
      epicurves_plot     = plot_seird,
      reproductive_plot  = reproductive_seird,
      model_summary      = summary_seird,
      model_table        = table_seird
    )
  )

}

seird_panel <- function(model_alt) {
  shiny::conditionalPanel(
    simulate_button("seird"),
    condition = sprintf("input.model == '%s'", model_alt),
    text_input_disease_name("seird"),
    slider_prevalence("seird"),
    slider_input_rate("seird", "Transmission probability", "0.05", input_label = "transmission_rate"),
    slider_input_rate("seird", "Recovery probability (daily)", "0.14", input_label = "recovery_rate"),
    slider_input_rate("seird", "Probability of death (daily)", 0.01, input_label = "death_rate"),
    shiny::numericInput(
      inputId = "seird_incubation_days",
      label   = "Incubation Days",
      value   = "7",
      min     = 0,
      max     = NA,
      step    = 1
      ),
    numeric_input_ndays("seird"),
    seed_input("seird"),
    network_input("seird"),
    npis_input("seird")
  )
}
