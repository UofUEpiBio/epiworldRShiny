# alt-name: Network SEIR
shiny_seir <- function(input) {

  model_seir <- epiworldR::ModelSEIR(
    name              = input$seir_disease_name,
    prevalence        = input$seir_prevalence,
    transmission_rate = input$seir_transmission_rate,
    recovery_rate     = input$seir_recovery_rate,
    incubation_days   = input$seir_incubation_days
    )

  # Generating random graph
  epiworldR::agents_smallworld(
    model_seir,
    n = input$seir_population_size,
    k = input$seir_k,
    d = as.logical(input$seir_directed),
    p = input$seir_prob_rewiring
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
  table_seir <- function() {

    df <- as.data.frame(epiworldR::get_hist_total(model_seir))

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
