# alt-name: SEIR Equity
shiny_seirconnequity <- function(input) {

  # input <- list(
  #   seirconnequity_disease_name      = "COVID",
  #   seirconnequity_population_size   = 1e4,
  #   seirconnequity_prevalence        = .1,
  #   seirconnequity_transmission_rate = .05,
  #   seirconnequity_recovery_rate     = 1/7,
  #   seirconnequity_contact_rate      = 5,
  #   seirconnequity_incubation_days   = 7,
  #   seirconnequity_n_days            = 100,
  #   seirconnequity_seed              = 445
  # )

  n <- input$seirconnequity_population_size

  model_seirconnequity <- epiworldR::ModelSEIRCONN(
    name              = input$seirconnequity_disease_name,
    prevalence        = input$seirconnequity_prevalence,
    transmission_rate = input$seirconnequity_transmission_rate,
    recovery_rate     = input$seirconnequity_recovery_rate,
    contact_rate      = input$seirconnequity_contact_rate,
    n                 = n,
    incubation_days   = input$seirconnequity_incubation_days
    )

  set_name(model_seirconnequity, "SEIR Equity Model")

  # Generating artificial pop data ---------------------------------------------
  set.seed(input$seirconnequity_seed)

  X <- pop_generator(
    n,
    prop_hispanic = input$seirconnequity_prop_hispanic,
    prop_female = input$seirconnequity_prop_female,
    prop_19_59_60plus = input$seirconnequity_prop_ages
    )

  # Saving the data to the global environment (this way we make sure it is
  # available to the model)
  assign("X", X, envir=epiworldRenv())

  # Adding population data
  set_agents_data(
    model = model_seirconnequity,
    data  = epiworldRenv()$X
    )

  # Creating immune system to add the difference in susceptibility -------------
  immune <- tool(
    "Immune system",
    susceptibility_reduction = 0,
    transmission_reduction = 0,
    recovery_enhancer = 0,
    death_reduction = 0
    )

  # Setting logit function for the virus to be more infectious for some groups
  # This is done with susceptibility reduction. The smallest the value, the
  # more infectious the virus is for that group.
  tfun <- tool_fun_logit(
    vars = 0:4,
    coefs = log(c( # Defined in terms of odds
      1.000, # 0-19
      4.000, # 20-59
      0.001, # 60+
      4.000, # NonHispanic
      2.000  # Female
    )),
    model = model_seirconnequity
  )

  # Setting the virus
  set_susceptibility_reduction_fun(
    tool  = immune,
    model = model_seirconnequity,
    tfun  = tfun
    )

  # Adding the tool to the model
  add_tool(
    model      = model_seirconnequity,
    tool       = immune,
    proportion = 1
    )

  # effective trate (user defined) = t rate virus x (1 - suscept redux)
  # we use the middle as a reference
  set_prob_infecting(
    virus = get_virus(model_seirconnequity, 0),
    prob  = min(1, input$seirconnequity_transmission_rate/
      (1 - plogis(log(4.0))))
  )

  # NPIs -----------------------------------------------------------------------
  interventions_add_all(model_seirconnequity, "seirconnequity", input)

  # Running and printing
  epiworldR::verbose_off(model_seirconnequity)
  epiworldR::run(model_seirconnequity, ndays = input$seirconnequity_n_days, seed = input$seirconnequity_seed)

  # Equity plots ---------------------------------------------------------------
  agents_states <- epiworldR::get_agents_states(model_seirconnequity)

  agents <- data.frame(
    id = 1:n,
    Age = colnames(X)[1:3][max.col(X[, 1:3])],
    Sex = c("Male", "Female")[X[, "Female"] + 1],
    Race = c("Hispanic", "Non-hispanic")[X[, "NotHispanic"] + 1],
    State = agents_states,
    check.names = FALSE
  )

  # Common plots ---------------------------------------------------------------

# Plot, summary, and reproductive number
  plot_seirconnequity <- function() {

    # We treat recovered and exposed as infected
    agents$State <- ifelse(
      agents$State %in% c("Recovered", "Exposed"),
      "Infected", agents$State
      )

    subset(agents, State != "Susceptible") |>
      ggplot(aes(y = Age)) +
        geom_bar(aes(fill = Sex), position = "dodge") +

        facet_wrap(~Race) +
        labs(
          title = "Total number of infected",
          x     = "Number of infected",
          y     = "Age group"
        )

  }
  summary_seirconnequity <- function() summary(model_seirconnequity)
  reproductive_seirconnequity <-
    function() plot_reproductive_epi(model_seirconnequity)

  # Table
  table_seirconnequity <- function() {
    df <- as.data.frame(epiworldR::get_hist_total(model_seirconnequity))
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
      epicurves_plot     = plot_seirconnequity,
      reproductive_plot  = reproductive_seirconnequity,
      model_summary      = summary_seirconnequity,
      model_table        = table_seirconnequity
    )
  )
}

seirconnequity_panel <- function(model_alt) {

  shiny::conditionalPanel(
    simulate_button("seirconnequity"),
    condition = sprintf("input.model == '%s'", model_alt),
    text_input_disease_name("seirconnequity"),
    slider_prevalence("seirconnequity"),
    slider_input_rate("seirconnequity", "Transmission probability", 0.05, input_label = "transmission_rate"),
    slider_input_rate("seirconnequity", "Recovery probability (daily)", 0.14, input_label = "recovery_rate"),
    slider_input_rate("seirconnequity", "Contact Rate", 4, maxval = 20),
    shiny::numericInput(
      inputId = "seirconnequity_incubation_days",
      label   = "Incubation Days",
      value   = 7,
      min     = 0,
      max     = NA,
      step    = 1
      ),
    shiny::sliderInput(
      inputId = "seirconnequity_population_size",
      label   = "Population Size",
      min     = 0,
      max     = 100000,
      value   = 50000,
      step    = 1000,
      ticks   = FALSE
    ),
    numeric_input_ndays("seirconnequity"),
    seed_input("seirconnequity"),
    population_input("seirconnequity"),
    npis_input("seirconnequity")
  )

}
