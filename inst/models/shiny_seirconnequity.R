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

  # Effective transmission probability (user defined) = transmission prob. of
  # the virus x (1 - susceptibility reduction). We use the middle group (odds of
  # 4) as a reference.
  model_seirconnequity <- epiworldR::ModelSEIR(
    name              = input$seirconnequity_disease_name,
    n                 = n,
    prevalence        = input$seirconnequity_prevalence,
    transmission_prob = min(
      1,
      input$seirconnequity_transmission_rate / (1 - plogis(log(4.0)))
    ),
    recovery_rate     = input$seirconnequity_recovery_rate,
    latent_period     = input$seirconnequity_incubation_days
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

  # Mixing layer: the groups are the age groups. Everybody meets, on average,
  # `contact_rate` others per day, in proportion to the size of each group
  # (the contact matrix is the expected number of contacts per day: its rows
  # add up to the contact rate, which is the model parameter "Contact rate").
  age_group <- max.col(X[, 1:3], ties.method = "first") - 1L
  age_prop  <- tabulate(age_group + 1L, nbins = 3L) / n

  add_param(
    model_seirconnequity, "Contact rate", input$seirconnequity_contact_rate
  )

  add_layer(
    model_seirconnequity,
    layer_mixing(
      "age",
      groups         = age_group,
      contact_matrix = matrix(age_prop, nrow = 3, ncol = 3, byrow = TRUE)
    ) |>
      set_contact_rate("Contact rate")
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
    assign     = assign_share(1)
    )

  # NPIs -----------------------------------------------------------------------
  interventions_add_all(model_seirconnequity, "seirconnequity", input)

  # Running and printing
  epiworldR::verbose_off(model_seirconnequity)
  epiworldR::run(model_seirconnequity, ndays = input$seirconnequity_n_days, seed = input$seirconnequity_seed)

  # Equity plots ---------------------------------------------------------------
  agents_stages <- epiworldR::get_agents_stages(model_seirconnequity)

  agents <- data.frame(
    id = 1:n,
    Age = colnames(X)[1:3][max.col(X[, 1:3])],
    Sex = c("Male", "Female")[X[, "Female"] + 1],
    Race = c("Hispanic", "Non-hispanic")[X[, "NotHispanic"] + 1],
    Stage = agents_stages,
    check.names = FALSE
  )

  # Common plots ---------------------------------------------------------------

# Plot, summary, and reproductive number
  plot_seirconnequity <- function() {

    # We treat recovered and exposed as infected
    agents$Stage <- ifelse(
      agents$Stage %in% c("Removed", "Exposed"),
      "Infected", agents$Stage
      )

    subset(agents, Stage != "Susceptible") |>
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
  table_seirconnequity <- function() hist_table(model_seirconnequity)

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
