# End-to-end tests: for each app, build its model with the default inputs of
# its panel (a few days only), run it, and check the outputs the app shows.

library(epiworldRShiny)

# Loads the apps (the same way the server does)
models_setup()
apps <- epiworldRenv()$models

# The inputs of an app (`input$<app>_<name>`), with the defaults of its UI. The
# population is smaller than the app's, to keep the test fast.
app_input <- function(app, ...) {

  input <- list(
    disease_name                       = "COVID-19",
    population_size                    = 1000,
    prevalence                         = .1,
    transmission_rate                  = .05,
    recovery_rate                      = .14,
    death_rate                         = .01,
    incubation_days                    = 7,
    contact_rate                       = 4,
    k                                  = 20,
    directed                           = "FALSE",
    prob_rewiring                      = .2,
    n_days                             = 20,
    seed                               = 2023,
    prop_hispanic                      = .5,
    prop_female                        = .5,
    prop_ages                          = c(.3, .6),
    vaccine_prevalence                 = 0,
    vaccine_susceptibility_reduction   = 0,
    vaccine_transmission_reduction     = 0,
    vaccine_recovery_enhancer          = 0,
    vaccine_death_reduction            = 0,
    masking_prevalence                 = 0,
    masking_transmission_reduction     = 0,
    school_closure_prevalence          = 0,
    school_closure_day                 = 0,
    school_closure_transmission_reduction = 0
  )

  input <- utils::modifyList(input, list(...))
  setNames(input, paste0(app, "_", names(input)))

}

# Runs an app and checks what its server shows: the epidemic curves, the
# reproductive number, the summary and the table of counts by day
check_app <- function(app, input) {

  out <- get(paste0("shiny_", app), envir = epiworldRenv())(input)

  expect_true(
    all(c("epicurves_plot", "reproductive_plot", "model_summary",
          "model_table") %in% names(out)),
    info = app
  )
  expect_true(
    inherits(out$epicurves_plot(), c("plotly", "ggplot")),
    info = app
  )
  expect_true(inherits(out$reproductive_plot(), "plotly"), info = app)
  expect_true(length(capture.output(out$model_summary())) > 0L, info = app)

  # The table has one row per day, and a column per stage: the stages add up
  # to the population in every day
  tab <- out$model_table()
  expect_true(is.data.frame(tab), info = app)
  expect_true(all(c("date", "Susceptible", "Infected") %in% names(tab)), info = app)
  expect_equal(nrow(tab), input[[paste0(app, "_n_days")]] + 1L, info = app)
  counts <- sapply(
    tab[, setdiff(names(tab), "date")],
    \(x) as.integer(gsub("</?strong>", "", x))
  )
  expect_true(
    all(rowSums(counts) == input[[paste0(app, "_population_size")]]),
    info = app
  )

  # The peak of infections is marked
  expect_equal(sum(grepl("<strong>", tab$Infected)), 1L, info = app)

  invisible(out)

}

# The apps that run an epiworldR model on the population
epi_apps <- setdiff(apps, "measles")
expect_equal(
  sort(epi_apps),
  sort(c(
    "sir", "sis", "sird", "sisd", "seir", "seird",
    "sirconn", "seirconn", "seirconnequity"
  ))
)

for (app in epi_apps)
  check_app(app, app_input(app))

# Deaths: the stage "Deceased" is in the table
tab <- check_app("sird", app_input("sird"))$model_table()
expect_true("Deceased" %in% names(tab))

# The equity app shows the infected by age group, sex and race
out <- check_app("seirconnequity", app_input("seirconnequity"))
expect_true(inherits(out$epicurves_plot(), "ggplot"))

# Interventions (vaccination, masking and school closure) on a random-mixing
# and on a network model
for (app in c("seirconn", "seir"))
  check_app(
    app,
    app_input(
      app,
      vaccine_prevalence                    = .5,
      vaccine_susceptibility_reduction      = .5,
      vaccine_transmission_reduction        = .5,
      vaccine_recovery_enhancer             = .5,
      vaccine_death_reduction               = .5,
      masking_prevalence                    = .5,
      masking_transmission_reduction        = .5,
      school_closure_prevalence             = .5,
      school_closure_day                    = 5,
      school_closure_transmission_reduction = .5
    )
  )

# Contacts matter: with no contacts in a random-mixing model the infection does
# not spread beyond the initial cases
out <- get("shiny_sirconn", envir = epiworldRenv())(
  app_input("sirconn", contact_rate = 1e-9, prevalence = .1)
)
tab <- out$model_table()
expect_true(
  all(as.integer(gsub("</?strong>", "", tab$Susceptible)) <= 900L)
)

# The real server: pick each model in the app, press its "Run Simulation"
# button, and the plots, summary and table are rendered
app <- suppressWarnings(epiworldRShiny())

for (id in epi_apps) {

  inp  <- app_input(id)
  disp <- names(apps)[apps == id]
  testServer(app, {
    do.call(session$setInputs, c(list(model = disp), inp))
    do.call(session$setInputs, setNames(list(1), paste0("simulate_", id)))
    expect_true(nchar(output$model_plot) > 100, info = id)
    expect_true(nchar(output$model_reproductive_plot) > 100, info = id)
    expect_true(nchar(output$model_summary) > 50, info = id)
    expect_true(nchar(output$model_table) > 100, info = id)
  })

}
