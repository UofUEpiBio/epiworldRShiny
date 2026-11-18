# End-to-end test of the measles app: it builds the models (with and without
# quarantine) with the default inputs of its panel, runs a few simulations,
# and gives the outputs the app shows.

library(epiworldRShiny)

models_setup()

input <- list(
  measles_population_size           = 200L,
  measles_contact_rate              = 15 / .99 / 4,
  measles_prevalence                = 1L,
  measles_transmission_rate         = .99,
  measles_vax_efficacy              = .97,
  measles_incubation_days           = 12,
  measles_prodromal_period          = 4,
  measles_rash_period               = 3,
  measles_days_undetected           = 2,
  measles_hospitalization_rate      = .2,
  measles_hospitalization_duration  = 7,
  measles_prop_vaccinated           = .85,
  measles_quarantine_days           = 21L,
  measles_quarantine_willingness    = 1,
  measles_isolation_days            = 4L,
  measles_n_days                    = 30,
  measles_n_sims                    = 6L,
  measles_seed                      = 2023,
  measles_show_debug                = TRUE
)

out <- suppressMessages(
  get("shiny_measles", envir = epiworldRenv())(input)
)

expect_true(
  all(c("epicurves_plot", "model_summary", "summary_table", "model_table",
        "hospitalizations", "takehome_stats") %in% names(out))
)

expect_true(inherits(out$epicurves_plot(), "plotly"))

# Outbreak size probabilities, with and without quarantine
tab <- out$summary_table()
expect_equal(nrow(tab), 5L)
expect_equal(
  names(tab),
  c("Outbreak Size", "Probability WITHOUT Quarantine",
    "Probability WITH Quarantine")
)

# Take-home numbers: at least the initial case, at most the school
stats <- out$takehome_stats()
expect_true(all(unlist(stats) >= 1 & unlist(stats) <= 200))

hosp <- out$hospitalizations()
expect_true(hosp$quarantine$mean >= 0 && hosp$no_quarantine$mean >= 0)

# The table of outbreak sizes, one row per simulation, day and arm
dat <- out$model_table()
expect_true(all(c("sim_num", "date", "outbreak_size", "quarantine") %in% names(dat)))
expect_equal(sort(unique(dat$quarantine)), c(FALSE, TRUE))
expect_true(length(capture.output(out$model_summary())) > 0L)

# The real server renders the measles outputs
app <- suppressWarnings(epiworldRShiny())
inp <- input
inp$measles_show_debug <- FALSE
suppressMessages(shiny::testServer(app, {
  do.call(session$setInputs, c(list(model = "Measles in Schools"), inp))
  session$setInputs(simulate_measles = 1)
  expect_true(nchar(output$epicurves_plot) > 100)
  expect_true(nchar(output$summary_table) > 50)
}))
