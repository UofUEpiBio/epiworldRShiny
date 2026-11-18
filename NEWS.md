# epiworldRShiny 1.0.0-0 (dev)

Tracks epiworldR 1.0.0 and measles 0.5.0 (their `v1-layers` branches, see `Remotes`), where contacts come from contact layers, models are built by the registry, and a model may hold several diseases. All the apps are ported once, with the same inputs and UI unless noted.

## Apps

* **Network apps** (`sir`, `sis`, `sird`, `sisd`, `seir`, `seird`): the population size is an argument of the model, and the network is a layer, `add_layer(model, layer_network("network", graph_smallworld(k, p, directed)))`, instead of `agents_smallworld()`. The inputs (population size, number of ties, directed, probability of rewiring) are the same.

* **Random-mixing apps** (`sirconn`, `seirconn`, `seirconnequity`): the `*CONN` models are gone from epiworldR, so these apps build `ModelSIR()` / `ModelSEIR()` with a random-mixing layer: everybody in one group, a contact matrix of 1, and the model parameter "Contact rate" (the contact rate input) as `set_contact_rate()`. In epiworldR 1.0.0 contacts are distinct, not drawn with replacement, so results for a given seed change (the expected number of contacts does not).

* **SEIR equity**: the groups are now the age groups of a mixing layer (`layer_mixing("age", groups, contact_matrix)`, with the contact matrix proportional to the size of each group, i.e., random mixing as before), and the "Contact rate" input is the layer's contact rate. The difference in susceptibility by age, sex and ethnicity is the same logit tool. The virus's transmission probability, set with `set_prob_infecting()` (defunct), is now the model's `transmission_prob`. The plot of the infected counts "Removed" as recovered (the last stage of the SEIR family is "Removed").

* **Measles**: follows measles 0.5.0 (`ModelMeaslesSchool()` has the same arguments). The savers' tables have a `disease` column; the app selects columns by name.

## Shared code

* Names of arguments follow epiworldR 1.0.0: `transmission_prob` and `latent_period` (the inputs keep their labels, "Transmission probability" and "Incubation Days").

* The outputs have `status` and `stage` columns (and a leading `disease` column): `plot_epi()` plots the stages, adding up the statuses. The tables of the apps are now all wide (one column per stage, the day with the peak of infections in bold); before, the SIR, SIS, SIRD and SISD apps showed the long table with every column of the peak row in bold.

* `plot_epi()`: the lines of the stages other than Susceptible, Infected, Removed, Recovered and Deceased get distinct colors (the color was a number before).

* The interventions place the vaccine and masks with `assign_share()` (`add_tool(assign = )`) instead of `proportion`.

* Examples (`plot_epi()`, `plot_reproductive_epi()` and the interventions) use `ModelSEIR()` with a random-mixing layer.

## Tests

* New end-to-end tests (`inst/tinytest/`, run with `tinytest::test_package("epiworldRShiny")`): every app builds and runs its model with the default inputs of its panel, and the outputs it shows (plots, summary, table of counts) are checked; plus interventions and the measles app.

# epiworldRShiny 0.2.7 (dev)

## Measles model

* The measles model is now built with `ModelMeaslesSchool()` from the [measles](https://cran.r-project.org/package=measles) R package instead of `ModelMeaslesQuarantine()` from `epiworldR`. The app footer now reports the `measles` package version alongside the `epiworldRShiny` and `epiworldR` versions.

* Starting version 0.3.1-0, the `measles` R package sets the default vaccination efficacy to 0.97. The previous value (0.99) was established more as a rate. The new version sets 0.97 as an all-or-nothing model, which is more appropriate for the measles vaccine. This may result in some minor differences between the previous version of the measles model and the new version.

* Removed the "Vaccination Improved Recovery" slider, as `ModelMeaslesSchool()` does not take a `vax_improved_recovery` parameter.

* The default number of simulations increased from 100 to 200.

* The model description now ends with a collapsed "Model assumptions & references" table listing every `ModelMeaslesSchool()` parameter: the value the app uses, the `measles` package default, its source, and why the two differ (transmission probability 0.99 vs 0.9, contact rate 3.79 vs 4.17, and vaccination coverage 0.85 vs 0.93). Each measles input's help text now names its source, and the "Learn more" link points to the canonical parameter table in the `measles` package instead of the archived `EpiForeSITE/epiworld-measles` repository (EpiForeSITE/measles#5).

* Simulations are noticeably faster: results are now collected with the `outbreak_size`, `hospitalizations`, and `active_cases` savers introduced in epiworldR 0.10.0.0, rather than post-processing the full transition and history tables.

## School data

* Added school data for Utah (1,387 schools, sourced from the Utah Department of Health and Human Services) and Wisconsin. The bundled dataset now covers 49,324 schools across 25 states, up from 45,295 across 23.

* The bundled school data no longer includes enrollment counts, so selecting a school populates only the vaccination rate and leaves the school size at the default of 500 students; the app reports this below the "Population Size" input. Custom CSVs that provide a `num_students` column are still used to populate the school size automatically.

## Other changes

* New exported function `school_selector_server()`, which factors the cascading State → County → School selector (including CSV upload, validation, and reset) out of the measles model so that other models can reuse it. Uploaded CSVs may now leave `num_students` empty.

* The `data-raw` pipeline is split into `01_utah_school_data.R` and `02_download_and_combine.R`, driven by the `update-utah-data` and `update-other-data` make targets. See `data-raw/README.md`; note that the Utah source file is not tracked in git and must be obtained from Utah DHHS.

* Cleared outstanding `R CMD check` NOTEs and updated the development container to the rocker devcontainer image.


# epiworldRShiny 0.2.6 (dev)

## New features

* Added school selector feature to the measles model. Users can now select schools from a database to automatically populate vaccination rates and school sizes. The selector uses a cascading dropdown system (State → County → School) and supports uploading custom school data via CSV. School data sourced from the [epiENGAGE measles dashboard](https://github.com/TACC/measles-dashboard) prepared by the TACC team.

## Other changes

* Upgraded to epiworldR 0.10.0.0, which includes significant performance improvements. See the [epiworldR 0.10.0.0 release notes](https://github.com/UofUEpiBio/epiworldR/blob/main/NEWS.md#epiworldr-01000) for details.


# epiworldRShiny 0.2.5 (2025-05-28)

## User visible changes

* The measles model was not counting hospitalized cases correctly. It was excluding some cases from the reporting. The same happens with the number of active cases: it was missing two uncommon statuses in the model.

## Other changes

* Added Google Analytics tracking to the app, tracking page views and simulation runs.


# epiworldRShiny 0.2.4

* Fixed minor bug in the measles app that was causing errors in shinyapps.io.

# epiworldRShiny 0.2.3

* Updated to epiworldR version 0.8.2.0: New version of the measles model with new parameters.

* Improved the UI for the measles model to make it more user-friendly.

# epiworldRShiny 0.2.2

* Adds a `NEWS.md` file to track changes to the package.

* Adds new Measles model included in the most recent version of epiworldR (0.8.1.0).

* Refactors UI to only use bslib and move away from the shinydashboard R package and the shinyjs package.

* Adds ability to load in custom (user-defined) models

* Adds tooltips to the measles model parameter controls
