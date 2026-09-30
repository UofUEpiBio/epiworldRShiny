# epiworldRShiny 0.2.7 (dev)

## Measles model

* The measles model is now built with `ModelMeaslesSchool()` from the [measles](https://cran.r-project.org/package=measles) R package instead of `ModelMeaslesQuarantine()` from `epiworldR`. The app footer now reports the `measles` package version alongside the `epiworldRShiny` and `epiworldR` versions.

* Starting version 0.3.1-0, the `measles` R package sets the default vaccination efficacy to 0.97. The previous value (0.99) was established more as a rate. The new version sets 0.97 as an all-or-nothing model, which is more appropriate for the measles vaccine. This may result in some minor differences between the previous version of the measles model and the new version.

* Removed the "Vaccination Improved Recovery" slider, as `ModelMeaslesSchool()` does not take a `vax_improved_recovery` parameter.

* The default number of simulations increased from 100 to 200.

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
