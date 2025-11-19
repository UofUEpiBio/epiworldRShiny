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
