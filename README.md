epiworldRShiny: A ‘shiny’ Wrapper of the R Package ‘epiworldR’
================

<!-- badges: start -->

[![R-CMD-check](https://github.com/UofUEpiBio/epiworldRShiny/actions/workflows/r.yml/badge.svg)](https://github.com/UofUEpiBio/epiworldRShiny/actions/workflows/r.yml)
[![CRAN
status](https://www.r-pkg.org/badges/version/epiworldRShiny)](https://CRAN.R-project.org/package=epiworldRShiny)
[![ForeSITE
Group](https://github.com/EpiForeSITE/software/raw/e82ed88f75e0fe5c0a1a3b38c2b94509f122019c/docs/assets/foresite-software-badge.svg)](https://github.com/EpiForeSITE)
<!-- badges: end -->

This R package provides a user-friendly application for
<a href="https://github.com/UofUEpiBio/epiworldR"
target="_blank">epiworldR</a>, a wrapper of the C++ library
<a href="https://github.com/UofUEpiBio/epiworld"
target="_blank">epiworld</a>. It provides a general framework for
modeling disease transmission using <a
href="https://en.wikipedia.org/w/index.php?title=Agent-based_model&amp;oldid=1153634802"
target="_blank">agent-based models</a>. Some of the main features
include:

- Fast simulation with an average of 30 million agents/day per second.
- 9 different epidemiological models to choose from.
- Built-in capability for user-defined interventions.
- Built-in capability to define population and disease parameters.
- Informative visualizations and tables after running each simulation.

You can find more examples on the package’s website:
<https://uofuepibio.github.io/epiworldRShiny/>

Try it out! Play around with a live version of the app hosted on
`shinyapps.io` here: <https://gvegayon.shinyapps.io/epiworldRShiny/>.

## Installation

You can install the development version of epiworldRShiny from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("UofUEpiBio/epiworldRShiny")
```

Or from CRAN

``` r
install.packages("epiworldRShiny")
```

## Running the ShinyApp

To run the app with only built-in models, use:

``` r
epiworldRShiny::run_app()
```

To add additional custom models, use:

``` r
epiworldRShiny::run_app(custom_models_path = "path/to/custom/models")
```

Note that custom models must follow the guidelines given in
[`inst/models/README.md`](inst/models/README.md).

## Examples

### Example \#1

This first example demonstrates how to run the Shiny app, run a
simulation, and observe results. Notice the sidebar contains many
disease and model parameters that can be altered. Changing these
parameters will affect the spread of the infectious disease in the
simulated population. After running the simulation, a plot of the
distribution of states over time, a plot of the disease’s reproductive
number, a model summary, and a table of counts over time are displayed.

This example features: - SEIR network model for COVID-19  
The day of peak infections occurs on day 12, maxing at about 18,000
infections.  
- The disease spreads rapidly at the simulation’s beginning, drastically
decreasing over the first ten days.  
- Model summary  
- State counts table

<figure>
<img
src="https://github.com/UofUEpiBio/epiworldRShiny/assets/105825983/f4e7d313-e3b6-4ebb-9c0a-ca4d53ef9cea"
alt="example 1 GIF" />
<figcaption aria-hidden="true">example 1 GIF</figcaption>
</figure>

### Example \#2

This example features the implementation of the vaccine and school
closure interventions to curb disease spread. All model output can be
interpreted using the same logic from example \#1.

Key features: - SEIRD network model for COVID-19  
- Vaccine prevalence = 70%  
- School closure prevalence = 50%  
- Day of school closure implementation = 7  
- Significantly decreased number of infections and deaths.  
- The majority of the population recovered or was susceptible by day 30.

<figure>
<img
src="https://github.com/UofUEpiBio/epiworldRShiny/assets/105825983/d5405162-f7fe-4a42-8a4c-e9a2ac31be73"
alt="example 2 GIF" />
<figcaption aria-hidden="true">example 2 GIF</figcaption>
</figure>

### Example \#3

The last example features the SEIR equity model. This model is unique
because it accounts for demographic diversity in a population, such as
race, gender, and age. This allows for comparing disease spread among
different demographics, unlike the previous two examples.

Key features: - SEIR equity model for COVID-19 - 30% hispanic
population, 70% non-hispanic - 52% female population - 30% of population
younger than 20 years old - 30% of population between 20 and 60 years
old - 40% of population older than 60.

<figure>
<img
src="https://github.com/UofUEpiBio/epiworldRShiny/assets/105825983/20aeb62d-cb42-4882-8577-a3406f167bca"
alt="example 3 GIF" />
<figcaption aria-hidden="true">example 3 GIF</figcaption>
</figure>
