% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app.R
\name{epiworldRShiny}
\alias{epiworldRShiny}
\alias{run_app}
\title{epiworldRShiny App Launcher}
\usage{
epiworldRShiny(custom_models_path = NULL, ...)

run_app(custom_models_path = NULL, ...)
}
\arguments{
\item{custom_models_path}{Optional path to custom model files (see details).}

\item{...}{Currently ignored.}
}
\value{
Loads and opens the R shiny app for the epiworldR package
}
\description{
Fires up the R Shiny App. You can find more examples and documentation at
the package's website: \url{https://UofUEpiBio.github.io/epiworldRShiny/}.

\code{run_app} is a wrapper for the \code{epiworldRShiny} function. It is a
convenience function to run the app.
}
\details{
When \code{custom_models_path} is specified, the function will look for valid model files
at the specified path. These will be added to the list of available models.
The function expects R files named \verb{shiny_<model_name>.R} which contain the model.
The function will also look for optional Markdown files named \verb{shiny_<model_name>.md}
which contain the model description.
}
