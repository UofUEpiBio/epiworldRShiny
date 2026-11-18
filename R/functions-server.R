
#' Population generator
#'
#' This function generates a population matrix with specified characteristics.
#'
#' @param n The number of individuals in the population.
#' @param prop_hispanic The proportion of individuals who are Hispanic.
#' Default is 0.5.
#' @param prop_female The proportion of individuals who are female. Default is
#' 0.5.
#' @param prop_19_59_60plus A vector of length 3 representing the proportions of
#' individuals in the age groups 0-19, 20-59, and 60+. Default is c(0.5, 0.3, 0.2).
#'
#' @return A matrix representing the generated population, with columns for age
#' groups (0-19, 20-59, 60+), NotHispanic, and Female.
#' @examples
#' pop_generator(n = 1000, prop_hispanic = .5, prop_female = .5,
#'               prop_19_59_60plus = c(.3, .6))
#' @export
#' @family Server side functions
pop_generator <- function(
  n,
  prop_hispanic = .5,
  prop_female   = .5,
  prop_19_59_60plus = c(.3, .6)
  ) {

  prop_19_59_60plus <- c(
    prop_19_59_60plus[1],
    diff(prop_19_59_60plus),
    1 - prop_19_59_60plus[2]
  )

  agegroups <- sample.int(
    3, size = n, replace = TRUE,
    prob = prop_19_59_60plus
    )

  X <- matrix(0, nrow = n, ncol = 3)
  X[cbind(1:n, agegroups)] <- 1
  colnames(X) <- c("0-19", "20-59", "60+")

  cbind(
    X,
    NotHispanic = sample.int(
      2, size = n, replace = TRUE,
      prob = c(1 - prop_hispanic, prop_hispanic)
      ) - 1,
    Female   = sample.int(
      2, size = n, replace = TRUE,
      prob = c(1 - prop_female, prop_female)
      ) - 1
  )

}

#' Find the right plotting-scale
#'
#' This function determines the scale of the y-axis for plot_epi.
#' @param x The maximum value found in the model stage counts
#'
#' @return An integer representing the scale for the y-axis. A max counts value
#' of 10000 will return a scale of 1, 100000 will return a scale of 1000,
#' 1000000 will return a scale of 10000.
#' @examples
#' find_scale(100000)
#'
#' @export
#' @family Server side functions
find_scale <- function(x) {
  res <- 10^(floor(log10(x)) + 1 - 3)
  if (res < 1000)
    return(1)
  res
}

#' Counts of each stage by day
#'
#' The history of a model (`get_hist_total()`) has one row per day, status and
#' stage. The apps plot stages, so the counts are added up over statuses.
#'
#' @param model An epiworldR model that has been run.
#' @return A data frame with columns `date`, `stage` and `counts`, with the
#' stages in the model's order.
#' @noRd
stage_counts <- function(model) {

  hist <- epiworldR::get_hist_total(model)
  res  <- stats::aggregate(counts ~ date + stage, data = hist, FUN = sum)

  # Keeping the order of the stages in the model
  res$stage <- factor(res$stage, levels = unique(hist$stage))
  res       <- res[order(res$date, res$stage), ]
  res$stage <- as.character(res$stage)
  rownames(res) <- NULL

  res

}

#' Table of the counts by day (wide, one column per stage)
#'
#' @param model An epiworldR model that has been run.
#' @return A data frame with a column `date` and one column per stage. The
#' count of the day with the peak of infections is in bold (HTML).
#' @noRd
hist_table <- function(model) {

  df <- stage_counts(model)

  # Row with the peak of infections
  infected <- df[df$stage == "Infected", ]
  peak     <- which(
    df$stage == "Infected" & df$date == infected$date[which.max(infected$counts)]
  )

  df$counts <- as.character(df$counts)
  df$counts[peak] <- sprintf("<strong>%s</strong>", df$counts[peak])

  df$stage <- factor(df$stage, levels = unique(df$stage))
  df <- stats::reshape(df, idvar = "date", timevar = "stage", direction = "wide")
  colnames(df) <- sub("^counts\\.", "", colnames(df))
  rownames(df) <- NULL

  df

}

#' Adds a random-mixing layer to a model
#'
#' Everybody is in one group and meets, on average, `contact_rate` others per
#' day (the model parameter "Contact rate", so it can be changed after the
#' model is built). This is what the `*CONN` models of epiworldR 0.x did.
#'
#' @param model An epiworldR model.
#' @param n Population size.
#' @param contact_rate Average number of contacts per day.
#' @return The model, invisibly.
#' @noRd
add_random_mixing <- function(model, n, contact_rate) {

  epiworldR::add_param(model, "Contact rate", contact_rate)

  epiworldR::add_layer(
    model,
    epiworldR::layer_mixing(
      "everyone",
      groups         = rep(0L, n),
      contact_matrix = 1
    ) |>
      epiworldR::set_contact_rate("Contact rate")
  )

  invisible(model)

}

#' Plot daily incidence
#'
#' This function generates a plot of the model stages over time
#' @param model The number of individuals in the population.
#' @param mark_max The stage which will have a mark at the peak
#'
#' @return A plot displaying each stage from the model over the course of the
#' simulation
#' @examples
#' library(epiworldR)
#' model <- ModelSEIR("COVID-19", n = 1000, prevalence = 0.05,
#'                    transmission_prob = 0.1, latent_period = 7,
#'                    recovery_rate = 0.14)
#' add_param(model, "Contact rate", 4)
#' add_layer(
#'   model,
#'   layer_mixing("everyone", groups = rep(0, 1000), contact_matrix = 1) |>
#'     set_contact_rate("Contact rate")
#' )
#' run(model, ndays = 100, seed = 123)
#' plot_epi(model, mark_max = "Infected")
#'
#' @export
#' @family Server side functions
plot_epi <- function(model, mark_max) {
  # If the user didn't specify mark_max
  if (missing(mark_max))
    mark_max <- "Infected"

  # The history by day and stage (statuses, i.e., where agents are, are added up)
  curves <- stage_counts(model)
  states <- unique(curves$stage)

  # Obtain time of peak infections
  df_model <- curves[curves$stage %in% mark_max, ]

  peak_time <- which.max(df_model$counts) - 1

  # Begin plotting code
  counts_scale <- find_scale(max(curves$counts))

  curves$counts <- curves$counts/counts_scale

  # Initialize date vector of size length for state names
  date_candidates <- integer(length = length(states))
  # Identify max date when the counts stop significantly changing by state

  benchmark_value <- diff(range(curves$counts))/200 # 0.5% of range

  for (i in 1L:length(states)) {
    date_candidates[i] <- with(
      curves[curves$stage == states[i],],
      sum(abs(diff(counts)) > benchmark_value )
      )
  }
  # Round the maximum date up to the nearest 10th
  max_date <- min(
    diff(range(curves$date)),
    max(ceiling(max(date_candidates) / 10L) * 10L, 10L)
  )

  # Defining range of x values by max date as the max
  # curves <- curves[curves$date < max_date,]
  # Defining range of y values
  counts_range <- range(curves$counts)

  # Using reshape to pivot the data
  curves_pivot <- reshape(
    data = curves,
    idvar = "date",
    timevar = "stage",
    direction = "wide",
    v.names = "counts"
  )
  # Renaming columns to remove the "counts." prefix
  names(curves_pivot) <- sub("counts\\.", "", names(curves_pivot))

  extract_words_in_parentheses <- function(model_name) {
    # Regular expression to extract words within parentheses
    extracted <- gregexpr("\\(([^)]+)\\)", model_name)  # Updated regex pattern

    # Extracting words within parentheses
    words_within_parentheses <- regmatches(model_name, extracted)

    # Extract the words without parentheses and combine instances with more than one word
    extracted_words <- gsub("[\\(\\)]", "", unlist(words_within_parentheses))

    # Combine multiple extracted words into a single string
    combined_words <- paste(extracted_words, collapse = " ")  # Combine words with a space

    # Return the extracted words within parentheses as a single string
    return(combined_words)
  }
  extracted_title <- extract_words_in_parentheses(get_name(model))
  title <- paste0(extracted_title, " Model")

  # Plotting
  plot <- plotly::plot_ly(data = curves_pivot, x = ~date)

  # Line colors
  # Define a vector of colors (adjust as needed for the number of states)
  line_colors <- c("blue", "orange", "purple")
  color_index <- 1  # Index for selecting colors from the vector
  for (state in states) {

    # Picking the line color
    col <- switch(
      state,
      Infected  = "red",
      Removed   = "green",
      Recovered = "green",
      Deceased  = "black",
      {
        color_index <- color_index + 1
        line_colors[(color_index - 2) %% length(line_colors) + 1]
      }
    )

    plot <- plot |>
      plotly::add_lines(
        y = as.formula(paste0("~`", state, "`")), name = state,
        line = list(color = col)
        )
  }

  if (peak_time < max(curves$date)){

    # Add a point at the moment of peak infections
    plot <- plot |>
      plotly::add_markers(
        x = peak_time,
        y = max(df_model$counts),
        marker = list(color = 'red', symbol = 0),
        name = "Max Infections",
        showlegend = FALSE
      )

    # Add a vertical dashed line at the moment of peak infections
    plot <- plot |>
      plotly::add_segments(
        x = ~peak_time,
        xend = ~peak_time,
        y = 0,
        yend = ~max(df_model$counts),
        line = list(color = 'red', dash = "dash"),
        name = "Max Infections",
        showlegend = TRUE
      )
  }

  plot <- plot |>
    plotly::layout(
      title  = title,
      xaxis  = list(title = 'Day (step)'),
      yaxis  = list(title = 'Population', range = counts_range),
      legend = list(
        x = 1,    # Places the legend on the right side
        y = 0.5,  # Puts it in the middle vertically
        # Sets the x-coordinate
        xanchor = 'right',
        yanchor = 'middle'
      )
    )

  return(plot)
}

#' Plot Rt
#'
#' This function generates a plot of the reproductive number over time
#' @param model The model object
#'
#' @return A plot displaying the reproductive number for the model over the
#' course of the simulation
#' @examples
#' library(epiworldR)
#' model <- ModelSEIR("COVID-19", n = 1000, prevalence = 0.05,
#'                    transmission_prob = 0.1, latent_period = 7,
#'                    recovery_rate = 0.14)
#' add_param(model, "Contact rate", 4)
#' add_layer(
#'   model,
#'   layer_mixing("everyone", groups = rep(0, 1000), contact_matrix = 1) |>
#'     set_contact_rate("Contact rate")
#' )
#' run(model, ndays = 100, seed = 123)
#' plot_reproductive_epi(model)
#' @export
#' @family Server side functions
plot_reproductive_epi <- function (model) {

  # Calculating average rep. number for each unique source_exposure_date
  rep_num <- epiworldR::get_reproductive_number(model)
  average_rt <- stats::aggregate(
    rt ~ source_exposure_date, data = rep_num, FUN = mean
    )

  # Plotting
  reproductive_plot <- plotly::plot_ly(
    data = average_rt, x = ~source_exposure_date,
    y = ~rt, type = 'scatter',
    mode = 'lines+markers'
    )

  reproductive_plot <- reproductive_plot |>
    plotly::layout(
      title = "Reproductive Number",
      xaxis = list(title = 'Day (step)'),
      yaxis = list(title = 'Average Rep. Number')
    )

  return(reproductive_plot)

}
