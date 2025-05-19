github_a <- function(label, href) {
  shiny::a(
    shiny::icon("github"),
    label,
    href = href,
    target = "_blank"
  )
}

epiworldr_github <- github_a(
  "epiworldR",
  "https://github.com/UofUEpiBio/epiworldR"
)
epiworldrshiny_github <- github_a(
  "epiworldRShiny",
  "https://github.com/UofUEpiBio/epiworldRShiny"
)

model_panel_footer <- shiny::div(
  shiny::markdown(
    paste(
      "epiworldRShiny version",
      utils::packageVersion("epiworldRShiny"),
      "| epiworldR version",
      utils::packageVersion("epiworldR")
    )
  ),
  shiny::markdown("**The University of Utah**"),
  style = "font-size:80%; text-align: center;"
)
