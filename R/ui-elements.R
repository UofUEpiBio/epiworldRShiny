##############################################
# Private UI Elements (not exported in the package)
##############################################

# Creates a link to GitHub repo
github_a <- function(label, href) {
  shiny::a(
    shiny::icon("github"),
    label,
    href = href,
    target = "_blank"
  )
}

# Link to epiworldR GitHub
epiworldr_github <- github_a(
  "epiworldR",
  "https://github.com/UofUEpiBio/epiworldR"
)

# Link to epiworldRShiny GitHub
epiworldrshiny_github <- github_a(
  "epiworldRShiny",
  "https://github.com/UofUEpiBio/epiworldRShiny"
)

# Footer for the model panel
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

##############################################
# Public UI Elements (exported in the package)
##############################################
