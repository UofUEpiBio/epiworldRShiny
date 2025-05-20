library(rsconnect)

rsconnect::setAccountInfo(
  name   = Sys.getenv("SHINY_USER"),
  token  = Sys.getenv("SHINY_TOKEN"),
  secret = Sys.getenv("SHINY_SECRET")
)

cat("Listing the current directory and its contents\n")
cat(getwd(), "\n")
list.files() |> cat("\n")

rsconnect::deployApp(
  appDir = file.path(getwd(), "epishiny"),
  appName = "epishiny",
  # appId = Sys.getenv("SHINY_APP_ID"),
  forceUpdate = TRUE,
  account = Sys.getenv("SHINY_USER"),
  logLevel = "verbose"
)
