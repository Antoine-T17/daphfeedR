#' Run daphfeedR
#' @export
run_app <- function(..., launch.browser = TRUE) {
  app_dir <- system.file("app", package = "daphFeedR")
  if (app_dir == "") stop("App directory not found. Is 'daphFeedR' installed?")

  withr::with_dir(app_dir, shiny::runApp(app_dir, launch.browser = launch.browser, ...))
}

