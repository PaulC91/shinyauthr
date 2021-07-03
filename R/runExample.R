#' Run shinyauthr example
#'
#' Launch example shiny app using shinyauthr authentication modules
#'
#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "test_app", package = "shinyauthr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `shinyauthr`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}

#' Run shinyauthr example
#'
#' Launch example shinydashboard using shinyauthr authentication modules
#'
#' @export
runShinyDashboardExample <- function() {
  appDir <- system.file("shiny-examples", "shinydashboard", package = "shinyauthr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `shinyauthr`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}

#' Run navbarPage example
#'
#' Launch example shiny navbarPage app using shinyauthr authentication modules
#'
#' @export
runNavbarPageExample <- function() {
  appDir <- system.file("shiny-examples", "navbarPage", package = "shinyauthr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `shinyauthr`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
