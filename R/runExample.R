#' Run shinyauthr examples
#'
#' Launch an example shiny app using shinyauthr authentication modules.
#' Use user1 pass1 or user2 pass2 to login.
#' 
#' @param example The app to launch. Options are "basic", "shinydashboard" or "navbarPage"
#' @return No return value, a shiny app is launched.
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   runExample("basic")
#'   runExample("shinydashboard")
#'   runExample("navbarPage")
#' }
#' @export
runExample <- function(example = c("basic", "shinydashboard", "navbarPage")) {
  example <- match.arg(example, c("basic", "shinydashboard", "navbarPage"), several.ok = FALSE)
  appDir <- system.file("shiny-examples", example, package = "shinyauthr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `shinyauthr`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
