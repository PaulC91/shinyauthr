#' Run shinyauthr example
#'
#' Launch example shiny dashboard using shinyauthr authentication modules
#'
#' @author Paul Campbell, \email{pacampbell91@gmail.com}
#'
#' @export
runShinyExample <- function() {
  appDir <- system.file("shiny-examples", "shinyauthr_example", package = "shinyauthr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `shinyauthr`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}