#' logout UI module
#'
#' Shiny UI Module for use with \link{logout}
#' 
#' Call via \code{logoutUI("your_id")}
#'
#' @param id Shiny id
#'
#' @return Shiny UI action button
#'
#' @author Paul Campbell, \email{pacampbell91@gmail.com}
#'
#' @export
logoutUI <- function(id) {
  ns <- shiny::NS(id)

  shinyjs::hidden(
    shiny::actionButton(ns("button"), "Log out", class = "btn-danger", style = "color: white;")
  )
}

#' logout server module
#'
#' Shiny authentication module for use with \link{logoutUI}
#'
#' Call via \code{shiny::callModule(logout, "your_id", ...)}
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param active [reactive] supply the returned \code{user_auth} boolean reactive from \link{login} 
#'   here to hide/show the logout button
#'
#' @return The reactive output of this module should be supplied as the \code{log_out} argument to the 
#'   \link{login} module to trigger the logout process
#'
#' @author Paul Campbell, \email{pacampbell91@gmail.com}
#' 
#' @examples
#' \dontrun{
#'   logout_init <- shiny::callModule(logout, "logout", 
#'                                    active = reactive(user_credentials()$user_auth))
#' }
#'
#' @export
logout <- function(input, output, session, active) {

  shiny::observeEvent(active(), ignoreInit = TRUE, {
    shinyjs::toggle(id = "button", anim = TRUE, time = 1, animType = "fade")
  })

  # return reactive logout button tracker
  shiny::reactive({input$button})
}
