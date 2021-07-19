#' logout UI module
#'
#' Shiny UI Module for use with \link{logoutServer}
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param label label for the logout button
#' @param icon An optional \code{\link[shiny]{icon}} to appear on the button.
#' @param class bootstrap class for the logout button
#' @param style css styling for the logout button
#'
#' @return Shiny UI action button
#' @example inst/shiny-examples/basic/app.R
#' @export
logoutUI <- function(id, label = "Log out", icon = NULL, class = "btn-danger", style = "color: white;") {
  ns <- shiny::NS(id)

  shinyjs::hidden(
    shiny::actionButton(ns("button"), label, icon = icon, class = class, style = style)
  )
}

#' logout server module
#'
#' Shiny authentication module for use with \link{logoutUI}
#' 
#' This module uses shiny's new \link[shiny]{moduleServer} method as opposed to the \link[shiny]{callModule}
#' method used by the now deprecated \link{login} function and must be called differently in your app.
#' For details on how to migrate see the 'Migrating from callModule to moduleServer' section of 
#' \href{https://shiny.rstudio.com/articles/modules.html}{Modularizing Shiny app code}.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param active \code{reactive} supply the returned \code{user_auth} boolean reactive from \link{loginServer}
#'   here to hide/show the logout button
#' @param ... arguments passed to \link[shinyjs]{toggle}
#'
#' @return Reactive boolean, to be supplied as the \code{log_out} argument of the
#'   \link{loginServer} module to trigger the logout process
#'
#' @example inst/shiny-examples/basic/app.R
#' @export
logoutServer <- function(id, active, ...) {
  shiny::moduleServer(
    id,
    function (input, output, session) {
      shiny::observe({
        shinyjs::toggle(id = "button", condition = active(), ...)
      })
      
      # return reactive logout button tracker
      shiny::reactive({
        input$button
      })
    }
  )
}

#' logout server module (deprecated)
#' 
#' Deprecated. Use \link{logoutServer} instead.
#'
#' Shiny authentication module for use with \link{logoutUI}
#' 
#' Call via \code{shiny::callModule(shinyauthr::logout, "id", ...)}
#' 
#' This function is now deprecated in favour of \link{logoutServer} which uses shiny's new \link[shiny]{moduleServer} 
#' method as opposed to the \link[shiny]{callModule} method used by this function. 
#' See the \link{logoutServer} documentation For details on how to migrate.
#' 
#' @usage NULL
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param active [reactive] supply the returned \code{user_auth} boolean reactive from \link{login}
#'   here to hide/show the logout button
#'
#' @return Reactive boolean, to be supplied as the \code{log_out} argument of the
#'   \link{login} module to trigger the logout process
#'
#' @examples
#' \dontrun{
#' logout_init <- shiny::callModule(
#'   logout,
#'   id = "logout",
#'   active = reactive(user_credentials()$user_auth)
#' )
#' }
#'
#' @export
logout <- function(input, output, session, active) {
  .Deprecated(msg = paste0("'shinyauthr::logout' is deprecated. Use 'shinyauthr::logoutServer' instead.\n", 
                           "See ?shinyauthr::logoutServer for information on how to switch."))
  
  shiny::observe({
    if (active()) {
      shinyjs::show(id = "button", anim = TRUE, time = 1, animType = "fade")
    } else {
      shinyjs::hide(id = "button", anim = TRUE, time = 1, animType = "fade")
    }
  })

  # return reactive logout button tracker
  shiny::reactive({
    input$button
  })
}
