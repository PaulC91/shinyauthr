logoutUI <- function(id) {
  ns <- NS(id)

  shinyjs::hidden(
    actionButton(ns("button"), "Log out", class = "btn-danger", style = "color: white;")
  )
}

logout <- function(input, output, session, active) {

  observeEvent(active(), ignoreInit = TRUE, {
    shinyjs::toggle(id = "button", anim = TRUE, time = 1, animType = "fade")
  })

  # return reactive logout button tracker
  reactive({input$button})
}
