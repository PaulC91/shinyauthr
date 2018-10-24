loginUI <- function(id) {
  ns <- NS(id)

  div(id = ns("panel"), style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
      wellPanel(
        tags$h2("Please log in", class = "text-center", style = "padding-top: 0;"),

        textInput(ns("user_name"), "User Name"),

        passwordInput(ns("password"), "Password"),

        div(
          style = "text-align: center;",
          actionButton(ns("button"), "Log in", class = "btn-primary", style = "color: white;")
        ),

        shinyjs::hidden(
          tags$div(id = ns("error"),
                   tags$p("Invalid username or password!",
                     style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center"))
        )
      )
  )
}

login <- function(input, output, session, data, user_col, pwd_col,
                  hashed = FALSE, algo = NULL, log_out = NULL) {

  credentials <- reactiveValues(user_auth = FALSE)

  observeEvent(log_out(), {
    credentials$user_auth <- FALSE
  })

  observeEvent(credentials$user_auth, ignoreInit = TRUE, {
    shinyjs::toggle(id = "panel")
  })

  users <- enquo(user_col)
  pwds <- enquo(pwd_col)

  observeEvent(input$button, {

    if(!hashed) {
      data <- mutate(data,  !! pwds := sapply(!! pwds, digest))
    }

    row_username <- which(pull(data, !! users) == input$user_name)
    row_password <- which(pull(data, !! pwds) == digest(input$password)) # digest() makes md5 hash of password

    # if user name row and password name row are same, credentials are valid
    if (length(row_username) == 1 &&
        length(row_password) >= 1 &&  # more than one user may have same pw
        (row_username %in% row_password)) {
      credentials$user_auth <- TRUE
      credentials$info <- filter(data, !! users == input$user_name)
    } else {
      shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade")
      shinyjs::delay(5000, shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade"))
    }

  })

  # return reactive list containing auth boolean and user information
  reactive({
    reactiveValuesToList(credentials)
  })

}
