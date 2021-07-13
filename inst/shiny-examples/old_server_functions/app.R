library(shiny)

# dataframe that holds usernames, passwords and other user data
user_base <- data.frame(
  username = c("user1", "user2"),
  password = c("pass1", "pass2"),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two"),
  stringsAsFactors = FALSE,
  row.names = NULL
)

ui <- fluidPage(
  # add logout button UI
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  # add login panel UI function
  shinyauthr::loginUI(id = "login"),
  # setup table output to show user info after login
  tableOutput("user_table")
)

server <- function(input, output, session) {

  # call the logout module with reactive trigger to hide/show
  logout_init <- callModule(
    shinyauthr::logout,
    id = "logout",
    active = reactive(credentials()$user_auth)
  )

  # call login module supplying data frame, user and password cols
  # and reactive trigger
  credentials <- callModule(
    shinyauthr::login,
    id = "login",
    data = user_base,
    user_col = username,
    pwd_col = password,
    log_out = reactive(logout_init())
  )

  # pulls out the user information returned from login module
  user_data <- reactive({
    credentials()$info
  })

  output$user_table <- renderTable({
    # use req to only render results when credentials()$user_auth is TRUE
    req(credentials()$user_auth)
    user_data()
  })
}

shinyApp(ui = ui, server = server)
