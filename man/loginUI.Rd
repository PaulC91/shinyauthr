% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/login.R
\name{loginUI}
\alias{loginUI}
\title{login UI module}
\usage{
loginUI(
  id,
  title = "Please log in",
  user_title = "User Name",
  pass_title = "Password",
  login_title = "Log in",
  login_btn_class = "btn-primary",
  error_message = "Invalid username or password!",
  additional_ui = NULL,
  cookie_expiry = 7
)
}
\arguments{
\item{id}{An ID string that corresponds with the ID used to call the module's server function}

\item{title}{header title for the login panel}

\item{user_title}{label for the user name text input}

\item{pass_title}{label for the password text input}

\item{login_title}{label for the login button}

\item{login_btn_class}{bootstrap class for the login button. defaults to "btn-primary"}

\item{error_message}{message to display after failed login}

\item{additional_ui}{additional shiny UI element(s) to add below login button. Wrap multiple inside \code{shiny::tagList()}}

\item{cookie_expiry}{number of days to request browser to retain login cookie}
}
\value{
Shiny UI login panel with user name text input, password text input and login action button.
}
\description{
Shiny UI Module for use with \link{loginServer}
}
\examples{
library(shiny)

# dataframe that holds usernames, passwords and other user data
user_base <- dplyr::tibble(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
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
  # call login module supplying data frame, 
  # user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  output$user_table <- renderTable({
    # use req to only render results when credentials()$user_auth is TRUE
    req(credentials()$user_auth)
    credentials()$info
  })
}

if (interactive()) shinyApp(ui = ui, server = server)
}
