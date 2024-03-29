% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/login.R
\name{loginServer}
\alias{loginServer}
\title{login server module}
\usage{
loginServer(
  id,
  data,
  user_col,
  pwd_col,
  sodium_hashed = FALSE,
  log_out = shiny::reactiveVal(),
  reload_on_logout = FALSE,
  cookie_logins = FALSE,
  sessionid_col,
  cookie_getter,
  cookie_setter
)
}
\arguments{
\item{id}{An ID string that corresponds with the ID used to call the module's UI function}

\item{data}{data frame or tibble containing user names, passwords and other user data. Can be either
a static object or a shiny \link[shiny]{reactive} object}

\item{user_col}{bare (unquoted) or quoted column name containing user names}

\item{pwd_col}{bare (unquoted) or quoted column name containing passwords}

\item{sodium_hashed}{have the passwords been hash encrypted using the sodium package? defaults to FALSE}

\item{log_out}{[reactive] supply the returned reactive from \link{logoutServer} here to trigger a user logout}

\item{reload_on_logout}{should app force a session reload on logout?}

\item{cookie_logins}{enable automatic logins via browser cookies?}

\item{sessionid_col}{bare (unquoted) or quoted column name containing session ids}

\item{cookie_getter}{a function that returns a data.frame with at least two columns: user and session}

\item{cookie_setter}{a function with two parameters: user and session.  The function must save these to a database.}
}
\value{
The module will return a reactive 2 element list to your main application.
  First element \code{user_auth} is a boolean indicating whether there has been
  a successful login or not. Second element \code{info} will be the data frame provided
  to the function, filtered to the row matching the successfully logged in username.
  When \code{user_auth} is FALSE \code{info} is NULL.
}
\description{
Shiny authentication module for use with \link{loginUI}
}
\details{
This module uses shiny's new \link[shiny]{moduleServer} method as opposed to the \link[shiny]{callModule}
method used by the now deprecated \link{login} function and must be called differently in your app.
For details on how to migrate see the 'Migrating from callModule to moduleServer' section of 
\href{https://shiny.rstudio.com/articles/modules.html}{Modularizing Shiny app code}.
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
