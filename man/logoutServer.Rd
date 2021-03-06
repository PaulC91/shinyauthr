% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logout.R
\name{logoutServer}
\alias{logoutServer}
\title{logout server module}
\usage{
logoutServer(id, active, ...)
}
\arguments{
\item{id}{An ID string that corresponds with the ID used to call the module's UI function}

\item{active}{\code{reactive} supply the returned \code{user_auth} boolean reactive from \link{loginServer}
here to hide/show the logout button}

\item{...}{arguments passed to \link[shinyjs]{toggle}}
}
\value{
Reactive boolean, to be supplied as the \code{log_out} argument of the
  \link{loginServer} module to trigger the logout process
}
\description{
Shiny authentication module for use with \link{logoutUI}
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
