% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logout.R
\name{logout}
\alias{logout}
\title{logout server module (deprecated)}
\arguments{
\item{input}{shiny input}

\item{output}{shiny output}

\item{session}{shiny session}

\item{active}{[reactive] supply the returned \code{user_auth} boolean reactive from \link{login}
here to hide/show the logout button}
}
\value{
Reactive boolean, to be supplied as the \code{log_out} argument of the
  \link{login} module to trigger the logout process
}
\description{
Deprecated. Use \link{logoutServer} instead.
}
\details{
Shiny authentication module for use with \link{logoutUI}

Call via \code{shiny::callModule(shinyauthr::logout, "id", ...)}

This function is now deprecated in favour of \link{logoutServer} which uses shiny's new \link[shiny]{moduleServer} 
method as opposed to the \link[shiny]{callModule} method used by this function. 
See the \link{logoutServer} documentation For details on how to migrate.
}
\examples{
\dontrun{
logout_init <- shiny::callModule(
  logout,
  id = "logout",
  active = reactive(user_credentials()$user_auth)
)
}

}
