library(shinytest)
library(shiny)

options(shiny.testmode = TRUE)

# Init shiny driver for testing
get_app <- function() {
  
  app <- ShinyDriver$new(system.file("shiny-examples", "shinyauthr_example",
                                     package = "shinyauthr"))
  
  return(app)
  
}


# Steps to authenthicate user
app_login <- function(app, role = "admin") {
  app$setInputs(`login-password` = if(role == 'admin') "pass1" else "pass2")
  app$setInputs(`login-user_name` = if(role == 'admin') "user1" else "pass2")
  app$setInputs(`login-button` = "click")
  
  Sys.sleep(1)  # wait for UI elements to load/update
}