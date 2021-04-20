library(shinytest)
library(testthat)
library(shiny)

options(shiny.testmode = TRUE)

# Init shiny driver for testing
get_app <- function() {
  app <- ShinyDriver$new(system.file("shiny-examples", "test_app", package = "shinyauthr"))
  return(app)
}

# Steps to authenthicate user
app_login <- function(app, role = "admin") {
  app$setInputs(`login-password` = if(role == 'admin') "pass1" else "pass2")
  app$setInputs(`login-user_name` = if(role == 'admin') "user1" else "pass2")
  app$setInputs(`login-button` = "click")
  Sys.sleep(1)  # wait for UI elements to load/update
} 

app_logout <- function(app) {
  app$setInputs(`logout-button` = "click")
  Sys.sleep(1)
} 

# load app in headless browser for use with shiny test
app <- get_app()
Sys.sleep(1)

test_that("UI changes accordingly with login and logout", {
  login_panel <- app$findElement(xpath = "//*[@id='login-panel']")
  logout_button <- app$findElement(xpath = "//button[@id='logout-button']")
  
  # check login panel is shown on start-up
  testthat::expect_equal(login_panel$getCssValue("display"), "block")
  # check logout button is hidden on start-up
  testthat::expect_equal(logout_button$getCssValue("display"), "none")
  
  # login
  app_login(app, role = "admin")
  
  # check login panel is hidden after login
  testthat::expect_equal(login_panel$getCssValue("display"), "none")
  # check logout button is shown after login
  testthat::expect_equal(logout_button$getCssValue("display"), "inline-block")
  
  # logout
  app_logout(app)

  # check login panel is shown again after logout
  testthat::expect_equal(login_panel$getCssValue("display"), "block")
  # check logout button is hidden again after logout
  testthat::expect_equal(logout_button$getCssValue("display"), "none")
  # check pwd input does not contain previously entered pwd after logout
  pwd_input <- app$findElement(xpath = "//input[@id='login-password']")
  testthat::expect_equal(pwd_input$getText(), "")
  
})

test_that("cookie login works after app refresh", {
  app_login(app, role = "admin")
  app$refresh()
  Sys.sleep(1)
  login_panel <- app$findElement(xpath = "//*[@id='login-panel']")
  logout_button <- app$findElement(xpath = "//button[@id='logout-button']")
  # check login panel is hidden after refresh + cookie login
  testthat::expect_equal(login_panel$getCssValue("display"), "none")
  # check logout button is shown after refresh + cookie login
  testthat::expect_equal(logout_button$getCssValue("display"), "inline-block")
  
  # values <- app$getAllValues()
  # expect_true(values$export$auth_status)
  # expect_s3_class(values$export$auth_info, "data.frame")
})

