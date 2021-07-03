library(testthat)
library(shiny)
library(shinytest)

# install dependencies on CI machines
if (!interactive()) {
  shinytest::installDependencies()
}

options(shiny.testmode = TRUE)

# Init shiny driver for testing
get_app <- function(app_name = "test_app") {
  app <- ShinyDriver$new(system.file("shiny-examples", app_name, package = "shinyauthr"))
  return(app)
}

# Steps to authenthicate user
app_login <- function(app, role = "admin") {
  app$setInputs(`login-password` = if(role == 'admin') "pass1" else "pass2")
  app$setInputs(`login-user_name` = if(role == 'admin') "user1" else "user2")
  app$setInputs(`login-button` = "click")
  Sys.sleep(1)  # wait for UI elements to load/update
}

app_logout <- function(app) {
  app$setInputs(`logout-button` = "click")
  Sys.sleep(1)
}

test_login_logout <- function(app, desc) {
  test_that(desc, {
    login_panel <- app$findElement(xpath = "//*[@id='login-panel']")
    logout_button <- app$findElement(xpath = "//button[@id='logout-button']")
    # check login panel is shown on start-up
    testthat::expect_equal(login_panel$getCssValue("display"), "block")
    # check logout button is hidden on start-up
    testthat::expect_equal(logout_button$getCssValue("display"), "none")
    # login
    app_login(app)
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
}

# load test app in headless browser for use with shiny test ======================
app <- get_app(app_name = "test_app")
Sys.sleep(1)

# test login and logout ===================
test_login_logout(app, "UI changes accordingly with login and logout")

# test cookie logins ===================
test_that("cookie login works after app refresh", {
  app_login(app, role = "user")
  app$refresh()
  Sys.sleep(1)
  login_panel <- app$findElement(xpath = "//*[@id='login-panel']")
  logout_button <- app$findElement(xpath = "//button[@id='logout-button']")
  # check login panel is hidden after refresh + cookie login
  testthat::expect_equal(login_panel$getCssValue("display"), "none")
  # check logout button is shown after refresh + cookie login
  testthat::expect_equal(logout_button$getCssValue("display"), "inline-block")
})

# test login and logout on now deprecated server functions ===================
app_deprecated <- get_app(app = "old_server_functions")
Sys.sleep(1)
test_login_logout(
  app_deprecated, 
  "UI changes accordingly with deprecated login and logout server functions"
)


