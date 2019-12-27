library(testthat)
library(shinytest)
library(dplyr)

app <- get_app()


## Login ---------------------
test_that("login form renders and sidebar is collapsed", {
  
  user_input <- app$findElement(xpath = "//input[@id='login-user_name']")
  pass_input <- app$findElement(xpath = "//input[@id='login-password']")
  sidebar <- app$getAllValues()$input$sidebarCollapsed
  
  testthat::expect_equal(user_input$getText(), "")
  testthat::expect_equal(pass_input$getText(), "")
  testthat::expect_true(sidebar)
  
})


test_that("user_auth and info get updated after successful login", {
  
  app_login(app, role = "admin")
  
  values <- app$getAllValues()
  testthat::expect_true(values$export$auth_status) 
  testthat::expect_s3_class(values$export$auth_info, "data.frame")
  
})


test_that("logout button and sidebar display after successful login", {
  
  app_login(app, role = "admin")
  
  logout_button <- app$findElement(xpath = "//button[@id='logout-button']")
  sidebar <- app$getAllValues()$input$sidebarCollapsed
  
  testthat::expect_false(sidebar)
  testthat::expect_equal(logout_button$getText(), "Log out")  # display logout button
  
})

# test_that("starwars dataset is shown to admins", {
#   
#   app_login(app, role = "admin")
#   values <- app$getAllValues()
#   
#   print(values$export$user_data)
#   
#   testthat::expect_identical(data, dplyr::starwars[, 1:10])
#   
# })

# test_that("storms is shown to standard accounts", {
#   
#   app_login(app, role = "standard")
#   values <- app$getAllValues()
#   
#   testthat::expect_identical(data, dplyr::storms[, 1:11])
#   
# })



## Logout ---------------------
test_that("user_auth and info get updated after successful logout", {

  # Authenticate and logout
  app_login(app, role = "admin")
  app$setInputs(`logout-button` = "click")
  Sys.sleep(1)
  
  values <- app$getAllValues()
  
  testthat::expect_false(values$export$auth_status)  # user_auth updates
  testthat::expect_null(values$export$auth_info)  # user_info updates
  
})


test_that("credentials are updated and logout button hides after successful logout", {
  
  # Authenticate and logout
  app_login(app, role = "admin")
  app$setInputs(`logout-button` = "click")
  Sys.sleep(1)
  
  sidebar <- app$getAllValues()$input$sidebarCollapsed
  logout_button <- app$findElement(xpath = "//button[@id='logout-button']")
  logout_display <- strsplit(logout_button$getAttribute("style"), ";")[[1]][2]
  
  testthat::expect_true(sidebar)
  testthat::expect_match(logout_display, "display: none")  # hide logout button
  
})
