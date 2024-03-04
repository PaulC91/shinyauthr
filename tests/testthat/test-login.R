library(testthat)
library(shiny)
library(shinyauthr)

user_base <- dplyr::tibble(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

test_that("user_auth is FALSE and info is NULL by default", {
  logout <- reactiveVal()
  testServer(
    loginServer, 
    args = list(
      data = user_base,
      user_col = "user",
      pwd_col = "password",
      log_out = logout
    ), {
      # module return value
      credentials <- session$getReturned()
      expect_false(credentials()$user_auth)
      expect_null(credentials()$info)
    }) 
})

test_that("user1 login and logout", {
  logout <- reactiveVal()
  testServer(
    loginServer, 
    args = list(
      data = user_base,
      user_col = "user",
      pwd_col = "password",
      log_out = logout,
      cookie_logins = FALSE
    ), {
      # module return value
      credentials <- session$getReturned()
      
      # login as user 1
      session$setInputs(user_name = "user1")
      session$setInputs(password = "pass1")
      session$setInputs(button = 1)
      session$elapse(1000)
      
      expect_true(session$returned()$user_auth)
      expect_equal(session$returned()$info$user, "user1")
      expect_equal(session$returned()$info$name, "User One")
      expect_equal(session$returned()$info$permissions, "admin")

      # logout
      logout(1)
      session$flushReact()
      session$elapse(1000)
      expect_false(session$returned()$user_auth)
      expect_null(session$returned()$info)
    })
})

test_that("user2 login works", {
  logout <- reactiveVal()
  testServer(
    loginServer, 
    args = list(
      data = user_base,
      user_col = "user",
      pwd_col = "password",
      log_out = logout
    ), {
      # module return value
      credentials <- session$getReturned()
      
      # login as user 2
      session$setInputs(
        user_name = "user2",
        password = "pass2",
        button = 1
      )
      expect_true(credentials()$user_auth)
      expect_equal(credentials()$info$user, "user2")
      expect_equal(credentials()$info$name, "User Two")
      expect_equal(credentials()$info$permissions, "standard")
      
      # logout
      logout(1)
      session$flushReact()
      expect_false(credentials()$user_auth)
      expect_null(credentials()$info)
    }) 
})

test_that("incorrect credentials does not log in", {
  logout <- reactiveVal()
  testServer(
    loginServer, 
    args = list(
      data = user_base,
      user_col = "user",
      pwd_col = "password",
      log_out = logout
    ), {
      # module return value
      credentials <- session$getReturned()
      # test incorrect credentials
      session$setInputs(
        user_name = "user1",
        password = "wrong_pwd",
        button = 1
      )
      expect_false(credentials()$user_auth)
      expect_null(credentials()$info)
    }) 
})

test_that("sodium decrypts password", {
  skip_if_not_installed("sodium")
  user_base <- dplyr::tibble(
    user = "user1",
    password = sodium::password_store("pass1")
  )
  logout <- reactiveVal()
  testServer(
    loginServer, 
    args = list(
      data = user_base,
      user_col = "user",
      pwd_col = "password",
      sodium_hashed = TRUE,
      log_out = logout
    ), {
      # module return value
      credentials <- session$getReturned()
      
      # login as user 1
      session$setInputs(
        user_name = "user1",
        password = "pass1",
        button = 1
      )
      
      expect_true(credentials()$user_auth)
    }) 
})

