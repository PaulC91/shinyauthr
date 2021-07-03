library(shiny)
library(shinydashboard)
library(dplyr)
library(glue)
library(shinyauthr)
library(RSQLite)
library(DBI)
library(lubridate)

# How many days should sessions last?
cookie_expiry <- 7

# This function must return a data.frame with columns user and sessionid.  Other columns are also okay
# and will be made available to the app after log in.

get_sessions_from_db <- function(conn = db, expiry = cookie_expiry) {
  dbReadTable(conn, "sessions") %>%
    mutate(login_time = ymd_hms(login_time)) %>%
    as_tibble() %>%
    filter(login_time > now() - days(expiry))
}

# This function must accept two parameters: user and sessionid. It will be called whenever the user
# successfully logs in with a password.

add_session_to_db <- function(user, sessionid, conn = db) {
  tibble(user = user, sessionid = sessionid, login_time = as.character(now())) %>%
    dbWriteTable(conn, "sessions", ., append = TRUE)
}

db <- dbConnect(SQLite(), ":memory:")
dbCreateTable(db, "sessions", c(user = "TEXT", sessionid = "TEXT", login_time = "TEXT"))

user_base <- tibble(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"),
  password_hash = sapply(c("pass1", "pass2"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

ui <- dashboardPage(
  dashboardHeader(
    title = "shinyauthr",
    tags$li(
      class = "dropdown",
      style = "padding: 8px;",
      shinyauthr::logoutUI("logout")
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        icon("github"),
        href = "https://github.com/paulc91/shinyauthr",
        title = "See the code on github"
      )
    )
  ),
  dashboardSidebar(
    collapsed = TRUE,
    div(textOutput("welcome"), style = "padding: 20px")
  ),
  dashboardBody(
    shinyauthr::loginUI(
      "login", 
      cookie_expiry = cookie_expiry, 
      additional_ui = tagList(
        tags$p("test the different outputs from the sample logins below
             as well as an invalid login attempt.", class = "text-center"),
        HTML(knitr::kable(user_base[, -3], format = "html", table.attr = "style='width:100%;'"))
      )
    ),
    uiOutput("testUI")
  )
)

server <- function(input, output, session) {
  
  # call login module supplying data frame, user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password_hash,
    sodium_hashed = TRUE,
    cookie_logins = TRUE,
    sessionid_col = sessionid,
    cookie_getter = get_sessions_from_db,
    cookie_setter = add_session_to_db,
    log_out = reactive(logout_init())
  )

  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )

  observe({
    if (credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })

  user_info <- reactive({
    credentials()$info
  })

  user_data <- reactive({
    req(credentials()$user_auth)

    if (user_info()$permissions == "admin") {
      dplyr::starwars[, 1:10]
    } else if (user_info()$permissions == "standard") {
      dplyr::storms[, 1:11]
    }
  })

  output$welcome <- renderText({
    req(credentials()$user_auth)

    glue("Welcome {user_info()$name}")
  })

  output$testUI <- renderUI({
    req(credentials()$user_auth)

    fluidRow(
      column(
        width = 12,
        tags$h2(glue("Your permission level is: {user_info()$permissions}.
                     You logged in at: {user_info()$login_time}.
                     Your data is: {ifelse(user_info()$permissions == 'admin', 'Starwars', 'Storms')}.")),
        box(
          width = NULL,
          status = "primary",
          title = ifelse(user_info()$permissions == "admin", "Starwars Data", "Storms Data"),
          DT::renderDT(user_data(), options = list(scrollX = TRUE))
        )
      )
    )
  })
}

shiny::shinyApp(ui, server)
