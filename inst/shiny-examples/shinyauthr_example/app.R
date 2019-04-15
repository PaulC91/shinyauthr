library(shiny)
library(shinydashboard)
library(dplyr)
library(shinyjs)
library(glue)
library(shinyauthr)

user_base <- data_frame(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"), 
  password_hash = sapply(c("pass1", "pass2"), sodium::password_store), 
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

ui <- dashboardPage(
  
  dashboardHeader(title = "shinyauthr",
                  tags$li(class = "dropdown", style = "padding: 8px;",
                          shinyauthr::logoutUI("logout")),
                  tags$li(class = "dropdown", 
                          tags$a(icon("github"), 
                                 href = "https://github.com/paulc91/shinyauthr",
                                 title = "See the code on github"))
  ),
  
  dashboardSidebar(collapsed = TRUE, 
                   div(textOutput("welcome"), style = "padding: 20px")
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(tags$style(".table{margin: 0 auto;}"),
              tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                          type="text/javascript"),
              includeScript("returnClick.js")
    ),
    shinyauthr::loginUI("login"),
    uiOutput("user_table"),
    uiOutput("testUI"),
    HTML('<div data-iframe-height></div>')
  )
)

server <- function(input, output, session) {
  
  credentials <- callModule(shinyauthr::login, "login", 
                            data = user_base,
                            user_col = user,
                            pwd_col = password_hash,
                            sodium_hashed = TRUE,
                            log_out = reactive(logout_init()))
  
  logout_init <- callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth))
  
  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  output$user_table <- renderUI({
    # only show pre-login
    if(credentials()$user_auth) return(NULL)
    
    tagList(
      tags$p("test the different outputs from the sample logins below 
             as well as an invalid login attempt.", class = "text-center"),
      
      renderTable({user_base[, -3]})
    )
  })
  
  user_info <- reactive({credentials()$info})
  
  user_data <- reactive({
    req(credentials()$user_auth)
    
    if (user_info()$permissions == "admin") {
      dplyr::starwars[,1:10]
    } else if (user_info()$permissions == "standard") {
      dplyr::storms[,1:11]
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
                     Your data is: {ifelse(user_info()$permissions == 'admin', 'Starwars', 'Storms')}.")),
        box(width = NULL, status = "primary",
            title = ifelse(user_info()$permissions == 'admin', "Starwars Data", "Storms Data"),
            DT::renderDT(user_data(), options = list(scrollX = TRUE))
        )
      )
    )
  })
  
}

shiny::shinyApp(ui, server)
