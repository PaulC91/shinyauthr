# shinyauthr

`shinyauthr` is an R package providing module functions that can be used to add an authentication layer to your shiny apps.

It borrows some code from treysp's [shiny_password](https://github.com/treysp/shiny_password) template with the goal of making implementation simpler for end users and allowing the login/logout UIs to fit easily into any UI framework, including [shinydashboard](https://rstudio.github.io/shinydashboard/). See [live example app here](https://cultureofinsight.shinyapps.io/shinyauthr/) and code in the [inst directory](inst/shiny-examples/shinyauthr_example).
 
## Installation

```r
devtools::install_github("paulc91/shinyauthr")
```
## Usage

The package provides 2 module functions each with a UI and server element:

- `login`
- `loginUI`
- `logout`
- `logoutUI`

Below is a minimal reproducible example of how to use the authentication modules in a shiny app. Note that you must initiate the use of the shinyjs package with `shinyjs::useShinyjs()` in your UI code for this to work appropriately.

```r
library(shiny)
library(shinyauthr)

# dataframe that holds usernames, passwords and other user data
user_base <- data.frame(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"), 
  permissions = c("admin", "standard"),
  name = c("User One", "User Two"),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  # must turn shinyjs on
  shinyjs::useShinyjs(),
  # add login panel UI function
  shinyauthr::loginUI("login"),
  # add logout button UI 
  div(class = "pull-right", shinyauthr::logoutUI("logout")),
  # setup table output to show user info after login
  tableOutput("user_table")
)

server <- function(input, output, session) {
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- callModule(shinyauthr::logout, "logout", 
                            reactive(credentials()$user_auth))
  
  # call login module supplying data frame, user and password cols
  # and reactive trigger
  credentials <- callModule(shinyauthr::login, "login", 
                            data = user_base,
                            user_col = user,
                            pwd_col = password,
                            log_out = reactive(logout_init()))
  
  # pulls out the user information returned from login module
  user_data <- reactive({credentials()$info})
  
  output$user_table <- renderTable({
    # use req to only render results when credentials()$user_auth is TRUE
    req(credentials()$user_auth)
    user_data()
  })
}

shinyApp(ui = ui, server = server)

```
## Details

When the login module is called, it returns a reactive list containing 2 elements:

- `user_auth`
- `info`

The initial values of these variables are `FALSE` and `NULL` respectively. However,
given a data frame or tibble containing user names, passwords and other user data (optional), the login module will assign a `user_auth` value of `TRUE` if the user supplies a matching user name and password. The value of `info` then becomes the row of data associated with that user which can be used in the main to control content based on user permission variables etc.

The logout button will only show when `user_auth` is `TRUE`. Clicking the button will reset `user_auth` back to `FALSE` which will hide the button and show the login panel again.

You can set the code in your server functions to only run after a successful login through use of the `req()` function inside all reactives, renders and observers. In the example above, using `req(credentials()$user_auth)` inside the `renderTable` function ensures the table showing the returned user information is only rendered when `user_auth` is `TRUE`.

## Hashing Passwords with `digest`

If you are hosting your user passwords on the internet, it is a good idea to first encrypt them with a hashing algorithm. You can use the [digest package](https://github.com/eddelbuettel/digests) to do this. You can then tell the `shinyauthr::login` module that your passwords are hashed and what algorithm you used when hashing with digest. Your plain text passwords must be a character vector, not factors, when hashing for this to work as shiny inputs are passed as character strings.

For example, a sample user base like the following can be incorporated for use with `shinyauthr`:

```r
# create a user base then hash passwords with md5 algorithm
# then save to an rds file in app directory
user_base <- data.frame(
  user = c("user1", "user2"),
  password = sapply(c("pass1", "pass2"), digest, "md5"), 
  permissions = c("admin", "standard"),
  name = c("User One", "User Two"),
  stringsAsFactors = FALSE
)

saveRDS(user_base, "user_base.rds")
```
```r
# in your app code, read in the user base rds file
user_base <- readRDS("user_base.rds")
```
```r
# then when calling the module, set hashed = TRUE and algo = "md5"
credentials <- callModule(shinyauthr::login, "login", 
                          data = user_base,
                          user_col = user,
                          pwd_col = password,
                          hashed = TRUE,
                          algo = "md5",
                          log_out = reactive(logout_init()))
```

## Disclaimer

I'm not a security professional so cannot guarantee this authentication procedure to be foolproof. It is ultimately the shiny app developer's responsibility not to expose any sensitive content to the client without the necessary login criteria being met.

I would welcome any feedback on any potential vulnerabilities in the process. I know that apps hosted on a server without an SSL certificate could be open to interception of usernames and passwords submitted by a user. As such I would not recommend the use of shinyauthr without an HTTPS connection.

For apps intended for use within commercial organisations, I would recommend one of RStudio's commercial shiny hosting options with built in authetication.

However, I hope that having an easy-to-implement open-source shiny authentication option like this will prove useful when alternative options are not feasible.

_Paul Campbell_

_October 2018_


