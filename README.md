# shinyauthr

<!-- badges: start -->
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

`shinyauthr` is an R package providing module functions that can be used to add an authentication layer to your shiny apps.

It borrows some code from treysp's [shiny_password](https://github.com/treysp/shiny_password) template with the goal of making implementation simpler for end users and allowing the login/logout UIs to fit easily into any UI framework, including [shinydashboard](https://rstudio.github.io/shinydashboard/). 

To enable cookie-based authentication in browsers, it also borrows code from calligross's [Shiny Cookie Based Authentication Example](https://gist.github.com/calligross/e779281b500eb93ee9e42e4d72448189) and from an earlier PR from [aqualogy](https://github.com/aqualogy/shinyauthr).

## Installation

```r
remotes::install_github("paulc91/shinyauthr")
```

## Run example app

```r
shinyauthr::runShinyExample()
```

See full code in the [inst directory](inst/shiny-examples/shinyauthr_example).

## Usage

The package provides 2 module functions each with a UI and server element:

- `login()`
- `loginUI()`
- `logout()`
- `logoutUI()`

Below is a minimal reproducible example of how to use the authentication modules in a shiny app. Note that this package invisibly calls `shinyjs::useShinyjs()` and there is no need for you to do so (although there is no harm if you do).

```r
library(shiny)

# dataframe that holds usernames, passwords and other user data
user_base <- data.frame(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"), 
  permissions = c("admin", "standard"),
  name = c("User One", "User Two"),
  stringsAsFactors = FALSE,
  row.names = NULL
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
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- callModule(shinyauthr::logout, 
                            id = "logout", 
                            active = reactive(credentials()$user_auth))
  
  # call login module supplying data frame, user and password cols
  # and reactive trigger
  credentials <- callModule(shinyauthr::login, 
                            id = "login", 
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

## Cookie-Based Authentication

To enable cookie-based automatic login for returning users, you must create your own functions to save and load session info in a database.

The first required function must accept two parameters `user` and `session`.  The first of these is the user name for log in.  The second is a randomly generated string that identifies the session.  The app asks the user's web browser to save this session id as a cookie.

The second required function is called without parameters and must return a data.frame of valid `user` and `session` ids.  If the user's web browser sends your app a cookie which appears in the `session` column, then the corresponding `user` is automatically logged in.

Pass these functions to the login module via `callModule(shinyauthr::login, ...)` as the `cookie_setter` and `cookie_getter` parameters.  A minimal example is below.

```r
library(shiny)
library(dplyr)
library(lubridate)
library(DBI)
library(RSQLite)

cookie_expiry <- 7 # Days until session expires

# This function must accept two parameters: user and sessionid. It will be called whenever the user
# successfully logs in with a password.  This function saves to your database.

add_sessionid_to_db <- function(user, sessionid, conn = db){
	tibble(user = user, sessionid = sessionid, login_time = as.character(now())) %>%
		dbWriteTable(conn, "sessionids", ., append = TRUE)
}

# This function must return a data.frame with columns user and sessionid  Other columns are also okay
# and will be made available to the app after log in as columns in credentials()$user_auth

get_sessionids_from_db <- function(conn = db, expiry = cookie_expiry){
	dbReadTable(conn, "sessionids") %>%
		mutate(login_time = ymd_hms(login_time)) %>%
		as_tibble() %>%
		filter(login_time > now() - days(expiry))
}

if(file.exists("my_db_file")){
	db <- dbConnect(SQLite(), "my_db_file")
} else{
	db <- dbConnect(SQLite(), "my_db_file")
	dbCreateTable(db, "sessionids", c(user = "TEXT", sessionid = "TEXT", login_time = "TEXT"))
}

# dataframe that holds usernames, passwords and other user data
user_base <- data.frame(
	user = c("user1", "user2"),
	password = c("pass1", "pass2"),
	permissions = c("admin", "standard"),
	name = c("User One", "User Two"),
	stringsAsFactors = FALSE,
	row.names = NULL
)

ui <- fluidPage(
	# add logout button UI
	div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
	# add login panel UI function
	shinyauthr::loginUI(id = "login", cookie_expiry = cookie_expiry),
	# setup table output to show user info after login
	tableOutput("user_table")
)

server <- function(input, output, session) {

	# call the logout module with reactive trigger to hide/show
	logout_init <- callModule(shinyauthr::logout,
							  id = "logout",
							  active = reactive(credentials()$user_auth))

	# call login module supplying data frame, user and password cols
	# and reactive trigger
	credentials <- callModule(shinyauthr::login,
							  id = "login",
							  data = user_base,
							  user_col = user,
							  pwd_col = password,
							  sessionid_col = sessionid,
							  cookie_getter = get_sessionids_from_db,
							  cookie_setter = add_sessionid_to_db,
							  log_out = reactive(logout_init()))

	# pulls out the user information returned from login module
	user_data <- reactive({credentials()$info})

	output$user_table <- renderTable({
		# use req to only render results when credentials()$user_auth is TRUE
		req(credentials()$user_auth)
		user_data() %>%
			mutate(across(starts_with("login_time"), as.character))
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

## Hashing Passwords with `sodium`

If you are hosting your user passwords on the internet, it is a good idea to first encrypt them with a hashing algorithm. You can use the [sodium package](https://github.com/jeroen/sodium) to do this. Sodium uses a slow hashing algorithm that is specifically designed to protect stored passwords from brute-force attacks. More on this [here](https://download.libsodium.org/doc/password_hashing/). You then tell the `shinyauthr::login` module that your passwords have been hashed by `sodium` and `shinyauthr` will then decrypt when login is requested. Your plain text passwords must be a character vector, not factors, when hashing for this to work as shiny inputs are passed as character strings.

For example, a sample user base like the following can be incorporated for use with `shinyauthr`:

```r
# create a user base then hash passwords with sodium
# then save to an rds file in app directory
library(sodium)

user_base <- data.frame(
  user = c("user1", "user2"),
  password = sapply(c("pass1", "pass2"), sodium::password_store), 
  permissions = c("admin", "standard"),
  name = c("User One", "User Two"),
  stringsAsFactors = FALSE,
  row.names = NULL
)

saveRDS(user_base, "user_base.rds")
```
```r
# in your app code, read in the user base rds file
user_base <- readRDS("user_base.rds")
```
```r
# then when calling the module set sodium_hashed = TRUE
credentials <- callModule(shinyauthr::login, "login", 
                          data = user_base,
                          user_col = user,
                          pwd_col = password,
                          sodium_hashed = TRUE,
                          log_out = reactive(logout_init()))
```

## Disclaimer

I'm not a security professional so cannot guarantee this authentication procedure to be foolproof. It is ultimately the shiny app developer's responsibility not to expose any sensitive content to the client without the necessary login criteria being met.

I would welcome any feedback on any potential vulnerabilities in the process. I know that apps hosted on a server without an SSL certificate could be open to interception of usernames and passwords submitted by a user. As such I would not recommend the use of shinyauthr without an HTTPS connection.

For apps intended for use within commercial organisations, I would recommend one of RStudio's commercial shiny hosting options, or [shinyproxy](https://www.shinyproxy.io/), both of which have built in authetication options.

However, I hope that having an easy-to-implement open-source shiny authentication option like this will prove useful when alternative options are not feasible.

_Paul Campbell_


