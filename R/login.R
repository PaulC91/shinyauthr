#' login UI module
#'
#' Shiny UI Module for use with \link{login}
#' 
#' Call via \code{loginUI("your_id")}
#'
#' @param id Shiny id
#'
#' @return Shiny UI
#'
#' @author Paul Campbell, \email{pacampbell91@gmail.com}
#'
#' @export
loginUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(id = ns("panel"), style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
      shiny::wellPanel(
        shiny::tags$h2("Please log in", class = "text-center", style = "padding-top: 0;"),

        shiny::textInput(ns("user_name"), shiny::tagList(shiny::icon("user"), "User Name")),

        shiny::passwordInput(ns("password"), shiny::tagList(shiny::icon("unlock-alt"), "Password")),

        shiny::div(
          style = "text-align: center;",
          shiny::actionButton(ns("button"), "Log in", class = "btn-primary", style = "color: white;")
        ),

        shinyjs::hidden(
          shiny::div(id = ns("error"),
                     shiny::tags$p("Invalid username or password!",
                     style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center"))
        )
      )
  )
}

#' login server module
#'
#' Shiny authentication module for use with \link{loginUI}
#'
#' Call via \code{shiny::callModule(login, "your_id", ...)}
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param data data frame or tibble containing usernames, passwords and other user data
#' @param user_col bare (unquoted) column name containing usernames
#' @param pwd_col bare (unquoted) column name containing passwords
#' @param hashed have the passwords been hash encrypted using the digest package? defaults to FALSE
#' @param algo if passwords are hashed, what hashing algorithm was used? options are c("md5", "sha1", "crc32", "sha256", "sha512", "xxhash32", "xxhash64", "murmur32")
#' @param log_out [reactive] supply the returned reactive from \link{logout} here to trigger a user logout
#'
#' @return The module will return a reactive 2 element list to your main application. 
#'   First element \code{user_auth} is a boolean inditcating whether there has been
#'   a successful login or not. Second element \code{info} will be the data frame provided
#'   to the function, filtered to the row matching the succesfully logged in username. 
#'   When \code{user_auth} is FALSE \code{info} is NULL.
#'
#' @author Paul Campbell, \email{pacampbell91@gmail.com}
#' 
#' @importFrom rlang :=
#' 
#' @examples
#' \dontrun{
#'   user_credentials <- shiny::callModule(login, "login", 
#'                                         data = user_base,
#'                                         user_col = user,
#'                                         pwd_col = password,
#'                                         log_out = reactive(logout_init()))
#' }
#'
#' @export
login <- function(input, output, session, data, user_col, pwd_col,
                  hashed = FALSE, algo = NULL, log_out = NULL) {

  credentials <- shiny::reactiveValues(user_auth = FALSE, info = NULL)

  shiny::observeEvent(log_out(), {
    credentials$user_auth <- FALSE
    credentials$info <- NULL
  })

  shiny::observeEvent(credentials$user_auth, ignoreInit = TRUE, {
    shinyjs::toggle(id = "panel")
  })

  users <- dplyr::enquo(user_col)
  pwds <- dplyr::enquo(pwd_col)
  
  data <- dplyr::mutate_if(data, is.factor, as.character)

  shiny::observeEvent(input$button, {

    if(hashed) {
      
      if(is.null(algo)) stop("Please provide a hash algorithm argument to the login module if passwords are hashed")
      
      if(!algo %in% c("md5", "sha1", "crc32", "sha256", "sha512", "xxhash32", "xxhash64", "murmur32")) {
        stop('Your hash algorithm name is not valid. Please select from:\n"md5", "sha1", "crc32", "sha256", "sha512", "xxhash32", "xxhash64", "murmur32"')
      }
      
      # check for match of hashed input password to hashed password column in data
      row_password <- which(dplyr::pull(data, !! pwds) == digest::digest(input$password, algo = algo))
      
    } else {
      # if passwords are not hashed, hash them with md5 and do the same with the input password
      # first ensure passwords are character class
      if(!is.character(dplyr::pull(data, !! pwds))) {
        data <- dplyr::mutate(data,  !! pwds := as.character(!! pwds))
      }
      
      data <- dplyr::mutate(data,  !! pwds := sapply(!! pwds, digest::digest))
      row_password <- which(dplyr::pull(data, !! pwds) == digest::digest(input$password))
    }

    # check for match of input username to username column in data
    row_username <- which(dplyr::pull(data, !! users) == input$user_name)
    
    # if user name row and password name row are same, credentials are valid
    if (length(row_username) == 1 &&
        length(row_password) >= 1 &&  # more than one user may have same pw
        (row_username %in% row_password)) {
      credentials$user_auth <- TRUE
      credentials$info <- dplyr::filter(data, !! users == input$user_name)
    } else { # if not valid temporarily show error message to user
      shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade")
      shinyjs::delay(5000, shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade"))
    }

  })

  # return reactive list containing auth boolean and user information
  shiny::reactive({
    shiny::reactiveValuesToList(credentials)
  })

}
