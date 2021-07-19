#' Generate random string
#'
#' This function is used to generate random session ids.
#'
#' @param n length of string
#'
#' @return A random character string.
#' @noRd
randomString <- function(n = 64) {
  paste(
    sample(x = c(letters, LETTERS, 0:9), size = n, replace = TRUE),
    collapse = ""
  )
}

#' Place Holder Database Cookie Handlers
#'
#' These functions silently reject all cookies during login and ignore requests to save session
#' ids to a database.  Use these if you don't want to allow automatic login with cookies.  These are
#' default values for `callModule(shinyauthr::login, ...)`.  Replace them with your own functions which
#' get and set the user and session ids in a database.
#'
#' @name cookie_placeholders
#' @noRd
#'
#' @param user character string representing the user id
#' @param session  character string representing the session id
#' @return `default_cookie_setter` returns a data.frame with zero rows and two columns: `user` and `session`

NULL

#' @rdname cookie_placeholders
#' @noRd
default_cookie_getter <- function(user_string, session_string) {
  df <- data.frame(user = character(0), session = character(0))
  names(df) <- c(user_string, session_string)
  function() {
    df
  }
}


#' @rdname cookie_placeholders
#' @noRd
default_cookie_setter <- function(user, session) {
  invisible(NULL)
}

#' Generate required Javascript
#'
#' This is used to generate the Javascript which gets and sets the cookies in the
#' user's browser.  We need to glue it together to handle the variable expiry time and
#' to get the correct module namespace.  This function is used internally only.
#'
#' @param id name of input object to hold the cookie value
#' @param expire_days number of days to ask browser to retain cookie
#'
#' @return Character string of Javascript code.
#' @noRd
#'
#' @examples
#' \dontrun{
#' shinyjs::extendShinyjs(
#'   text = js_cookie_to_r_code(ns("jscookie")),
#'   functions = c("getcookie", "setcookie", "rmcookie")
#' )
#' }
js_cookie_to_r_code <- function(id, expire_days = 7) {
  glue::glue(.open = "{{{", .close = "}}}", '
	  shinyjs.getcookie = function(params) {
	    var cookie = Cookies.get("shinyauthr");
	    if (typeof cookie !== "undefined") {
	      Shiny.setInputValue("{{{id}}}", cookie);
	    } else {
	      var cookie = "";
	      Shiny.setInputValue("{{{id}}}", cookie);
	    }
	  }
	  shinyjs.setcookie = function(params) {
	    Cookies.set("shinyauthr", escape(params), { expires: {{{expire_days}}} });
	    Shiny.setInputValue("{{{id}}}", params);
	  }
	  shinyjs.rmcookie = function(params) {
	    Cookies.remove("shinyauthr");
	    Shiny.setInputValue("{{{id}}}", "");
	  }')
}

#' Javascript code to trigger login button with return key
#'
#' @param idpassword name of password input, with correct namespace
#' @param idbutton name of action button, with correct namespace
#' @return Character string of Javascript code.
#' @noRd
js_return_click <- function(idpassword, idbutton) {
  glue::glue(.open = "{{{", .close = "}}}", '
	  $(document).keyup(function(event) {
    if ($("#{{{idpassword}}}").is(":focus") && (event.keyCode == 13)) {
        $("#{{{idbutton}}}").click();
    }
    });')
}

#' Javascript code for managing cookies
#'
#' This code is minified from the GitHub project js-cookie/js-cookie.  We include it here
#' to make the shinyauthr package more self-contained
#' @noRd
jscookie_script <- function() {
  shiny::includeScript(system.file("js-cookie/js-cookie.js", package = "shinyauthr"))
}
