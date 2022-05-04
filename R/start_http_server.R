#' (Re)start an HTTP server in R
#'
#' @description Turn the default R help HTTP server into a RJSONp SciViews
#' server (while still serving help pages, of course).
#'
#' @param port port on which the server should run (both help and SciViews). By
#' default, it is port 8888. Note that this server runs only locally and can
#' only serve requests from 127.0.0.1 (because communication is not crypted).
#' @param name the name given to the SciViews server. By default, it is `R`.
#'
#' @return An integer indicating the port used.
#' @export
#' @seealso [svSocket::startSocketServer()]
#' @keywords IO
#' @concept Interprocess communication
#' @examples
#' # Try to start the HTTP server on default port with default name
#' res <- try(start_http_server(), silent = TRUE)
#' if (!inherits(res, "try-error")) {
#'   # Get the port
#'   http_server_port()
#'
#'  # Get the name
#'  http_server_name()
#'
#'  # Get the list of clients... empty, unless you connect a client in between
#'  http_server_clients()
#'
#'  # Stop the server now
#'  stop_http_server()
#' }
start_http_server <- function(port = http_server_port(),
name = http_server_name()) {
  if (!is.character(name))
    stop("'name' must be a string!")
  name <- as.character(name)[1]

  # The port on which we want to run it
  if (!is.numeric(port[1]) || port[1] < 1)
    stop("'port' must be a positive integer!")
  port <- as.integer(round(port[1]))
  # The port on which the server currently runs
  if (R.Version()$`svn rev` >= 67550) {
    oports <- getOption("help.ports")
    (on.exit(options(help.ports = oports)))
    options(help.ports = port)
    curport <- tools::startDynamicHelp(NA)

    # Can we run the server?
    if (curport == -1L || nzchar(Sys.getenv("R_DISABLE_HTTPD")))
      stop("R http server is disabled or cannot start")

    # If curport is not the right one, try restarting
    if (curport != 0L) {
      if (curport != port)
        warning("R http server currently running on port ", curport,
          " and is restarted on port ", port, immediate. = TRUE)
      stop_http_server()
      curport <- tools::startDynamicHelp(TRUE)
    }

  } else {# Old code before startDynamicHelp(NA)
    curport <- getNamespace("tools")$httpdPort

    # Can we run the server?
    if (curport == -1L || nzchar(Sys.getenv("R_DISABLE_HTTPD")))
      stop("R http server is disabled or cannot start")

    # If it is currently running, stop it now
    if (curport != 0L) {
      if (curport != port)
        warning("R http server currently running on port ", curport,
          " and is restarted on port ", port, immediate. = TRUE)
      curport <- stop_http_server()
    }

    # Start the http server on the right port
    if (curport == 0L) {
      oports <- getOption("help.ports")
      (on.exit(options(help.ports = oports)))
      options(help.ports = port)
      curport <- tools::startDynamicHelp()
    } else {
      stop("Unable to start the http server")
    }
  }

  # Is the HTTP server running on the right port now?
  if (curport == port) {
    # Set the name of the HTTP server (for easier identification)
    http_server_name(name)

    # Install the SciViews function that will process our requests
    e <- getNamespace("tools")$.httpd.handlers.env
    e[["SciViews"]] <- function(path, query, body, ...) {
      # Analyze the query: command + callback
      #cat(query, "\n", sep = " -- ")
      msg <- query[1]

      ## Strings are supposed to be send in UTF-8 format
      #Encoding(msg) <- "UTF-8"
      #msg <- enc2native(msg)

      l <- length(query)
      if (l == 1) callback <- NULL else {
        callback <- query[l]
        #Encoding(callback) <- "UTF-8"
      }

      # Process the command in a similar way as process_socket() does
      # in the svSocket package... but return a RJSONp object if callback
      # is not NULL.
      # We use a custom function here to create this object faster than
      # by converting an R object to RJSON.
      Rjsonp <- function(res, callback) {
        # If no echo, return only a basic RJSONp object
        if (!return_results || is.null(res)) {
          obj <- paste(callback,
            '(list("result" := NA, ',
            '"options" := list("echo" := FALSE), "name" := "',
            server_name, '", "port" := ', server_port, '))', sep = "")
        } else {
          # Return a more consistent RJSONp object
          # Format main client options as a RJSON object
          options <- paste('list("echo" := ', pars$echo,
            ', "bare" := ', pars$bare,
            ', "partial" := ', (pars$code != ""), ')', sep = "")

          # Replace \n by \\n, etc. in res
          #res <- gsub("\n", "\\n", res, fixed = TRUE)
          res <- encodeString(res, quote = '"')

          # Check encoding and provide it if it is not UTF-8
          # No, provide it all the time!
          cs <- localeToCharset()[1]
          if (cs != "UTF-8") {
            encode <- paste(', "encoding" := "', cs, '"', sep = "")
          } else {
            encode <- ""
          }

          # Format the answer as a RJSONp object and return it
          obj <- paste(callback, '(list("result" := c(',
            paste(shQuote(res, type = "cmd"), collapse = ", "),
            '), "options" := ', options,
            ', "name" := "', server_name,
            '", "port" := ', server_port, encode, '))', sep = "")
          # Encode this string as UTF-8
          obj <- enc2utf8(obj)
        }
        #cat(obj, "\n")
        return(list(obj))
      }

      # The HTTP request message cannot be too long.
      # So, for submission of very long R code, this mechanism
      # is not appropriate. Here we use a specially formatted msg
      # indicating that we should read code from a file instead.
      if (regexpr("^SOURCE=", msg) > 0) {
        srcfile <- sub("^SOURCE=", "", msg)
        on.exit(try(unlink(srcfile), silent = TRUE))
        if (!file.exists(srcfile) || inherits(msg <-
            try(readLines(srcfile, warn = FALSE, encoding = "UTF-8"),
              silent = TRUE), "try-error")) {
          res <- paste(
            gettext("Error: missing or unreadable source file"),
            " '", srcfile, "'\n", sep = "")
          cat(res)
          if (is.null(callback)) {
            return(NULL)
          } else {
            return(Rjsonp(NULL, callback))
          }
        } else {
          msg <- paste(msg, collapse = "\n")
        }
      }

      # Get the server name and port, and R encoding
      server_name <- http_server_name()
      server_port <- http_server_port()

      # Do we receive an <<<id=myID>>> sequence (name of the client)?
      if (regexpr("^<<<id=[a-zA-Z0-9]+>>>", msg) > 0) {
        # Get the identifier
        client <- sub("^<<<id=([a-zA-Z0-9]+)>>>.*$", "\\1", msg)
        # ... and eliminate that sequence
        msg <- sub("^<<<id=[a-zA-Z0-9]+>>>", "", msg)
      } else {
        # The client name is simply 'default'
        client <- "default"
      }

      # Do we receive <<<esc>>>? => break (currently, only breaks
      # multiline mode)
      if (substr(msg, 1, 9) == "<<<esc>>>") {
        pars <- par_http_server(client, code = "")  # Reset multiline code
        msg <- substr(msg, 10, 1000000)
      }

      # Replace <<<n>>> by \n (for multiline code)
      msg <- gsub("<<<n>>>", "\n", msg)

      # Replace <<<s>>> by the corresponding client id and server port
      msg <- gsub("<<<s>>>", paste('"', client, '", ', server_port,
        sep = ""), msg)

      hidden_mode <- FALSE
      return_results <- TRUE
      # If msg starts with <<<Q>>> or <<<q>>>, then disconnect server
      # before or after evaluation of the command, respectively
      # Since we always disconnect AFTER with http server, these options
      # have no effect here. They are used with the socket server only
      # If msg starts with <<<e>>>, evaluate command in the console and
      # disconnect
      # If msg starts with <<<h>>> or <<<H>>>, evaluate in hidden mode
      # and disconnect
      start_msg <- substr(msg, 1, 7)
      if (start_msg == "<<<Q>>>") {
        msg <- substr(msg, 8, 1000000)
        return_results <- FALSE
      } else if (start_msg == "<<<q>>>") {
        msg <- substr(msg, 8, 1000000)
        par_http_server(client, last = "")
      } else if (start_msg == "<<<e>>>") {
        msg <- substr(msg, 8, 1000000)
        # We just configure the server correctly
        par_http_server(client, bare = FALSE, echo = TRUE, prompt = ":> ",
          continue = ":+ ", multiline = TRUE, last = "")
        # Add a command to the command history
        #timestamp("my R command", "", "", quiet = TRUE)
      } else if (start_msg == "<<<h>>>") {
        msg <- substr(msg, 8, 1000000)
        # Do not echo command on the server (silent execution)
        hidden_mode <- TRUE
        par_http_server(client, bare = TRUE, last = "")
      } else if (start_msg == "<<<H>>>") {
        msg <- substr(msg, 8, 1000000)
        # Do not echo command on the server
        hidden_mode <- TRUE
        return_results <- FALSE
        par_http_server(client, bare = TRUE)
      } else if (start_msg == "<<<u>>>") {
        msg <- substr(msg, 8, 1000000)
        # Silent execution, nothing is returned to the client
        # (but still echoed to the server)
        hidden_mode <- FALSE
        return_results <- FALSE
        par_http_server(client, bare = TRUE)
      }

      # Get parameters for the client
      pars <- par_http_server(client)
      if (bare <- pars$bare) {
        prompt <- ""
        continue <- ""
        echo <- FALSE
      } else {
        prompt <- pars$prompt
        continue <- pars$continue
        echo <- pars$echo
      }
      # TODO: do we still need this?
      # Eliminate last carriage return
      msg <- sub("(.*)[\n][^\n]*$", "\\1", msg)
      if (!hidden_mode) {
        if (isTRUE(echo)) {
          # Note: command lines are now echoed directly in capture_all()
          # => no need of this any more!
          if (pars$code == "") {
            pre <- prompt
          } else {
            pre <- continue
          }
          #cat(pre, msg, "\n", sep = "")
        }
        # Add previous content if we were in multiline mode
        if (pars$code != "")
          msg <- paste(pars$code, msg, sep = "\n")
        pars$code <- ""  # This changes the original data too!
      }

      # Parse the R code
      expr <- parse_text(msg)
      # Is it a wrong code?
      if (inherits(expr, "try-error")) {
        res <- paste(ngettext(1, "Error: ", "", domain = "R"),
          sub("^[^:]+: ([^\n]+)\n[0-9]+:(.*)$", "\\1\\2", expr), sep = "")
        if (isTRUE(echo))
          cat(res)
        if (is.null(callback)) {
          ret <- paste(res, pars$last, prompt, sep = "")
          # Encode this as UTF-8
          return(enc2utf8(ret))
        } else {
          return(Rjsonp(paste(res, pars$last, prompt, sep = ""), callback))
        }
      }
      # Is it incomplete code?
      if (!is.expression(expr)) {
        # Is multiline mode allowed?
        if (!bare && pars$multiline) {
          pars$code <- msg
          if (is.null(callback)) {
            if (return_results) {
              ret <- paste(pars$last, continue, sep = "")
              # Encode this as UTF-8
              return(enc2utf8(ret))
            } else {
              return(NULL)
            }
          } else {
            if (return_results) {
              return(Rjsonp(paste(pars$last, continue, sep = ""), callback))
            } else {
              return(Rjsonp(NULL, callback))
            }
          }
        } else {# Multimode not allowed
          res <- paste(
            gettext("Error: incomplete command in single line mode"),
            "\n", sep = "")
          if (isTRUE(echo))
            cat(res)
          if (is.null(callback)) {
            if (return_results) {
              ret <- paste(res, pars$last, prompt, sep = "")
              # Encode this as UTF-8
              return(enc2utf8(ret))
            } else {
              return(NULL)
            }
          } else {
            if (return_results) {
              return(Rjsonp(paste(res, pars$last, prompt, sep = ""), callback))
            } else {
              return(Rjsonp(NULL, callback))
            }
          }
        }
      }
      # Freeze parameters (unlinks from the environment)
      pars <- as.list(pars)
      # Is it something to evaluate?
      if (length(expr) < 1) {
        if (is.null(callback)) {
          ret <- paste(pars$last, prompt, sep = "")
          # Encode this as UTF-8
          return(enc2utf8(ret))
        } else {
          return(Rjsonp(paste(pars$last, prompt, sep = ""), callback))
        }
      }
      # Correct code,... we evaluate it
      results <- capture_all(expr, echo = echo, split = echo)
      # Should we run taskCallbacks?
      # Note: these are installed in svKomodo package
      if (!hidden_mode) {
        h <- get_temp(".svTaskCallbackManager", default = NULL, mode = "list")
        if (!is.null(h))
          h$evaluate()
      }
      # Collapse and add last and the prompt at the end
      results <- paste(results, collapse = "\n")
      #if (Echo) cat(results)
      if (!return_results) {
        if (is.null(callback)) {
          return(NULL)
        } else {
          return(Rjsonp(NULL, callback))
        }
      }
      prompt <- if (pars$bare) "" else pars$prompt
      results <- paste(results, pars$last, prompt, sep = "")
      # Return the results in plain text, or RJSONp object
      if (is.null(callback)) {
        return(enc2utf8(results))
      } else {
        return(Rjsonp(results, callback))
      }
    }
  }
  return(invisible(curport))
}

# Old name of the function
#' @export
#' @rdname start_http_server
startHttpServer <- start_http_server
