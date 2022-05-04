#' Get or change http server options
#'
#' @param client the name of one client. A client that does not identify
#' itself is named `default`.
#' @param ... named arguments specifying options to set or change.
#'
#' @return An environment that contains the whole configuration is returned
#' invisibly.
#' @note
#' Possible named arguments (with their default values) are: `prompt = ":> "`
#' for the server prompt, `continue = ":+ "` for the continuation prompt when
#' multiline instructions are send, `code = ""` for current partial code in
#' multiline mode, `last = ""` for a string to add at the end of each
#' evaluation, `echo = FALSE` to echo commands at the R console or terminal,
#' `multiline = TRUE` to allow multiline mode, `bare = TRUE` a bare mode that
#' inactivates all other options (the server is always started in bare mode).
#' @export
#' @seealso [start_http_server()] for a complete example.
#' @keywords IO
#' @concept Interprocess communication
par_http_server <- function(client, ...) {
  if (missing(client)) {
    client <- "default"
  } else {
    client <- as.character(client)[1]
  }

  # Set or get parameters for a given HTTP client
  server_port <- http_server_port()

  # No attempt is made to make sure this client exists
  sc <- paste("http_client", client, sep = "_")
  if (!exists(sc, envir = temp_env(), inherits = FALSE,
    mode = "environment")) {
    # Create a new environment with default values
    e <- new.env(parent = temp_env())
    e$client <- client
    e$server_port <- server_port
    e$prompt <- ":> "    # Default prompt
    e$continue <- ":+ "  # Default continuation prompt
    e$code <- ""         # Current partial code for multiline mode
    e$last <- ""         # String to add at the end of evaluations
    e$echo <- FALSE      # Don't echo commands to the console
    e$flag <- FALSE      # Do not flag pieces of code (not used yet!)
    e$multiline <- TRUE  # Allow for multiline code
    e$bare <- TRUE       # Always start in "bare" mode
    # Note: in bare mode, all other parameters are inactive!
    # and assign it to SciViews:TempEnv
    assign(sc, e, envir = temp_env())
  } else {
    e <- get(sc, envir = temp_env(), mode = "environment")
  }

  # Change or add parameters if they are provided
  args <- list(...)
  if (l <- length(args)) {
    change_par <- function(x, val, env) {
      if (is.null(x))
        return(FALSE)  # Do nothing without a valid name
      if (is.null(val)) {
        suppressWarnings(rm(list = x, envir = env))  # Remove it
        return(TRUE)
      }
      env[[x]] <- val  # Add or change this variable in the environment
      return(TRUE)
    }
    n <- names(args)
    res <- rep(TRUE, l)
    for (i in seq_len(l)) res[i] <- change_par(n[i], args[[i]], e)
    if (any(!res))
      warning("Non named arguments are ignored")
  }

  # If server_port has changed, update it now
  if (e$server_port != server_port)
    e$server_port <- server_port

  return(invisible(e))
}

# Old name of the function
#' @export
#' @rdname par_http_server
parHttp <- par_http_server
