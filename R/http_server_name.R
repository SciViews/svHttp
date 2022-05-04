#' Get or change the name of the HTTP server
#'
#' @param name the name given to the SciViews server. By default, it is `R`.
#'
#' @return A character vector with the name of the HTTP server.
#' @export
#' @seealso [start_http_server()] for a complete example.
#' @keywords IO
#' @concept Interprocess communication
#' @examples
#' http_server_name()
http_server_name <- function(name) {
  if (!missing(name)) {
    if (!is.character(name))
      stop("'name' must be a string!")
    name <- as.character(name)[1]
    # This name is stored in the option R.id
    options(http.server.name = name)
    return(name)
  } else {# Get the server name
    name <- getOption("http.server.name")
    if (is.null(name)) name <- "R"
    return(name)
  }
}

# Old name of the function
#' @export
#' @rdname http_server_name
HttpServerName <- http_server_name
