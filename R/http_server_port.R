#' Get or change the port of the HTTP server
#'
#' @param port port on which the server should run (both help and SciViews). By
#' default, it is port 8888. Note that this server runs only locally and can
#' only serve requests from 127.0.0.1 (because communication is not crypted).
#'
#' @return A number with the port of the HTTP server.
#' @export
#' @seealso [start_http_server()] for a complete example.
#' @keywords IO
#' @concept Interprocess communication
#' @examples
#' http_server_port()
http_server_port <- function(port) {
  if (!missing(port)) {
    port <- as.integer(round(port[1]))
    # This port is stored in 'http.serve' option
    options(http.server.port = port)
    # If the server is running on another port, restart it now
    if (R.Version()$`svn rev` >= 67550) {
      curport <- tools::startDynamicHelp(NA)
    } else {
      curport <- getNamespace("tools")$httpdPort
    }
    if (curport > 0 && curport != port)
      start_http_server(port = port)
    return(port)
  } else {# Get the server port
    port <- getOption("http.server.port")
    if (is.null(port)) {
      port <- 8888
    } else {
      port <- as.integer(round(port[1]))
    }
    return(port)
  }
}

# Old name of the function
#' @export
#' @rdname http_server_port
HttpServerPort <- http_server_port
