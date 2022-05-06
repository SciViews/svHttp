#' Get list of all names of clients that already connected to the http server
#'
#' @return A character vector with the name of currently connected clients.
#' @export
#' @seealso [start_http_server()] for a complete example.
#' @keywords IO
#' @concept Interprocess communication
#' @examples
#' library(svHttp)
#' http_server_clients()
http_server_clients <- function() {
  sub("^http_client_", "", ls(envir = temp_env(), pattern = "^http_client_"))
}

# Old name of the function
#' @export
#' @rdname http_server_clients
HttpClientsNames <- http_server_clients
