#' Stop the SciViews and R HTTP server and eliminate all tracks
#'
#' @param remove.clients do we remove also persistent data for the clients,
#' `FALSE` by default.
#'
#' @return A number with the port of the HTTP server.
#' @export
#' @seealso [start_http_server()] for a complete example.
#' @keywords IO
#' @concept Interprocess communication
stop_http_server <- function(remove.clients = FALSE) {
  # Eliminate the SciViews custom process function for HTTP server
  e <- getNamespace("tools")$.httpd.handlers.env
  if ("SciViews" %in% ls(envir = e)) rm(list = "SciViews", envir = e)

  # Do we also remove persistent data for clients?
  if (isTRUE(remove.clients))
    rm(list = ls(envir = temp_env(), pattern = "^http_client_"),
      envir = temp_env())

  # Stop the HTTP deamon
  try(tools::startDynamicHelp(FALSE), silent = TRUE)
}

# Old name of the function
#' @export
#' @rdname stop_http_server
stopHttpSever <- stop_http_server
