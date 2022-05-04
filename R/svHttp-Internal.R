.onLoad <- function(lib, pkg) {
  # Try starting the Http server
  #try(start_http_server(), silent = TRUE)
}

.onUnload <- function(libpath) {
  # Make sure that the SciViews Http server is closed
  stop_http_server(TRUE)
}
