## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(svHttp)
server_port <- start_http_server()
server_port

## -----------------------------------------------------------------------------
http_server_run <- function(cmd, port = 8888) {
  # cmd is a string containing the command to process. We have to URLencode it
  cmd <- utils::URLencode(cmd)
  url <- paste0("http://localhost:", port, "/custom/SciViews?", cmd)
  res <- curl::curl_fetch_memory(url)
  if (res$status_code %in% c(200, 500)) {# Should be OK
    rawToChar(res$content)
  } else if (res$status_code > 0) {
    stop("Error while executing ", url, ": error ", res$status_code)
  }
}

## ---- eval=FALSE--------------------------------------------------------------
#  cat(http_server_run("R.version"))
#  cat(http_server_run("http_server_name()"))
#  res <- http_server_run("ls()")
#  # Now you could do whatever you want with res
#  res

## -----------------------------------------------------------------------------
http_server_port()
http_server_name()
# Change the name
http_server_name("myHttpServer")

## -----------------------------------------------------------------------------
http_server_clients()

## -----------------------------------------------------------------------------
stop_http_server()

