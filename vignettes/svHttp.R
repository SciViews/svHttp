## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----start_server, eval=FALSE-------------------------------------------------
# library(svHttp)
# server_port <- start_http_server()
# server_port

## ----no_server_version, include=FALSE-----------------------------------------
server_ready <- FALSE

## ----wait, include=FALSE, warning=TRUE, error=TRUE----------------------------
try({
# Launch the server
server_port <- 0L
res <- try({
  library(svHttp)
  server_port <- start_http_server()
  }, silent = TRUE)
server_ready <- !inherits(res, "try-error")
server_port
})

## ----run_server_function------------------------------------------------------
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

## ----run_sever_examples, eval=FALSE-------------------------------------------
# cat(http_server_run("R.version"))
# cat(http_server_run("http_server_name()"))
# res <- http_server_run("ls()")
# # Now you could do whatever you want with res
# res

## ----server_infos, eval=server_ready------------------------------------------
http_server_port()
http_server_name()
# Change the name
http_server_name("myHttpServer")

## ----get_clients, eval=server_ready-------------------------------------------
http_server_clients()

## ----stop_server, eval=FALSE--------------------------------------------------
# stop_http_server()

## ----try_stop_server, include=FALSE, warning=TRUE, error=TRUE-----------------
try({
try(stop_http_server(), silent = TRUE)
})

