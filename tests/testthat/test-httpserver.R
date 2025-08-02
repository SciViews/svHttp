test_that("The default Http server config is ok", {
  # No Http server, no clients
  expect_true(http_server_port() == 8888)
  expect_true(http_server_name() == "R")
  expect_true(length(http_server_clients()) == 0L)
})
