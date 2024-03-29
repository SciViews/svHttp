% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/http_server_name.R
\name{http_server_name}
\alias{http_server_name}
\alias{HttpServerName}
\title{Get or change the name of the HTTP server}
\usage{
http_server_name(name)

HttpServerName(name)
}
\arguments{
\item{name}{the name given to the SciViews server. By default, it is \code{R}.}
}
\value{
A character vector with the name of the HTTP server.
}
\description{
Get or change the name of the HTTP server
}
\examples{
http_server_name()
}
\seealso{
\code{\link[=start_http_server]{start_http_server()}} for a complete example.
}
\concept{Interprocess communication}
\keyword{IO}
