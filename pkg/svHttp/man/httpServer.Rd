\name{startHttpServer}
\alias{startHttpServer}
\alias{stopHttpServer}
\alias{HttpServerPort}
\alias{HttpServerName}
\alias{HttpClientsNames}
\alias{parHttp}

\title{ Use the R help HTTP server to serve SciViews Komodo or other clients }

\description{
  These functions turn the default R help HTTP server into a RJSONp SciViews
  server (while still serving help pages, of course) for GUI clients like
  Komodo Edit/IDe with the SciViews-K plugin installed.
}

\usage{
startHttpServer(port = HttpServerPort(), name = HttpServerName())
stopHttpServer(remove.clients = FALSE)

HttpServerPort(port)
HttpServerName(name)

HttpClientsNames()
parHttp(client, \dots)
}

\arguments{
  \item{port}{ port on which the server should run (both help and SciViews).
    By default, it is port 8888. Note that this server runs only locally and
    can only serve requests from 127.0.0.1. }
  \item{name}{ the name given to the SciViews server. By default, it is
    \code{R}. }
  \item{remove.clients}{ do we remove also persistent data for the clients,
    \code{FALSE} by default. }
  \item{client}{ the name of one client. A client that does not identify itself
    is named \code{default}. }
  \item{\dots}{ named arguments specifying options to set or change. }
}

\value{
  \code{startHttpServer()}, \code{stopHttpServer()} and \code{HttpServerPort()}
  return an integer indicating the port used. For the last function, it is the
  port that will be used if the HTTP server is not started yet.
  
  The other functions return a character string or a list with the requested
  information.
}

\note{
  For \code{HttpServerPort()} and \code{HttpServerName()}, the argument is
  optional. If it is specified, the corresponding option is changed, otherwise,
  the function just returns the current value of the option. 
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link[svSocket]{startSocketServer}} }

\keyword{ IO }

\concept{ interprocess commnunication Komodo }
