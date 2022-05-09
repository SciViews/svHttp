# svHttp 1.0.4

-   A link to `svSocket::start_socket_server()`is updated.

# svHttp 1.0.3

-   Correction of two links in the vignette.

# svHttp 1.0.2

-   Silent checking of current port in `start_http_server()` and `http_server_port()`.

# svHttp 1.0.1

-   The vignette is completed.

# svHttp 1.0.0

-   All functions are renamed to use snake_case, e.g., `startHttpServer()` is renamed `start_http_server()`. Old names remain for backward compatibility, but they are deprecated.

# svHttp 0.9-56

-   Starting from R revision \>= 67550, the HTML help port is now retrieved using `tools::startDynamicHelp(NA)`.

# svHttp 0.9-55

-   LICENSE file eliminated.

-   No more use of `:::` (not allowed by CRAN).

# svHttp 0.9-54

-   NEWS file reworked to use the new Rd format.

-   Temporary data are now saved in SciViews:TempEnv environment (was TempEnv), and one thus needs svMisc \>= 0.9-68.

# svHttp 0.9-53

-   If the Http server is not started, `.onUnload()` failed in `tools::startDynamicHelp(FALSE)`. Fixed.

# svHttp 0.9-52

-   Functions to manage the SciViews HTTP server have been moved from svGUI 0.9-51 to here.

-   The SciViews HTTP server was broken under R 2.13.0 because of wrong arguments in the function called by HTTP clients. Fixed now.

-   The SciViews HTTP server cannot accept very long instructions (they are truncated). In such a case, write the instructions in a file instead and pass `SOURCE=<filepath>` as message. Note that the temporary file is deleted by the HTTP server after use. The maximum length of the request is perhaps different from one to the other system (I had little problems on Mac OS X). It is probably safe to use a file if argument is more than 100 characters long. Note that encoding for this temporary file is assumed to be UTF-8!

-   Transcript of changes concerning the HTTP server from svGUI here under:

-   HTTP server now works with the new version of `captureAll()` from svMisc 0.9-62 and it is compatible with its echo = and split = arguments.

-   HTTP server now works correctly with incomplete commands (bug corrected).

-   HTTP server code processing now uses `parseText()` of svMisc \>= 0.9-60 instead of the deprecated `Parse()` function.

-   The R http server is modified to work with either RJSONp calls, or with plain text exchange, as the SciViews socket server works. RJSONp objects returned use `list()` to create lists, but also structures or new S4 objects.

-   A new series of function to communication with a SciViews GUI client like Komodo/SciViews-K by using the R http help server is added. It offers a tcltk-free alternative to the svSocket server.

-   The package no longer starts the socket server implemented in svSocket and it does not import svSocket any more. As the HTTP server is an alternative, one could now choose to run SciViews communication through the HTTP server without using svSocket, and thus, without starting Tcl/Tk any more.
