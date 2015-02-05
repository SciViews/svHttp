\name{NEWS}
\title{NEWS file for the svHttp package}

\section{Changes in version 0.9-56}{
	\itemize{
    \item{ Starting from R revision >= 67550, the HTML help port is now
		retrieved using \code{tools::startDynamicHelp(NA)}. }
  }
}

\section{Changes in version 0.9-55}{
	\itemize{
    \item{ LICENSE file eliminated. }
	\item{ No more use of ::: (not allowed by CRAN). }
  }
}

\section{Changes in version 0.9-54}{
	\itemize{
    \item{ NEWS file reworked to use the new Rd format. }
	\item{ Temporary data are now saved in SciViews:TempEnv environment
	  (was TempEnv), and one thus needs svMisc >= 0.9-68. }
  }
}

\section{Changes in version 0.9-53}{
	\itemize{
    \item{ If the Http server is not started, \code{.onUnload()} failed in
			\code{tools::startDynamicHelp(FALSE)}. Fixed. }			
	}
}

\section{Changes in version 0.9-52}{
	\itemize{
    \item{ Functions to manage the SciViews HTTP server have been moved from
			\pkg{svGUI} 0.9-51 to here. }
		
		\item{ The SciViews HTTP server was broken under R 2.13.0 because of wrong
			arguments in the function called by HTTP clients. Fixed now. }
			
	  \item{ The SciViews HTTP server cannot accept very long instructions (they
			are truncated). In such a case, write the instructions in a file instead
			and pass /code{SOURCE=<filepath>} as message. Note that the temporary file
			is deleted by the HTTP server after use. The maximum length of the request
			is perhaps different from one to the other system (I had little problems
			on Mac OS X). It is probably safe to use a file if argument is more than
			100 characters long. Note that encoding for this temporary file is assumed
			to be UTF-8! }
			
		\item{ For additional changes in the Http server in \pkg{svGUI} up to
			version 0.9-51, see its corresponding \preformatted{NEWS} file. }
  }
}
