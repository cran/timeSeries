\name{is.timeSeries}
\alias{is.timeSeries}
\alias{is.signalSeries}


\title{Check if an object is from class 'timeSeries'}

\description{

  \code{is.timeSeries} tests if its argument is a \code{timeSeries}.
  \code{is.signalSeries} tests if series has no timestamps.
  
}

\usage{
is.timeSeries(x)
is.signalSeries(x)
}

\arguments{
  \item{x}{  
    an object.
  }
}

\value{

  a logical value, \code{TRUE} or \code{FALSE}.
  
}

\examples{
## Create an artificial 'timeSeries' object - 
   setRmetricsOptions(myFinCenter = "GMT")
   charvec <- timeCalendar()
   data <- matrix(rnorm(12))
   TS <- timeSeries(data, charvec, units = "RAND")
   TS

## Test for 'timeSeries' - 
   is.timeSeries(TS)
}

\keyword{chron}
