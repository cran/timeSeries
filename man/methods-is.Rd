\name{is.timeSeries}


\title{Check if an object is from class 'timeSeries'}


\alias{is.timeSeries}
\alias{is.signalSeries}
\alias{is.na,timeSeries-method}
\alias{is.unsorted,timeSeries-method}


\description{

  \code{is.timeSeries} tests if its argument is a \code{timeSeries}.
  \code{is.timeSeries} tests if series has no timestamps.
  
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

    \code{TRUE} or \code{FALSE} depending on whether its
    argument is an object of class \code{"timeSeries"} or not.
  
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
