\name{runlengths}
\alias{runlengths}


\title{Runlengths of a time series}

\description{
    
  Computes runlengths of an univariate \code{"timeSeries"} object.
       
}

\usage{
runlengths(x, \dots)
}

\arguments{

  \item{x}{
    an univariate time series of class \code{"timeSeries"}.
  }
  \item{\dots}{
    arguments passed to the function \code{na.omit}.
  }
        
}

\details{

  Runlengths are defined here as contiguous sequences of values having
  the same sign.

  Zeroes are treated as \code{NA}s.
}

\value{
  an object of class \code{"timeSeries"}
}

\examples{
## random time series - 
   set.seed(4711)
   x <- rnorm(12)
   tS <- timeSeries(data = x, charvec = timeCalendar(), units = "x")
   tS
   
## return runlengths -
   runlengths(tS)

## replace the middle value of the negative stretch of 3 values
tS[5] <- NA
## the two negative values separated by NA are still one run
runlengths(tS)
}

\keyword{chron}
