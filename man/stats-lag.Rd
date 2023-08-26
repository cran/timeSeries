\name{lag}
\alias{lag}

\alias{lag,timeSeries-method}
\alias{lag.timeSeries}


\title{Lag a 'timeSeries' object}

\description{
    
  Compute a lagged version of a \code{"timeSeries"} object.
   
}

\usage{
\S4method{lag}{timeSeries}(x, k = 1, trim = FALSE, units = NULL, \dots)
}

\arguments{
         
  \item{x}{
    an object of class \code{timeSeries}.
  } 
  \item{k}{
    
    an integer number, the number of lags (in units of observations).
    By default 1. Can also be a vector, in which case the result is a
    multivariate \code{"timeSeries"} in which column \code{i} contains
    the series lagged by \code{k[i]}, see the examples.

  }        
  \item{trim}{
    a logical value. By default \code{TRUE}, the first missing
    observation in the return series will be removed.
  }
  \item{units}{

    an optional character string, which allows to overwrite the current
    column names of a \code{"timeSeries"} object. By default \code{NULL}
    which means that the column names are selected automatically.

  }
  \item{\dots}{
    arguments passed to other methods.
  }
}

\value{
  an object of class \code{"timeSeries"}
}

\seealso{
  \code{\link[stats]{lag}} for \verb{stats::lag},
  \code{\link{diff}}
}

\examples{
## Load Micsrosoft Data Set -
   x = MSFT[1:20, "Open"]
   
## Lag the 'timeSeries' Object:
   lag(x, k = -1:1)
}

\keyword{chron}
