\name{rowCum}
\alias{rowCum}

\alias{rowCumsums}
\alias{rowCumsums,ANY-method}
\alias{rowCumsums,timeSeries-method}


\title{Cumulative row statistics}

\description{
  Compute cumulative row statistics.
}

\usage{
\S4method{rowCumsums}{ANY}(x, na.rm = FALSE, \dots)
\S4method{rowCumsums}{timeSeries}(x, na.rm = FALSE, \dots)
}

\arguments{
  
  \item{x}{

    a time series, may be an object of class \code{"matrix"} or
    \code{"timeSeries"}.

  }
  \item{na.rm}{
    a logical. Should missing values be removed?
  }
  \item{\dots}{
    arguments to be passed.
  }
}

\value{
  for the default method, a matrix,
  
  for the \code{"timeSeries"} method, an S4 object of class \code{"timeSeries"}.
}

\seealso{
  \code{\link[=colCum]{colCumXXX}}
}

\examples{
## Simulated Monthly Return Data - 
   X = matrix(rnorm(24), ncol = 2)
     
## Compute cumulated Sums -
   rowCumsums(X)  
}

\keyword{univar}
