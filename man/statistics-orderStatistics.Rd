\name{orderStatistics}
\alias{orderStatistics}


\title{Order statistics}

\description{
    Computes the order statistics of a \code{"timeSeries"} object. 
}

\usage{
orderStatistics(x)
}

\arguments{
  \item{x}{
    a \code{"timeSeries"} object.
  }
}

\details{

  \code{orderStatistics} computes the order statistics for each column of a
  \code{"timeSeries"} object. The output is a named list with the order
  statistics for each column in a separate component.
   
}

\value{

  a named list, in which each component is an
  univariate \code{"timeSeries"} containing the order statistics of the
  corresponding column of the input time series.

}

\examples{
## Load Swiss Pension Fund Benchmark Data -  
   setRmetricsOptions(myFinCenter = "GMT")
   X <- LPP2005REC[, "SPI"]
   colnames(X)
   
## Compute 1\% Order Statistics -
   N <- round(0.01*nrow(X))
   N
   OS <- orderStatistics(X)[[1]]
   OS[1:N, ]
}

\keyword{chron}
