\name{orderStatistics}
\alias{orderStatistics}


\title{Order statistics}

\description{
    
    Computes order statistics of a \code{"timeSeries"}. 
}


\usage{
orderStatistics(x)
}


\arguments{

    \item{x}{
        an univariate \code{"timeSeries"} object.
        }

}


\value{

    Function \code{orderStatistics} returns the order statistics of
    an univariate \code{"timeSeries"} object. The output is an object of
    class \code{"list"}.
   
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
