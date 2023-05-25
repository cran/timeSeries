\name{colCum}
\alias{colCum}

\alias{colCummaxs}
\alias{colCummins}
\alias{colCumprods}
\alias{colCumreturns}
\alias{colCumsums}

\alias{colCummaxs,matrix-method}
\alias{colCummaxs,timeSeries-method}
\alias{colCummins,matrix-method}
\alias{colCummins,timeSeries-method}
\alias{colCumprods,matrix-method}
\alias{colCumprods,timeSeries-method}
\alias{colCumreturns,matrix-method}
\alias{colCumreturns,timeSeries-method}
\alias{colCumsums,matrix-method}
\alias{colCumsums,timeSeries-method}



\title{Cumulated Column Statistics}

\description{
    Functions to compute cumulative column statistics.
}


\usage{
\S4method{colCumsums}{timeSeries}(x, na.rm = FALSE, \dots)

\S4method{colCummaxs}{timeSeries}(x, na.rm = FALSE, \dots)

\S4method{colCummins}{timeSeries}(x, na.rm = FALSE, \dots)

\S4method{colCumprods}{timeSeries}(x, na.rm = FALSE, \dots)

\S4method{colCumreturns}{timeSeries}(x, method = c("geometric", "simple"), na.rm = FALSE, \dots)

}


\arguments{
  
    \item{method}{
        a character string to indicate if geometric (TRUE) or simple 
        (FALSE) returns should be computed.
        }
    \item{na.rm}{
        a logical. Should missing values be removed?
        }
    \item{x}{
        a time series, may be an object of class \code{"matrix"}, 
        or \code{"timeSeries"}.
        }
    \item{\dots}{
        arguments to be passed.
        }
        
}


\value{

  \code{"matrix"} for the default methods of all functions,
  
  \code{"timeSeries"} for the  \code{"timeSeries"} methods
    
}


\examples{
## Simulated Return Data -
   x = matrix(rnorm(24), ncol = 2)
     
## Cumulative Sums Column by Column  -
   colCumsums(x)  
}


\keyword{univar}

