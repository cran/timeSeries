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


\title{Cumulated column statistics}

\description{
  Functions to compute cumulative column statistics.
}

\usage{
\S4method{colCumsums}{timeSeries}(x, na.rm = FALSE, \dots)

\S4method{colCummaxs}{timeSeries}(x, na.rm = FALSE, \dots)

\S4method{colCummins}{timeSeries}(x, na.rm = FALSE, \dots)

\S4method{colCumprods}{timeSeries}(x, na.rm = FALSE, \dots)

\S4method{colCumreturns}{timeSeries}(x, method = c("geometric", "simple"),
                                     na.rm = FALSE, \dots)
}

\arguments{
  
  \item{x}{

    a time series, may be an object of class \code{"matrix"}, or
    \code{"timeSeries"}.

  }
  \item{na.rm}{
    a logical. Should missing values be removed?
  }
  \item{method}{

    a character string to indicate if geometric (\code{TRUE}) or simple
    (\code{FALSE}) returns should be computed.

  }
  \item{\dots}{
    arguments to be passed.
  }
}

\details{

  These functions compute the requested cumulative quantities columnwise
  to obtain a matrix of the same dimension as the data.  The
  \code{"timeSeries"} methods replace the data part of the original
  object with the resulting matrix.

  The \code{"timeSeries"} methods for the \code{Math} group functions
  \code{cummin}, \code{cummax}, \code{cumsum}, and \code{cumprod}, 
  work similarly but don't have the \code{na.rm} argument.
  
}

\value{
  \code{"matrix"} for the default methods of all functions,
  
  \code{"timeSeries"} for the  \code{"timeSeries"} methods
}

\seealso{
  \code{\link{Math,timeSeries-method}},
  \code{\link{rowCumsums}}

}
\examples{
## simulate return data
x <- matrix(rnorm(24), ncol = 2)
X <- as.timeSeries(x)
     
## cumulative sums  by column  -
class(colCumsums(x))  # "matrix"
class(colCumsums(X))  # "timeSeries"

colCumsums(X)
}

\keyword{univar}
