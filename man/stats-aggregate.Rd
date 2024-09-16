\name{aggregate-methods}
\alias{aggregate-methods}

\docType{methods}

\alias{aggregate}
\alias{aggregate.timeSeries}
\alias{aggregate,timeSeries-method}


\title{Aggregate time series}

\description{
    
  Aggregate a \code{"timeSeries"} object over general periods.  
    
}

\usage{
\S4method{aggregate}{timeSeries}(x, by, FUN, \dots)
}

\arguments{
  
  \item{x}{
    an object of class \code{"timeSeries"}.
  } 
  \item{by}{
    a sequence of \code{"timeDate"} objects denoting the aggregation
    periods, see section \sQuote{Details}.
  }
  \item{FUN}{
    the function to be applied.
  }               
  \item{\dots}{
    arguments passed to other methods.
  }
}

\details{

  \code{aggregate} aggregates \code{x} by applying \code{FUN} on the
  values of the time series in each of the aggregation periods,
  specified by argument \code{by}.

  Argument \code{by} should be of the same class as
  \code{time(x)}. \code{by} is sorted and duplicated values are removed
  from it. Each pair of consecutive values in \code{by} then determines
  a period over which to apply the aggregation function \code{FUN}, see
  \code{\link[base]{findInterval}}.

}

\value{
  an object of class \code{"timeSeries"}
}

\seealso{
  \code{\link{apply}},
  \code{\link{align}}
}

\examples{
## Load Microsoft Data Set -
   x <- MSFT

## Aggregate by Weeks - 
   by <- timeSequence(from = start(x),  to = end(x), by = "week")
   aggregate(x, by, mean)

## Aggregate to Last Friday of Month -
   by <- unique(timeLastNdayInMonth(time(x), 5))
   X <- aggregate(x, by, mean)
   X
   dayOfWeek(time(X))
   isMonthly(X)

## Aggregate to Last Day of Quarter -
   by <- unique(timeLastDayInQuarter(time(x)))
   X <-  aggregate(x, by, mean)
   X
   isQuarterly(X)
}

\keyword{methods}
\keyword{chron}
