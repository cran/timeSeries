\name{cumulated}
\alias{cumulated}
\alias{cumulated.default}


\title{Cumulated time series from returns}

\description{
    
  Computes a cumulated financial \code{"timeSeries"}, e.g. prices or
  indexes, from financial returns.

}

\usage{
cumulated(x, \dots)

\method{cumulated}{default}(x, method = c("continuous", "discrete", 
    "compound", "simple"), percentage = FALSE, \dots)
}

\arguments{

  \item{x}{
    an object of class \code{timeSeries}.
  }
  \item{method}{
    a character string, the method for computation of returns.
  } 
  \item{percentage}{
    a logical value. By default \code{FALSE}, if \code{TRUE} the series
    will be expressed in percentage changes.
  }   
  \item{\dots}{
    ignored by the default method.
  }
}

\details{

  Note, the function \code{cumulated} assumes as input discrete returns
  from a price or index series. Only then the cumulated series agrees
  with the original price or index series. The first values of the
  cumulated series cannot be computed, it is assumed that the series is
  indexed to 1.
        
}

\value{
  a \code{"timeSeries"} object
}

\seealso{
  \code{\link{returns}},
  %\code{\link{cumulated}},
  \code{\link{drawdowns}},
  \code{\link{splits}},
  %\code{\link{spreads}},
  \code{\link{midquotes}},
  \code{\link{index2wealth}}
}

\examples{
## Use the Microsofts' Close Prices Indexed to 1 - 
   MSFT.CL <- MSFT[, "Close"]
   MSFT.CL <- MSFT.CL/MSFT[[1, "Close"]]
   head(MSFT.CL)

## Compute Discrete Return -    
   MSFT.RET <- returns(MSFT.CL, method = "discrete")
   
## Cumulated Series and Compare - 
   MSFT.CUM <- cumulated(MSFT.RET, method = "discrete") 
   head(cbind(MSFT.CL, MSFT.CUM))
}

\keyword{chron}
