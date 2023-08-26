\name{splits}                     
\alias{splits}
                                    
                                    
\title{splits}

\description{

  Searches for outlier splits in a \code{"timeSeries"} object.
    
}

\usage{
splits(x, sd = 3, complement = TRUE, ...)
}

\arguments{

  \item{x}{ 
    a \code{"timeSeries"} object.
  }
  \item{sd}{

    \code{numeric(1)}; deviations of how many standard deviations to
    consider too big? Can be fractional. E.g., 5 means that values
    larger or smaller than five times the standard deviation of the
    series will be detected.

  }
  \item{complement}{
    a logical flag, should the outlier series or its complements 
    be returned?
  }
  \item{\dots}{
    arguments to be passed.
  }
    
}

\details{

  This function finds splits in financial price or index series. If a
  price or index is splitted we observe a big jump of several standard
  deviations in the returns, which is identified usually as an outlier.
    
}

\value{
  a \code{"timeSeries"} object
}

\seealso{
  \code{\link{returns}},
  \code{\link{cumulated}},
  \code{\link{drawdowns}},
  %\code{\link{splits}},
  \code{\link{spreads}},
  \code{\link{midquotes}},
  \code{\link{index2wealth}}
}

\examples{
## Create a Return Series with a Split - 
   data <- runif(12, -1, 1)
   data[6] <- 20
   x <- timeSeries(data, timeCalendar(), units="RUNIF")
   x
   
## Search for the Split:
   splits(x, sd=3, complement=TRUE)
   splits(x, sd=3, complement=FALSE)
}
