\name{drawdowns}
\alias{drawdowns}

\alias{drawdownsStats}


\title{Calculations of drawdowns}

\description{
    
  Compute series of drawdowns from financial returns and calculate
  drawdown statisitcs.
    
}

\usage{
drawdowns(x, \dots)

drawdownsStats(x, \dots)
}

\arguments{

  \item{x}{

    a \code{"timeSeries"} object of financial returns.  Note, drawdowns
    can be calculated from an uni- or multivariate time deries object,
    statistics can only be computed from an univariate time series
    object.

  }
  \item{\dots}{
    optional arguments passed to \code{na.omit}.
  }
        
}

\value{

  for \code{drawdowns}, an object of class \code{timeSeries}.
    
  for \code{drawdownsStats} an object of
  class \code{"data.frame"} with the following components:
  \item{drawdown}{the depth of the drawdown, } 
  \item{from}{the start date, }  
  \item{trough}{the trough period, } 
  \item{to}{the end date, } 
  \item{length}{the length in number of records, } 
  \item{peaktrough}{the peak trough, and } 
  \item{recovery}{the recovery length in number of records.}
        
}
    
\details{

    The code in the core of the function \code{drawdownsStats}
    was borrowed from the package \code{PerformanceAnalytics} 
    authored by Peter Carl and Sankalp Upadhyay.
    
}
   
\author{

  Peter Carl and Sankalp Upadhyay for code from the contributed R
  package \code{PerformanceAnalytics} used in the function
  \code{drawdownsStats}.
    
}

\seealso{
  \code{\link{returns}},
  \code{\link{cumulated}},
  %\code{\link{drawdowns}},
  \code{\link{splits}},
  %\code{\link{spreads}},
  \code{\link{midquotes}},
  \code{\link{index2wealth}}
}

\examples{
## Use Swiss Pension Fund Data Set of Returns - 
   head(LPP2005REC)
   SPI <- LPP2005REC[, "SPI"]
   head(SPI)
   
## Plot Drawdowns - 
   dd = drawdowns(LPP2005REC[, "SPI"], main = "Drawdowns")
   plot(dd)
   dd = drawdowns(LPP2005REC[, 1:6], main = "Drawdowns")
   plot(dd)

## Compute Drawdowns Statistics - 
   ddStats <- drawdownsStats(SPI)
   class(ddStats)
   ddStats
   
## Note, Only Univariate Series are allowd -
   ddStats <- try(drawdownsStats(LPP2005REC))
   class(ddStats)
}

\keyword{chron}
