\name{TimeSeriesData}
\alias{TimeSeriesData}

\alias{LPP2005REC}
\alias{MSFT}
\alias{USDCHF}


\title{Time series data sets} 

\description{
   Three data sets used in example files.
}

\details{

  The following datasets are available:
  
  \describe{   
    \item{MSFT}{
      Daily Microsoft OHLC (Open-high-low-close) prices and volume from
      2000-09-27 to 2001-09-27.
    }
    \item{USDCHF}{
      USD/CHF intraday foreign exchange rates.
    }
    \item{LPP2005REC}{

      Swiss pension fund assets returns benchmark from 2005-11-01 to
      2007-04-11.
      
    }
  }

  The datasets are objects from class \code{"timeSeries"}.

}

\note{

  No further information about the \code{LPP2005REC} is available. The
  meaning of the columns?

}

\seealso{
  \code{\link{readSeries}},
  \code{\link{timeSeries}}
}

\examples{
## LPP2005 example data set
data(LPP2005REC)
plot(LPP2005REC, type = "l")
class(LPP2005REC)
dim(LPP2005REC)
head(LPP2005REC)
LPP2005REC[1:5, 2:4]
range(time(LPP2005REC))
summary(LPP2005REC)
   
## MSFT example data set
data(MSFT)
plot(MSFT[, 1:4], type = "l")
plot(MSFT[, 5], type = "h")
class(MSFT)
range(time(MSFT))
head(MSFT)
   
## Plot USDCHF example data set
data(USDCHF)
plot(USDCHF)
range(time(USDCHF))
head(USDCHF)
}
 
\keyword{datasets}
