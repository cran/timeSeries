\name{isRegular}


\alias{isDaily,timeSeries-method}
\alias{isMonthly,timeSeries-method}
\alias{isQuarterly,timeSeries-method}

\alias{isRegular,timeSeries-method}

\alias{frequency,timeSeries-method}



\title{Checks if a time series is regular}

\description{

    Checks if a time series is regular. 
    
}


\usage{
\S4method{isDaily}{timeSeries}(x)
\S4method{isMonthly}{timeSeries}(x)
\S4method{isQuarterly}{timeSeries}(x)

\S4method{isRegular}{timeSeries}(x)

\S4method{frequency}{timeSeries}(x, \dots)
}


\arguments{

    \item{x}{  
        an R object of class 'timeSeries'.
    }
    \item{\dots}{
        arguments to be passed.
    }

}

\details{

  What is a regular time series? If a time series is daily, monthly, or
  weekly, then we speak of a regular series.  This can be tested calling
  the functions \code{isDaily}, \code{isMonthly}, \code{isQuarterly}, or
  in general \code{isRegular}. If the series is regular then its
  frequency can be determined by calling \code{frequency}.

  Here are the definitions of daily, monthly, and quarterly time series:

  \describe{

    \item{daily}{if the series has no more than one date/time stamp per
      day.}

    \item{monthly}{if the series has no more than one date/time stamp
      per month.}

    \item{quarterly}{if the series has no more than one date/time stamp
      per quarter.}
  }

  A regular series is either a monthly or a quarterly series.
    
  Note that with the above definitions a monthly series is also a daily
  series, a quarterly series is also a monthly series. On the other
  hand, a daily series is not regular!

  
  NOT yet implemented is the case of weekly series.
  
}

\value{

    The \code{is*} functions return \code{TRUE} or \code{FALSE} depending 
    on whether the series fulfills the condition or not.\cr
    
    \code{frequency} returns in general 1, for quarterly series 4,
    and for monthly series 12.
  
}


\examples{
data(MSFT)
isRegular(MSFT)

## a monthly ts
ap <- as.timeSeries(AirPassengers)
isRegular(ap)

## a quarterly ts
pres <- as.timeSeries(presidents)
isRegular(pres)
}


\keyword{chron}

