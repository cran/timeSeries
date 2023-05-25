\name{dummyTimeSeries}
\alias{dummyTimeSeries}

\alias{dummyDailySeries}
\alias{dummyMonthlySeries}

\alias{dummySeries} % deprecated


\title{Dummy time series}

\description{
  Create dummy daily and monthly time series for examples and exploration.
}

\usage{
dummyDailySeries(x = rnorm(365), units = NULL, zone = "", 
                 FinCenter = "")

dummyMonthlySeries(\dots)
}

\arguments{

  \item{x}{
    an object of class \code{timeSeries}.
  }
  
  \item{units}{
  
    an optional character string, which allows to overwrite the current
    column names of a \code{timeSeries} object. By default \code{NULL}
    which means that the column names are selected automatically.

  }
  \item{FinCenter}{

    a character with the the location of the financial center named as
    \code{"continent/city"}.
    
  }
  \item{zone}{
    the time zone or financial center where the data were recorded.
  }
  \item{...}{
    optional arguments passed to \code{timeSeries}.
  }
}

\details{

  \code{dummyDailySeries} creates a \code{timeSeries} object with
  dummy daily dates from a numeric matrix with daily records of
  unknown dates.
  
  \code{dummyMonthlySeries} creates a dummy monthly \code{"timeSeries"} object.
      
}

\value{
  a \code{"timeSeries"} object
}

\examples{
dd <- dummyDailySeries()
head(dd)
tail(dd)

dummyMonthlySeries(y = 2022)
}
\keyword{chron}
\keyword{ts}
