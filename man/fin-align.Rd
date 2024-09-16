\name{align-methods}
\docType{methods}

\alias{align}
\alias{align-methods}
\alias{align,timeSeries-method}

\alias{alignDailySeries}
\alias{daily2weekly}
\alias{daily2monthly}


\title{Align a 'timeSeries' object to equidistant time stamps}

\description{

  Aligns a \code{"timeSeries"} object to equidistant time stamps.  There
  are also functions for the common cases of changing daily to weekly
  and daily to monthly.

}

\usage{
\S4method{align}{timeSeries}(x, by = "1d", offset = "0s",
    method = c("before", "after", "interp", "fillNA",
               "fmm", "periodic", "natural", "monoH.FC"),
    include.weekends = FALSE, ...)

alignDailySeries(x, method = c("before", "after", "interp", "fillNA",
    "fmm", "periodic", "natural", "monoH.FC"),
    include.weekends = FALSE, units = NULL, zone = "",
    FinCenter = "", ...)

daily2monthly(x, init = FALSE) 
daily2weekly(x, startOn = "Tue", init = FALSE)
}

\arguments{

  \item{x}{
    an object of class \code{"timeSeries"}.
  }
  \item{by}{
    a character string denoting the period.
  }
  \item{offset}{
    a character string denoting the offset.
  }
  \item{method}{

    the method to be used for the alignment. A character string, one of
    \code{"before"}, use the data from the row whose position is just
    before the unmatched position, or \code{"after"}, use the data from
    the row whose position is just after the unmatched position, or
    \code{"linear"}, interpolate linearly between \code{"before"} and
    \code{"after"}.

  }
  \item{include.weekends}{

    a logical value. Should weekend dates be included or removed from
    the series?

  }
  \item{units}{
    
    an optional character string, which allows to overwrite the current
    column names of a \code{timeSeries} object. By default \code{NULL}
    which means that the column names are selected automatically.
    
  }
  \item{zone}{
    the time zone or financial center where the data were recorded.
  }
  \item{FinCenter}{
    a character with the the location of the  financial center named
    as \code{"continent/city"}.
  }
  \item{startOn}{

    a character string, specifying the day of week as a three letter
    abbreviation. Weekly aggregated data records are then fixed to the
    weekdays given by the argument \code{startOn}.

  }
  \item{init}{

    a logical value, if set to \code{TRUE} then the time series will be
    indexed to 1 for its first value. By default \code{init} is set to
    \code{FALSE}.

  }
  \item{\ldots}{
    further arguments to be passed to the interpolating function.
  }
}

\details{

  TODO: complete.

  \code{alignDailySeries} aligns a daily 'timeSeries' to new positions,
  Effectively, it is a frontend to the \code{"timeSeries"} method for
  \code{align} with \code{by = "1d"}, and \code{offset = "0s"}.

  In addition, there are two tailored functions for common cases:
  \code{daily2monthly} and \code{daily2weekly} which aggregate
  \code{"timeSeries"} objects from daily to monthly or weekly levels,
  respectively.
    
  In the case of the function \code{daily2weekly} one can explicitly set
  the starting day of the week, the default value is Tuesday,
  \code{startOn = "Tue"}.
 


}

\seealso{
  \code{\link{aggregate}},
  \code{\link{apply}}
}

\value{
  a \code{"timeSeries"} object,

  for \code{alignDailySeries}, a weekly aligned daily \code{"timeSeries"} object 
  from a daily time series with missing holidays.

}

\examples{
## Use Microsofts' OHLCV Price Series -
   head(MSFT)
   end(MSFT)

## Use MSFT and Compute Sample Size -
   dim(MSFT)

## Align the Series -
   MSFT.AL <- align(MSFT)

## Show the Size of the Aligned Series -
   dim(MSFT.AL)


## alignDailySeries

## Cut out April Data from 2001 -
   Close <- MSFT[, "Close"]
   tsApril01 <- window(Close, start="2001-04-01", end="2001-04-30")
   tsApril01

## Align Daily Series with NA -
   tsRet <- returns(tsApril01, trim = TRUE)
   GoodFriday(2001)
   EasterMonday(2001)
   alignDailySeries(tsRet, method = "fillNA", include.weekends = FALSE)
   alignDailySeries(tsRet, method = "fillNA", include.weekends = TRUE)

## Align Daily Series by Interpolated Values -
   alignDailySeries(tsRet, method = "interp", include.weekend = FALSE)
   alignDailySeries(tsRet, method = "interp", include.weekend = TRUE)


## Load Microsoft Data Set -
   x <- MSFT

## Aggregate daily records to end of month records -
   X <- daily2monthly(x)
   X
   isMonthly(X)
   
## Aggregate daily records to end of week records -
   X <- daily2weekly(x, startOn="Fri")
   X
   dayOfWeek(time(X))
}

\keyword{methods}
\keyword{chron}

