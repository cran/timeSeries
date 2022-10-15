\name{SpecialDailySeries}

\alias{daily}

\alias{alignDailySeries}
\alias{rollDailySeries}

%alias{ohlcDailyPlot}


\title{Special daily time series}


\description{

    Special daily 'timeSeries' functions.
    
}

\usage{
alignDailySeries(x, method = c("before", "after", "interp", "fillNA",
    "fmm", "periodic", "natural", "monoH.FC"),
    include.weekends = FALSE, units = NULL, zone = "",
    FinCenter = "", ...)

rollDailySeries(x, period = "7d", FUN, \dots)
}


\arguments{

  \item{x}{
    an object of class \code{timeSeries}.
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
  \item{period}{

    a character string specifying the rollling period composed by the
    length of the period and its unit, e.g. \code{"7d"} represents one
    week.

  }
  \item{FUN}{
    a function to use for aggregation, by default \code{colMeans}.
  }
  \item{\dots}{
    arguments passed to interpolating methods.
  }
}

\details{

  \code{alignDailySeries} aligns a daily 'timeSeries' to new positions,
  
  \code{rollDailySeries} rolls daily a 'timeSeries' on a given period.
}


\value{

    for \code{alignDailySeries}, a weekly aligned daily
    \code{timeSeries} object from a daily time series with missing
    holidays. \cr

    for \code{rollDailySeries}, an object of class \code{timeSeries}
    with rolling values, computed from the function \code{FUN}.

}


\examples{
## Use Microsofts' OHLCV Price Series -
   head(MSFT)
   end(MSFT)

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
}


\keyword{chron}
\keyword{ts}


%     \code{ohlcDailyPlot} \tab Plots open high low close bar chart, \cr

%ohlcDailyPlot(x, volume = TRUE, colOrder = c(1:5), units = 1e6,
%    xlab = c("Date", "Date"), ylab = c("Price", "Volume"),
%    main = c("O-H-L-C", "Volume"), grid.nx = 7, grid.lty = "solid", \dots)

    %\cr
    %
    %\code{ohlcDailyPlot}
    %displays a Open-High-Low-Close Plot of daily data records.

  %\item{colOrder}{
  %    [ohlcDailyPlot] - \cr
  %    an integer vector which gives the order of the prices and the
  %    volume in the input object. By default the following order of
  %    columns from 1 to 5 is assumed: Open, high, low, close, and volume.
  %    }

  %\item{grid.lty, grid.nx}{
  %    [ohlcDailyPlot] - \cr
  %    The type of grid line and the number of grid lines used in the
  %    plot.
  %    }
  
  %\item{main}{
  %    [ohlcDailyPlot] - \cr
  %    a character string to title the price and volume plot.
  %    }

      %\cr
      %[ohlcDailyPlot] - \cr
      %a numeric value, specifying in which multiples the volume should
      %be referenced on the plot labels. By default 1e6, i.e. in units
      %of 1 Million.
  
  %\item{volume}{
  %    [ohlcDailyPlot] - \cr
  %    a logigical value. Should a volume plot added to the OHLC Plot.
  %    By default \code{TRUE}.
  %    }

  %\item{xlab, ylab}{
  %    [ohlcDailyPlot] - \cr
  %    two string vectors to name the x and y axis of the price and
  %    volume plot.
  %    }
