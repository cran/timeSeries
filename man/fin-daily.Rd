\name{SpecialDailySeries}


\title{Special Daily Time Series}


\alias{daily}
\alias{dummyDailySeries}
\alias{dummySeries}
\alias{alignDailySeries}
\alias{rollDailySeries}

%alias{ohlcDailyPlot}


\description{

    Special daily 'timeSeries' functions.
    
}


\details{

    \tabular{ll}{
    \code{dummyDailySeries} \tab Creates a dummy daily 'timeSeries' object, \cr
    \code{alignDailySeries} \tab Aligns a daily 'timeSeries' to new positions,\cr
    \code{rollDailySeries} \tab Rolls daily a 'timeSeries' on a given period, \cr
    \code{ohlcDailyPlot} \tab Plots open high low close bar chart, \cr
    \code{dummySeries} \tab Creates a dummy monthly 'timeSeries' object}
      
}


\usage{
dummyDailySeries(x = rnorm(365), units = NULL, zone = "",
    FinCenter = "")
alignDailySeries(x, method = c("before", "after", "interp", "fillNA",
    "fmm", "periodic", "natural", "monoH.FC"),
    include.weekends = FALSE, units = NULL, zone = "",
    FinCenter = "", ...)
rollDailySeries(x, period = "7d", FUN, \dots)

%ohlcDailyPlot(x, volume = TRUE, colOrder = c(1:5), units = 1e6,
%    xlab = c("Date", "Date"), ylab = c("Price", "Volume"),
%    main = c("O-H-L-C", "Volume"), grid.nx = 7, grid.lty = "solid", \dots)
}


\arguments{

    %\item{colOrder}{
    %    [ohlcDailyPlot] - \cr
    %    an integer vector which gives the order of the prices and the
    %    volume in the input object. By default the following order of
    %    columns from 1 to 5 is assumed: Open, high, low, close, and volume.
    %    }
    \item{FinCenter}{
        a character with the the location of the  financial center named
        as "continent/city".
        }
    \item{FUN}{
        the function to be applied.\cr
        [applySeries] - \cr
        a function to use for aggregation, by default \code{colAvgs}.
        }
    %\item{grid.lty, grid.nx}{
    %    [ohlcDailyPlot] - \cr
    %    The type of grid line and the number of grid lines used in the
    %    plot.
    %    }
    \item{include.weekends}{
         [alignDailySeries] - \cr
         a logical value. Should weekend dates be included or removed
         from the series.
         }
    %\item{main}{
    %    [ohlcDailyPlot] - \cr
    %    a character string to title the price and volume plot.
    %    }
    \item{method}{
        [alignDailySeries] - \cr
        the method to be used for the alignment. A character string, one
        of \code{"before"}, use the data from the row whose position is
        just before the unmatched position, or \code{"after"}, use the
        data from the row whose position is just after the unmatched
        position, or \code{"linear"}, interpolate linearly between
        \code{"before"} and \code{"after"}.
        }
    \item{period}{
        [rollDailySeries] - \cr
        a character string specifying the rollling period composed by the
        length of the period and its unit, e.g. \code{"7d"} represents
        one week.
        }
    \item{units}{
        [allignDailySeries] - \cr
        an optional character string, which allows to overwrite the
        current column names of a \code{timeSeries} object. By default
        \code{NULL} which means that the column names are selected
        automatically.
        %\cr
        %[ohlcDailyPlot] - \cr
        %a numeric value, specifying in which multiples the volume should
        %be referenced on the plot labels. By default 1e6, i.e. in units
        %of 1 Million.
        }
    %\item{volume}{
    %    [ohlcDailyPlot] - \cr
    %    a logigical value. Should a volume plot added to the OHLC Plot.
    %    By default \code{TRUE}.
    %    }
    \item{x}{
        an object of class \code{timeSeries}.
        }
    %\item{xlab, ylab}{
    %    [ohlcDailyPlot] - \cr
    %    two string vectors to name the x and y axis of the price and
    %    volume plot.
    %    }
    \item{zone}{
        the time zone or financial center where the data were recorded.
        }
    \item{\dots}{
        arguments passed to interpolating methods.
        }

}


\value{

    \code{dummyDailySeries}
    \cr
    creates from a numeric matrix with daily records of unknown dates a
    \code{timeSeries} object with dummy daily dates.
    \cr

    \code{alignDailySeries}
    \cr
    returns from a daily time series with missing holidays a weekly
    aligned daily \code{timeSeries} object
    \cr

    \code{rollDailySeries}\cr
    \cr
    returns an object of class \code{timeSeries} with rolling values,
    computed from the function \code{FUN}.
    %\cr
    %
    %\code{ohlcDailyPlot}
    %displays a Open-High-Low-Close Plot of daily data records.

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

