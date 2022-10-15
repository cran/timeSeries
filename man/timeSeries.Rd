\name{TimeSeriesClass}
\alias{TimeSeriesClass}

\alias{timeSeries}

\alias{timeSeries,ANY,ANY-method}
\alias{timeSeries,ANY,missing-method}
\alias{timeSeries,ANY,timeDate-method}
\alias{timeSeries,matrix,ANY-method}
\alias{timeSeries,matrix,missing-method}
\alias{timeSeries,matrix,timeDate-method}
\alias{timeSeries,missing,ANY-method}
\alias{timeSeries,missing,missing-method}
\alias{timeSeries,missing,timeDate-method}
\alias{timeSeries,matrix,numeric-method}

\alias{index_timeSeries}
\alias{time_timeSeries}
\alias{index_timeSeries-class}
\alias{time_timeSeries-class}

\alias{summary.timeseries}


\title{Create objects from class 'timeSeries'}

\description{
  \code{timeSeries} creates a \code{"timeSeries"} object from scratch.
}


\usage{
timeSeries(data, charvec, units = NULL, format = NULL, zone = "", 
           FinCenter = "", recordIDs = data.frame(), title = NULL, 
           documentation = NULL, \dots)
}


\arguments{

  \item{data}{
    a \code{matrix} object or any objects which can be coerced to a matrix.
  }
  \item{charvec}{
    a character vector of dates and times or any objects which can
    be coerced to a \code{"timeDate"} object.
  }
  \item{units}{
    an optional character string, which allows to overwrite the 
    current column names of a \code{"timeSeries"} object. By default 
    \code{NULL} which means that the column names are selected
    automatically. 
  }     
  \item{format}{
    the format specification of the input character vector,
    a character string with the format in POSIX notation.
  }  
  \item{zone}{
    the time zone or financial center where the data were recorded.
  }  
  \item{FinCenter}{
    a character with the the location of the  financial center named 
    as "continent/city". 
  }
  \item{recordIDs}{
    for \code{timeSeries}, a data frame which can be used for record
    identification.
  }
  \item{title}{
    an optional title string, if not specified the input's data 
    name is deparsed.
  }
  \item{documentation}{
    optional documentation string, or a vector of character strings.
  }
  \item{\dots}{
    arguments passed to other methods.
  }
}


\value{

  \code{timeSeries} returns an S4 object of class \code{"timeSeries"}.
    
}


\details{

  \bold{Generation of Time Series Objects:}
  \cr
    
  We have defined a \code{"timeSeries"} class which is in many aspects
  similar to the S-Plus class with the same name, but has also some
  important differences. The class has seven Slots, the 'Data' slot
  which holds the time series data in matrix form, the 'position' slot
  which holds the time/date as a character vector, the 'format' and
  'FinCenter' slots which are the same as for the 'timeDate' object, the
  'units' slot which holds the column names of the data matrix, and a
  'title' and a 'documentation' slot which hold descriptive character
  strings.  Date and time is managed in the same way as for
  \code{timeDate} objects.

  \code{\link{as.timeSeries}} also creates \code{"timeSeries"} objects.
  \code{as.timeSeries(x)} is mostly equivalent to \code{timeSeries(x)}
  but the two functions have different methods. Beside that, the main
  difference between the two functions is that \code{as.timeSeries}
  doesn't accept additional arguments. The one argument call is
  naturally interpreted as \sQuote{convert to}, so
  \code{\link{as.timeSeries}} is more expressive and is recommended in
  that case.

  \code{"timeSeries"} methods are provided for many base R functions,
  including arithmetic operations, mathematical functions, \code{print},
  \code{summary}, and time series functions. Not all are explicitly
  documented, since they can just be used. 

}


% \note{
% 
%     These functions were written for Rmetrics users using R and Rmetrics
%     under Microsoft's Windows operating system where timze zones, 
%     daylight saving times and holiday calendars are insuffeciently 
%     supported. 
%     
% }

\seealso{
  \code{\link{as.timeSeries}},
  class \code{\linkS4class{timeSeries}},
  
}

\examples{
## Load Microsoft data -
   # Microsoft Data: 
   setRmetricsOptions(myFinCenter = "GMT")
   data(MSFT)
   head(MSFT)

## Create a 'timeSeries' object, the direct Way ...
   Close <- MSFT[, 5]
   head(Close)
   
## Create a 'timeSeries' object from scratch - 
   data <- as.matrix(MSFT[, 4])
   charvec <- rownames(MSFT)
   Close <- timeSeries(data, charvec, units = "Close")
   head(Close)
   c(start(Close), end(Close))

## Cut out April data from 2001 - 
   tsApril01 <- window(Close, "2001-04-01", "2001-04-30") 
   tsApril01

## Compute Continuous Returns - 
   returns(tsApril01)
   
## Compute Discrete Returns - 
   returns(tsApril01, type = "discrete")
   
## Compute Discrete Returns, Don't trim -
   returns(tsApril01, trim = FALSE)
   
## Compute Discrete Returns, Use Percentage Values - 
   tsRet <- returns(tsApril01, percentage = TRUE, trim = FALSE)
   tsRet
     
## Aggregate Weekly - 
   GoodFriday(2001)
   to <- timeSequence(from = "2001-04-11", length.out = 3, by = "week") 
   from <- to - 6*24*3600
   from
   to
   applySeries(tsRet, from, to, FUN = sum)

## Create large 'timeSeries' objects with different 'charvec' object classes - 
   # charvec is a 'timeDate' object
   head(timeSeries(1:1e6L, timeSequence(length.out = 1e6L, by = "sec")))
   head(timeSeries(1:1e6L, seq(Sys.timeDate(), length.out = 1e6L, by = "sec")))
   # 'charvec' is a 'POSIXt' object
   head(timeSeries(1:1e6L, seq(Sys.time(), length.out = 1e6L, by = "sec")))
   # 'charvec' is a 'numeric' object
   head(timeSeries(1:1e6L, 1:1e6L))
}


\keyword{chron}

