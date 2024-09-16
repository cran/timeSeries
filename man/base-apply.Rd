\name{apply}
\alias{apply}

\alias{apply,timeSeries-method}

\alias{fapply}
\alias{applySeries}

\alias{rollDailySeries}


\title{Apply functions over time windows}

\description{
    
  Applies a function to a \code{"timeSeries"} object over regular or
  irregular time windows, possibly overlapping.
    
}

\usage{
\S4method{apply}{timeSeries}(X, MARGIN, FUN, \dots, simplify = TRUE)

fapply(x, from, to, FUN, \dots)

applySeries(x, from = NULL, to = NULL, by = c("monthly", "quarterly"), 
            FUN = colMeans, units = NULL, format = x@format, 
            zone = x@FinCenter, FinCenter = x@FinCenter, 
            recordIDs = data.frame(), title = x@title, 
            documentation = x@documentation, \dots)

rollDailySeries(x, period = "7d", FUN, \dots)
}

\arguments{
  
  \item{x,X}{
    an object of class \code{timeSeries}.
  } 
  \item{MARGIN}{
    a vector giving the subscripts which the function will be
    applied over, see base R's \code{\link[base]{apply}}.
  }
  \item{FUN}{
    the function to be applied. For the function \code{applySeries} the
    default setting is \code{FUN = colMeans}.
  }
  \item{simplify}{
    simplify the result?
  }
  \item{from, to}{
    
    starting date and end date as \code{"timeDate"} objects. Note,
    \code{to} must be time ordered after \code{from}.  If \code{from}
    and \code{to} are missing in function \code{fapply} they are set by
    default to \code{from=start(x)}, and \code{to=end(x)}.

  }
  \item{by}{

    a character value either \code{"monthly"} or \code{"quarterly"} used
    in the function \code{applySeries}. The default value is
    \code{"monthly"}. Only operative when both arguments \code{from} and
    \code{to} have their default values \code{NULL}.  In this case the
    function \code{FUN} will be applied to monthly or quarterly periods.

  }
  \item{units}{

    an optional character string, which allows to overwrite the current
    column names of a \code{timeSeries} object. By default \code{NULL}
    which means that the column names are selected automatically.

  }  
  \item{format}{

    the format specification of the input character vector in POSIX
    notation.

  }     
  \item{zone}{
    the time zone or financial center where the data were recorded.
  }
  \item{FinCenter}{
    a character value with the the location of the  financial center 
    named as "continent/city", or "city". 
  }
  \item{recordIDs}{

    a data frame which can be used for record identification
    information. Note, this is not yet handled by the apply functions,
    an empty data.frame will be returned.

  } 
  \item{title}{

    an optional title string, if not specified the input's data name is
    deparsed.

  }
  \item{documentation}{
    optional documentation string, or a vector of character strings.
  }   
  \item{period}{

    a character string specifying the rollling period composed by the
    length of the period and its unit, e.g. \code{"7d"} represents one
    week.

  }
  \item{\dots}{
    arguments passed to other methods.
  }
}

\details{
  
  The \code{"timeSeries"} method for \code{apply} extracts the core data
  (a matrix) from \code{X} and calls \code{apply}, passing on all the
  remaining arguments. If the result is suitable, it converts it to
  \code{"timeSeries"}, otherwise returns it as is. \sQuote{Suitable}
  here means that it is a matrix or a vector (which is converted to a
  matrix) and the number of observations is the same as \code{X}.

  Like \code{apply} applies a function to the margins of an array, the
  function \code{fapply} applies a function to the time stamps or signal
  counts of a financial (therefore the \dQuote{f} in front of the
  function name) time series of class \code{"timeSeries"}.

  
  \code{applySeries} takes a \code{"timeSeries"} object as input and
  applies \code{FUN} to windows of \code{x}.  The windows are specified
  by \code{from} and \code{to}, which need to have the same length. Then
  \code{from[i], to[i]} specifies the \code{i}-th window.  If
  \code{time(x)} is a \code{"timeDate"} object, then \code{from} and
  \code{to} are converted to \code{"timeDate"} (if they are not already
  such objects), otherwise they are converted to integers.

  An alternative way to specify the window(s) on which
  \code{applySeries} operates is with argument \code{by}. It is used
  only if \code{from} and \code{to} are missing or \code{NULL}. \code{by
  = "monthly"} or \code{by = "quarterly"} applies \code{FUN} to the data
  for each year-month or year-quarter, respectively. By year-month we
  mean that there are separate windows for the months in different
  years.

  The resulting time stamps are the time stamps of the \code{to}
  vector. The periods can be regular or irregular, and they can even
  overlap.

  If \code{from = start(x)} and \code{to = end(x)}, then the function
  behaves like \code{apply} on the column margin.

  
  \code{fapply} is the same as \code{applySeries} (in fact, the former
  calls the latter), except that the defaults for \code{from} and
  \code{to} are \code{start(x)} and \code{end(x)}, respectively. (GNB:
  in addition, \code{fapply} throws error if \code{x} is a
  \sQuote{signal series}.)

  \code{rollDailySeries} rolls a daily 'timeSeries' on a given period.

}

\value{

  for \code{rollDailySeries}, an object of class \code{"timeSeries"} with
  rolling values, computed from the function \code{FUN}.

}

\examples{
## Percentual Returns of Swiss Bond Index and Performance Index - 
   LPP <- 100 * LPP2005REC[, c("SBI", "SPI")]
   head(LPP, 20)
   
## Aggregate Quarterly Returns -
   applySeries(LPP, by = "quarterly", FUN = colSums)
   
## Aggregate Quarterly every last Friday in Quarter -
   oneDay <- 24*3600
   from <- unique(timeFirstDayInQuarter(time(LPP))) - oneDay
   from <- timeLastNdayInMonth(from, nday = 5)
   to <- unique(timeLastDayInQuarter(time(LPP)))
   to <- timeLastNdayInMonth(to, nday = 5)
   data.frame(from = as.character(from), to = as.character(to))

   applySeries(LPP, from, to, FUN = colSums)
## Alternative Use - 
   fapply(LPP, from, to, FUN = colSums)
   
## Count Trading Days per Month - 
   colCounts <- function(x) rep(NROW(x), times = NCOL(x))
   applySeries(LPP, FUN = colCounts, by = "monthly")


## TODO: examples for rollDailySeries()
}

\keyword{chron}
\keyword{ts}
