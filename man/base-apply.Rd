\name{apply}


\title{Apply functions over time series periods}

\alias{fapply}
\alias{applySeries}
\alias{apply,timeSeries-method}


\description{
    
    Applies a function to a \code{"timeSeries"} object over time periods 
    of arbitrary positons and lengths.          
    
}


\usage{
\S4method{apply}{timeSeries}(X, MARGIN, FUN, \dots, simplify = TRUE)

fapply(x, from, to, FUN, \dots)

applySeries(x, from = NULL, to = NULL, by = c("monthly", "quarterly"), 
            FUN = colMeans, units = NULL, format = x@format, 
            zone = x@FinCenter, FinCenter = x@FinCenter, 
            recordIDs = data.frame(), title = x@title, 
            documentation = x@documentation, \dots)
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
  \item{\dots}{
    arguments passed to other methods.
  }
}


\details{
  
  The \code{"timeSeries"} method for \code{apply} extracts the core data
  (a matrix) from \code{X} and calls \code{apply} on passing on all the
  remaining arguments. If the result is suitable, it converts it to
  \code{"timeSeries"}, otherwise returns it as is. \sQuote{Suitable}
  here means that it is a matrix or a vector (which is converted to a
  matrix) and the number of observations is the same as \code{X}. 

  Like \code{apply} applies a function to the margins of an array, the
  function \code{fapply} applies a function to the time stamps or signal
  counts of a financial (therefore the \dQuote{f} in front of the
  function name) time series of class \code{"timeSeries"}.
    
  The function \code{fapply} takes a \code{"timeSeries"} object as input
  and if \code{from} and \code{to} are missing, they are set to the
  start and end time stamps of the series as default values. The
  function then behaves like \code{apply} on the column margin.
    
  Note, the function \code{fapply} can be used repetitively in the
  following sense: If \code{from} and \code{to} are two \code{"timeDate"}
  vectors of equal length then for each period spanned by the elements
  of the two vectors the function \code{FUN} will be applied to each
  period.  The resulting time stamps are the time stamps of the
  \code{to} vector. Note, the periods can be regular or irregelar, and
  they can even overlap.
    
  The function \code{fapply} calls the more general function
  \code{applySeries} which also offers, to create automatically monthly
  and quarterly periods.
    
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
   
## Count Trading Days per Month - 
   colCounts <- function(x) rep(NROW(x), times = NCOL(x))
   applySeries(LPP, FUN = colCounts, by = "monthly")
   
## Alternative Use - 
   fapply(LPP, from, to, FUN = colSums)
}


\keyword{chron}

