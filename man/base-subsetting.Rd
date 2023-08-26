\name{TimeSeriesSubsettings}
\alias{TimeSeriesSubsettings}

\alias{$,timeSeries-method}

\alias{[,timeSeries,ANY,index_timeSeries-method}
\alias{[,timeSeries,character,character-method}
\alias{[,timeSeries,character,index_timeSeries-method}
\alias{[,timeSeries,character,missing-method}
\alias{[,timeSeries,index_timeSeries,character-method}
\alias{[,timeSeries,index_timeSeries,index_timeSeries-method}
\alias{[,timeSeries,index_timeSeries,missing-method}
\alias{[,timeSeries,matrix,missing-method}
\alias{[,timeSeries,missing,character-method}
\alias{[,timeSeries,missing,index_timeSeries-method}
\alias{[,timeSeries,missing,missing-method}
\alias{[,timeSeries,time_timeSeries,ANY-method}
\alias{[,timeSeries,time_timeSeries,character-method}
\alias{[,timeSeries,time_timeSeries,index_timeSeries-method}
\alias{[,timeSeries,time_timeSeries,missing-method}
\alias{[,timeSeries,timeDate,character-method}
\alias{[,timeSeries,timeDate,index_timeSeries-method}
\alias{[,timeSeries,timeDate,missing-method}
\alias{[,timeSeries,timeSeries,index_timeSeries-method}
\alias{[,timeSeries,timeSeries,missing-method}


\alias{$<-,timeSeries,ANY-method}
\alias{$<-,timeSeries,factor-method}
\alias{$<-,timeSeries,numeric-method}

\alias{[<-,timeSeries,character,ANY-method}
\alias{[<-,timeSeries,character,missing-method}
\alias{[<-,timeSeries,timeDate,ANY-method}
\alias{[<-,timeSeries,timeDate,missing-method}


\alias{head}
\alias{head,timeSeries-method}
\alias{head.timeSeries}

\alias{tail}
\alias{tail,timeSeries-method}
\alias{tail.timeSeries}

\alias{outlier}
\alias{outlier,timeSeries-method}
\alias{outlier,ANY-method}


\title{Subsetting time series}

\description{
  
  Objects from class \code{"timeSeries"} can be subsetted in different
  ways.  Methods are defined for the subsetting operators \code{"$"},
  \code{"["} and their assignment versions, as well as for some related
  functions from base \R. A function to drop or extract outliers is also
  described here.
  
}

% \method{[}{timeSeries}(x, i, j, drop)
% \method{[}{timeSeries}(x, i, j) <- value
\usage{
\S4method{head}{timeSeries}(x, n = 6, recordIDs = FALSE, \dots)
\S4method{tail}{timeSeries}(x, n = 6, recordIDs = FALSE, \dots)

outlier(x, sd = 5, complement = TRUE, ...)
}


\arguments{
  
  \item{x}{
    an object of class \code{timeSeries}.
  }
  % \item{from, to}{
  %    starting date and end date, \code{to} must be after \code{from}.
  %    }
  % \item{start, end}{
  %    starting date and end date, \code{end} must be after \code{start}.
  %    }
  % \item{i, j}{
  %    ["["] - \cr 
  %    index arguments used for subsettings.
  % }       
  \item{n}{
    an integer specifying the number of lines to be returned. 
    By default \code{n=6}.
  }
  \item{recordIDs}{
    a logical value. Should the \code{recordIDs} be returned together
    with the data matrix and time series positions?
  }
  \item{sd}{
    
    a numeric value of standard deviations, e.g. 10 means that values
    larger or smaller than ten times the standard deviation will be
    removed from the series.

  }
  \item{complement}{
    
    a logical flag. If \code{TRUE}, the default, return the series free
    of outliers. If \code{FALSE}, return the outliers series.

  }
  % \item{value}{
  %   a numeric value to use as a replacement. It will be repeated a
  %   whole number of times if necessary. 
  % }
  \item{\dots}{
    arguments passed to other methods.
  }
}

\details{

  The \code{"timeSeries"} methods for the subsetting operators \code{"$"},
  \code{"["} and their assignment versions, as well as for the functions
  \code{head} and \code{tail} are meant to do what the user expects.

  \strong{TODO:} Further details are needed here, despite the above
  paragraph.

  \code{outlier} drops the outliers if \code{complement = TRUE} and
  returns only them if \code{complement = FALSE}.

  All functions described here return \code{"timeSeries"} objects.

  See also \code{\link{window}} which extracts the sub-series between
  two datetimes.
  
}
        
\value{
  All functions return an object of class \code{"timeSeries"}.
 
}

\seealso{
  \code{\link{window}}
}
\examples{
## Create an Artificial 'timeSeries' Object - 
   setRmetricsOptions(myFinCenter = "GMT")
   charvec <- timeCalendar()
   set.seed(4711)
   data <- matrix(exp(cumsum(rnorm(12, sd = 0.1))))
   tS <- timeSeries(data, charvec, units = "tS")
   tS
   
## Subset Series by Counts "[" - 
   tS[1:3, ]
       
## Subset the Head of the Series - 
   head(tS, 6)
}


\keyword{chron}

