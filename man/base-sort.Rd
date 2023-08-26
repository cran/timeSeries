\name{sort}  
\alias{sort}

\alias{sort,timeSeries-method}
\alias{sort.timeSeries}                 
                                    
\alias{is.unsorted}
\alias{is.unsorted,timeSeries-method}


\title{Sort a 'timeSeries' by time stamps}

\description{
  Sort a \code{"timeSeries"} object with respect to its time stamps.
}

\usage{
\S4method{sort}{timeSeries}(x, decreasing = FALSE, \dots)

\S4method{is.unsorted}{timeSeries}(x, na.rm = FALSE, strictly = FALSE)
}

\arguments{

  \item{x}{
    a \code{"timeSeries"} object.
  }
  \item{decreasing}{
    a logical flag. Should we sort in increasing or decreasing order?
    By default \code{FALSE}.
  }
  \item{na.rm}{
    a logical value, should missing values be removed?
  }
  \item{strictly}{
    logical indicating if the check should be for strictly increasing
    values.
  }
  \item{\dots}{
    optional arguments passed to other methods.
  }
}

\details{

  Sorts a time series either in increasing or decreasing time stamp
  order. Internally the function \code{order} from R's base package is
  used. \code{order} generates a permutation which rearranges the time
  stamps in ascending or descending order.
    
  To find out if the series is unsorted, use \code{is.unsorted}.

}

\value{
  for \code{sort}, a \code{"timeSeries"} object,

  for the \code{is.unsorted} method, \code{TRUE} or \code{FALSE}
}

%\seealso{
%  \code{\link{}}
%}
\examples{   
## Monthly Calendar Series -    
   x <- daily2monthly(LPP2005REC[, 1:2])[3:14, ]
   
\dontshow{set.seed(1234)}
## Resample the Series with respect to the time stamps -    
   resampled <- sample(x)
   resampled
   is.unsorted(resampled)
   
## Now sort the serie in decreasing time order - 
   sorted <- sort(resampled, , decreasing = TRUE)
   sorted
   is.unsorted(sorted)
   
## Is the reverted series ordered? - 
   reverted <- rev(sorted)
   reverted
   is.unsorted(reverted)
}

\keyword{chron}
