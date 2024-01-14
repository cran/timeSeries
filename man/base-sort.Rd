\name{sort}  
\alias{sort}

\alias{sort.timeSeries}                 

\alias{is.unsorted}
\alias{is.unsorted.timeSeries}
\alias{is.unsorted,timeSeries-method}


\title{Sort a 'timeSeries' by time stamps}

\description{
  Sort a \code{"timeSeries"} object with respect to its time stamps.
}

\usage{
\method{sort}{timeSeries}(x, decreasing = FALSE, \dots)

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

  The method for \code{sort} sorts \code{x} either in increasing or
  decreasing time stamp order.

  The method for \code{is.unsorted} returns \code{TRUE} if the time
  stamps of \code{x} are not sorted in increasing order (including the
  case when they are sorted in decreasing order) and \code{FALSE}
  otherwise. \code{is.unsorted} may also return \code{NA} when there are
  \code{NA}s among the time stamps of \code{x}.

  All this is in line with the documented functionality of
  \code{base::is.unsorted}.

}

\value{
  for \code{sort}, a \code{"timeSeries"} object,

  for the \code{is.unsorted} method, \code{TRUE}, \code{FALSE}, or
  \code{NA}, as described in section \sQuote{Details}.

}

\note{

  If \code{is.unsorted} returns \code{NA} when there are \code{NA}s in
  the data but not in the time stamps use \code{library{timeSeries}} or
  call the function as \code{timeSeries::is.unsorted}. If you need more
  details, read the rest of this note.

  \code{base::is.unsorted} 'sees' the method for \code{"timeSeries"}
  objects when package timeSeries is loaded (whether or not it is
  attached). However, due to the way \code{base::is.unsorted} is
  implemented, it may give wrong answers when there are \code{NA}'s
  among the values of the time series. Developers of packages applying
  \code{is.unsorted} on timeSeries objects should import if from package
  timeSeries.

  The above feature is not a shortcoming of \code{base::is.unsorted} but
  a consequence of the fact that the timeSeries method is not consistent
  with its semantics. For example, it works on the time stamps, while
  \code{is.na} works on the data values.
  
}

\seealso{
  \code{\link[base]{is.unsorted}} for further details on the \code{NA} case
}
\examples{   
## a monthly calendar series
x <- daily2monthly(LPP2005REC[, 1:2])[3:14, ]
   
\dontshow{set.seed(1234)}
## resample the series with respect to the time stamps,
resampled <- sample(x)
## the time stamps are unordered
resampled
is.unsorted(resampled) # TRUE (i.e., not sorted)
   
## Now sort the series in decreasing time order
backward_in_time <- sort(resampled, , decreasing = TRUE)
## time stamps ordered in decreasing order
## but is.unordered requires increasing order:
backward_in_time
is.unsorted(backward_in_time) # still TRUE
   
## Is the reverted series ordered?
forward_in_time <- rev(backward_in_time)
forward_in_time
is.unsorted(forward_in_time) # FALSE (i.e., sorted)
}

\keyword{chron}
