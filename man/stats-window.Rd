\name{window}                     
\alias{window}

\alias{window,timeSeries-method}
\alias{window.timeSeries}

\alias{cut,timeSeries-method}
\alias{cut.timeSeries}

                                    
\title{Methods for 'window' in package 'timeSeries'}

\description{

  Extract a part from a \code{"timeSeries"} object.
    
}

\usage{
\S4method{window}{timeSeries}(x, start, end, \dots)

\S4method{cut}{timeSeries}(x, from, to, \dots)
}

\arguments{
  
  \item{x}{
    an object of class \code{"timeSeries"}.
  }
  \item{from, to}{
    starting date and end date, \code{to} must be after \code{from}.
  }
  \item{start, end}{
    starting date and end date, \code{end} must be after \code{start}.
  }
  \item{\dots}{
    arguments passed to other methods.
  }
}


\details{

  \code{window} Windows a piece from a \code{"timeSeries"} object,

  \code{cut} is deprecated and will be removed in the near future.
}

\examples{
## Load LPP Benchmark Returns -
   x <- LPP2005REC[, 7:9]
   range(time(x))
  
## Extract Data for January 2006 - 
   window(x, "2006-01-01", "2006-01-31")
}


\keyword{chron}

