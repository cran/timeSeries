\name{window}                     
\alias{window}

\alias{window,timeSeries-method}
\alias{window.timeSeries}

                                    
\title{Methods for 'window' in package 'timeSeries'}

\description{

  Extract a part from a \code{"timeSeries"} object.
    
}

\usage{
\S4method{window}{timeSeries}(x, start, end, \dots)
}

\arguments{
  
  \item{x}{
    an object of class \code{"timeSeries"}.
  }
  \item{start, end}{
    starting date and end date, \code{end} must be after \code{start}.
  }
  \item{\dots}{
    arguments passed to other methods.
  }
}


\details{

  \code{window} extracts the subset of the \code{"timeSeries"} object
  \code{x} observed between the times \code{start} and \code{end}.

}

\seealso{
  \code{\link[=head,timeSeries-method]{head}},
  \code{\link{outlier}}
}

\examples{
## Load LPP Benchmark Returns -
   x <- LPP2005REC[, 7:9]
   range(time(x))
  
## Extract Data for January 2006 - 
   window(x, "2006-01-01", "2006-01-31")
}

\keyword{chron}
