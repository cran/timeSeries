\name{diff}                     
\alias{diff}

\alias{diff,timeSeries-method}
                                    
                                    
\title{Difference a 'timeSeries' object}

\description{
  Difference a \code{"timeSeries"} object.
}

\usage{
\S4method{diff}{timeSeries}(x, lag = 1, diff = 1, trim = FALSE, pad = NA, \dots)
}

\arguments{
  
  \item{x}{
    an object of class \code{"timeSeries"}.
  }
   
  \item{lag}{
    an integer indicating which lag to use.
  }
  \item{diff}{
    an integer indicating the order of the difference.
  }
  \item{trim}{

    a logical flag. Should \code{NA}s at the beginning of the series be
    removed?

  }
  \item{pad}{

    a numeric value with which \code{NA}s should be replaced at the
    beginning of the series.

  }
  
  \item{\dots}{
    currently not used.
  }
}

%\details{
%}
 
\value{
  the differenced \code{"timeSeries"} object
}

\seealso{
  \code{\link[base]{diff}} for \verb{base::diff},
  \code{\link{lag}}
}

\examples{
## Load Microsoft Data Set -
   x <- MSFT[1:12, ]
   x

## Compute Differences -
   diff(x)   
   
## Trimmed Differences -
   diff(x, trim = TRUE) 

## Padded Differences -
   diff(x, trim = FALSE, pad = 0) 
}

\keyword{chron}
\keyword{ts}
