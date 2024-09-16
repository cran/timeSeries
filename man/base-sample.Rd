\name{sample}                     
\alias{sample}

\alias{sample,timeSeries-method}
                                    
                                    
\title{Resample 'timeSeries' objects}

\description{

  Takes a sample of the specified size from the elements of a
  \code{"timeSeries"}.
    
}

\usage{
\S4method{sample}{timeSeries}(x, size, replace = FALSE, prob = NULL)
}

\arguments{
  \item{x}{
    an object from class \code{"timeSeries"}.
  }
  \item{size}{
    a non-negative integer giving the number of items to choose.
  }
  \item{replace}{
    sample with replacement if \code{TRUE}, otherwise without
    replacement. 
  }
  \item{prob}{
    
    a vector of probability weights for obtaining the elements of the
    vector being sampled.

  }
}
\details{

  The function takes a sample of size \code{size} from the elements of
  the time series with or without replacement depending on argument
  \code{replace}. The result is returned as a \code{"timeSeries"}
  object.
  
  For details about the arguments see the documentation of
  \code{base:sample}.
  
}

\value{
  an object from class \code{"timeSeries"}
}

\seealso{
  \code{\link[base]{sample}} (\code{sample} in base \R),
  
  \code{\link[timeDate]{sample}} (the \code{"timeDate"} method)
}

\examples{   
## Monthly Calendar Series -    
   x <- daily2monthly(LPP2005REC[, 1:2])[3:14, ]
   
\dontshow{set.seed(1234)}
## Resample the Series with respect to the time stamps -    
   resampled <- sample(x)
   resampled
   is.unsorted(resampled)
}

\keyword{chron}
