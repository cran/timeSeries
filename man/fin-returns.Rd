\name{returns}
\alias{returns}

\alias{returns,ANY-method}
\alias{returns,timeSeries-method}

\alias{returns0}

\alias{returnSeries}
\alias{getReturns}


\title{Financial returns}

\description{
    
  Compute financial returns from prices or indexes.
       
}

\usage{
returns(x, \dots)
returns0(x, \dots)

\S4method{returns}{ANY}(x, method = c("continuous", "discrete", 
    "compound", "simple"), percentage = FALSE, \dots)
\S4method{returns}{timeSeries}(x, method = c("continuous", "discrete", 
    "compound", "simple"), percentage = FALSE, na.rm = TRUE, 
    trim = TRUE, \dots) 
}

\arguments{

  \item{x}{
    an object of class \code{timeSeries}.
  }
  \item{method}{

    a character string. Which method should be used to compute the
    returns, one of "continuous", "discrete", or "compound", "simple".
    The second pair of methods is a synonym for the first two methods.

  }    
  \item{percentage}{
    
    a logical value. By default \code{FALSE}, if \code{TRUE} the series
    will be expressed in percentage changes.

  }
  \item{na.rm}{
    a logical value. Should NAs be removed? By default \code{TRUE}.
  }    
  \item{trim}{
    a logical value. Should the time series be trimmed? By Default 
    \code{TRUE}.
  }    
  \item{\dots}{
    arguments to be passed.
  }
        
}

\value{

  an object of class \code{timeSeries}.
    
  \code{returns0} returns an untrimmed series with the first
  row of returns set to zero(s).
        
}

\note{

  The functions \code{returnSeries} and \code{getReturns} will be
  removed in the near future. They are synonyms for the function
  \code{returns} and their use was discouraged for many years. Just use
  \code{returns}.

  The function \code{returnSeries} is no longer
  exported. \code{getReturns} is exported only because we are waiting
  for a package on CRAN to be updated.

}

\seealso{
  %\code{\link{returns}},
  \code{\link{cumulated}},
  \code{\link{drawdowns}},
  \code{\link{splits}},
  \code{\link{spreads}},
  \code{\link{midquotes}},
  \code{\link{index2wealth}}
}

\examples{
## Load Microsoft Data - 
   setRmetricsOptions(myFinCenter = "GMT")
   data(MSFT)
   X = MSFT[1:10, 1:4]
   X

## Continuous Returns - 
   returns(X)
   returns0(X)
  
## Discrete Returns:
   returns(X, method = "discrete")
   
## Don't trim:
   returns(X, trim = FALSE)
   
## Use Percentage Values:
   returns(X, percentage = TRUE, trim = FALSE)
}

\keyword{chron}
