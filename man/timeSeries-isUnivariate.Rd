\name{isUnivariate}
\alias{isUnivariate}
\alias{isMultivariate}


\title{Checks if a time series is univariate}


\description{
    
    Checks if a time series object or any other
    rectangular object is univariate or multivariate.       
}


\usage{
isUnivariate(x)
isMultivariate(x)
}


\arguments{
   
    \item{x}{ 
        an object of class \code{"timeSeries"} or any other rectangular 
        object.
        }

}


\details{

    A rectangular object \code{x} is considered to be univariate if 
    the function \code{NCOL(x)} returns one, and is considered to be
    multivariate if \code{NCOL(x)} returns a value bigger than one.

}

\value{

  a logical value
        
}


\examples{
## Load Microsoft Data - 
   setRmetricsOptions(myFinCenter = "GMT")
   data(MSFT)
   Open = MSFT[, "Open"]
  
## Is the 'timeSeries' Univariate - 
   isUnivariate(MSFT)
   isUnivariate(Open)

## Is the 'timeSeries' Multivariate -   
   isMultivariate(MSFT)
   isMultivariate(Open)
}


\keyword{chron}

