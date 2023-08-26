\name{finCenter}
\alias{finCenter}

\alias{finCenter<-}

\alias{finCenter,timeSeries-method}
\alias{finCenter<-,timeSeries-method}

\alias{getFinCenter}  
\alias{setFinCenter<-}


\title{Get and set Financial center of a 'timeSeries'}

\description{

  Get or assign a financial center to a \code{"timeSeries"} object.
  
}

\usage{
\S4method{finCenter}{timeSeries}(x)
\S4method{finCenter}{timeSeries}(x) <- value

getFinCenter(x)         
setFinCenter(x) <- value
}

\arguments{
  
  \item{x}{
    a \code{"timeSeries"} object.
  }
  \item{value}{
    a character with the the location of the financial center named 
    as \code{"continent/city"}.
  }
}

\seealso{
  \code{\link[timeDate]{listFinCenter}} and \code{\link[timeDate]{finCenter}}
  in package \code{"timeDate"}
}

\examples{
## An artificial 'timeSeries' Object - 
   tS <- dummyMonthlySeries()
   tS

## Print Financial Center -
   finCenter(tS)
   getFinCenter(tS)

## Assign New Financial Center - 
   finCenter(tS) <- "Zurich"
   tS
   setFinCenter(tS) <- "New_York"
   tS
}

\keyword{programming}
