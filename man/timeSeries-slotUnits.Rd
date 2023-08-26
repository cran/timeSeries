\name{units}

\alias{getUnits}
\alias{getUnits.default}

\alias{setUnits<-}


\title{Get and set unit names of a 'timeSeries'}

\description{

  Gets and sets the column names of a \code{"timeSeries"} object. The
  column names are also called units or unit names.
    
}

\usage{
getUnits(x)         
setUnits(x) <- value
}

\arguments{

  \item{x}{
    a \code{"timeSeries"} object.
  }
  \item{value}{
    a character vector of unit names.
  }
}

\seealso{
  \code{\link{timeSeries}}
}

\examples{
## A Dummy 'timeSeries' Object
   tS <- dummyMonthlySeries()
   tS

## Get the Units - 
   getUnits(tS)

## Assign New Units to the Series - 
   setUnits(tS) <- c("A", "B")
   head(tS)
}

\keyword{programming}
