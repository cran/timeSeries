\name{rev}     

\alias{rev,timeSeries-method}
\alias{rev.timeSeries}
                                                 
                                    
\title{Reverse a 'timeSeries'}

\description{

  Reverses an uni- or multivariate \code{"timeSeries"} object.
    
}

\usage{
\S4method{rev}{timeSeries}(x)
}

\arguments{
  \item{x}{
    an uni- or multivariate \code{"timeSeries"} object.
  } 
}

\value{
  a  \code{"timeSeries"} object
}

\examples{
\dontshow{set.seed(1234)}
## Create Dummy "timeSeries"  -
   tS <- dummyMonthlySeries()
   
## Reverse Series -
   rev(tS)
}

\keyword{chron}
