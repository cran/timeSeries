\name{rev}     

\alias{rev.timeSeries}
                                                 
                                    
\title{Reverse a 'timeSeries'}

\description{

  Reverses an uni- or multivariate \code{"timeSeries"} object.
    
}

\usage{
\method{rev}{timeSeries}(x)
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
## Create Dummy "timeSeries"
tS <- dummyMonthlySeries()
   
## reverse series
rev(tS)
}

\keyword{chron}
