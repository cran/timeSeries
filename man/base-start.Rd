\name{start}  
\alias{start}

\alias{end}

\alias{start,timeSeries-method}
\alias{start.timeSeries}

\alias{end,timeSeries-method}
\alias{end.timeSeries}                   
                                    
                                    
\title{Start and end of a 'timeSeries'}

\description{

  Returns start or end time stamp of a \code{"timeSeries"} object.
    
}

\usage{
\S4method{start}{timeSeries}(x, \dots)

\S4method{end}{timeSeries}(x, \dots)
}

\arguments{

  \item{x}{
    an uni- or multivariate \code{"timeSeries"} object.
  }
  \item{\dots}{
    optional arguments passed to other methods.
  }
}

\value{
  a \code{"timeSeries"} object
}

\examples{   
\dontshow{set.seed(1234)}
## Create a dummy \code{"timeSeries"} -    
   tS <- dummyMonthlySeries()[, 1]
   tS
   
## Return start and end time stamp -
   c(start(tS), end(tS))
   range(time(tS))
}

\keyword{chron}
