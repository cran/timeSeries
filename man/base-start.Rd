\name{start}  
\alias{start}
\alias{start.timeSeries}

\alias{end}
\alias{end.timeSeries}                   

%\alias{start,timeSeries-method}

%\alias{end,timeSeries-method}

                                    
\title{Start and end of a 'timeSeries'}

\description{

  Returns start or end time stamp of a \code{"timeSeries"} object.
    
}

\usage{
\method{start}{timeSeries}(x, \dots)

\method{end}{timeSeries}(x, \dots)
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
## Create a dummy \code{"timeSeries"}
tS <- dummyMonthlySeries()[, 1]
tS
   
## Return start and end time stamp
c(start(tS), end(tS))
range(time(tS))
}

\keyword{chron}
