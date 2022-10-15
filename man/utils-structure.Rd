\name{str-methods}
\alias{str}

\alias{str,timeSeries-method}


\title{timeSeries object structure}


\description{
    
  Compactly display the structure of a \code{"timeSeries"} object.        
    
}


\usage{
\S4method{str}{timeSeries}(object, \dots)
}


\arguments{

    \item{object}{  
        an object of class \code{timeSeries}.
        }
    \item{\dots}{
        arguments passed to other methods.
        }

}


\value{

   a str report for an object of class \code{timeSeries}.
        
}


\examples{
## Load Microsoft Data Set -
   data(MSFT)
   X <- MSFT[1:12, 1:4]
   colnames(X) <- abbreviate(colnames(X), 4)
    
## Display Structure - 
   str(X)
}


\keyword{chron}

