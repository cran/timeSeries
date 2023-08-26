\name{str-methods}
\alias{str-methods}
\alias{str}
\alias{str,timeSeries-method}


\title{Display the structure of 'timeSeries' objects}

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

  \code{NULL}, invisibly. The function is called for its side effect of
  printing a compact representation of the structure of the
  \code{"timeSeries"} object.
        
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
