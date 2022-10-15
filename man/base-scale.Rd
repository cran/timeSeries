\name{scale}                     
\alias{scale}                                    

\alias{scale,timeSeries-method}


\title{Center and scale 'timeSeries' objects}


\description{

  Center and scale a \code{"timeSeries"} object.
    
}

\usage{
\S4method{scale}{timeSeries}(x, center = TRUE, scale = TRUE)
}

\arguments{
  \item{x}{
    an object from class \code{"timeSeries"}.
  }
  \item{center, scale}{
    a numeric vector or a logical value, see \sQuote{Details}.
  }
}
\details{

    \code{scale} centers and/or scales the columns of a 
    \code{"timeSeries"} object.
    
    The value of \code{center} determines how column centering is performed. 
    If \code{center} is a numeric vector with length equal to the number 
    of columns of \code{x}, then each column of \code{x} has the corresponding 
    value from \code{center} subtracted from it. If \code{center} is TRUE 
    then centering is done by subtracting the column means (omitting NAs) 
    of \code{x} from their corresponding columns, and if \code{center} 
    is FALSE, no centering is done.

    The value of \code{scale} determines how column scaling is performed 
    (after centering). If \code{scale} is a numeric vector with length equal 
    to  the number of columns of \code{x}, then each column of \code{x} is 
    divided by the corresponding value from \code{scale}. If \code{scale} 
    is TRUE then scaling is done by dividing the (centered) columns of 
    \code{x} by their standard deviations if \code{center} is TRUE, and 
    the root mean square otherwise. If \code{scale} is FALSE, no 
    scaling is done.
}

 
\value{

  a centered and/or scaled \code{"timeSeries"} object
    
}


\examples{   
## Load Series:
   x <- 100* LPP2005REC[, c("SBI", "SPI")]
   
## Scale and Center -
   X <- scale(x)
   hist(X[, 1], prob=TRUE)
   s <- seq(-3, 3, length=201)
   lines(s, dnorm(s), col="red")  
}


\keyword{chron}
\keyword{ts}
