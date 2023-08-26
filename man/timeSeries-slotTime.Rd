\name{time}


\title{Get and set time stamps of a 'timeSeries'}


\alias{time}
\alias{time<-}

\alias{time,timeSeries-method}
\alias{time.timeSeries}

\alias{time<-.timeSeries}
\alias{time<-.default}

\alias{getTime}  
\alias{setTime<-}


\description{
    
    Functions and methods extracting and modifying positions 
    of 'timeSeries' objects.
        
}


\usage{
\S4method{time}{timeSeries}(x, \dots)
\method{time}{timeSeries}(x) <- value

getTime(x)         
setTime(x) <- value
}


\arguments{

  \item{value}{
    a valid value for the time component of \code{x}.
  }
  \item{x}{
    an object of class \code{timeSeries}.
  } 
  \item{\dots}{
    optional arguments passed to other methods.
  }
}

\details{
  
  \code{time} and \code{time<-} are generic functions with methods for
  class \code{"timeSeries"}. They get and set the time component of the
  object. 

  \code{getTime} and \code{setTime} are non-generic alternatives are
  non-generic wrappers of \code{time} and \code{time<-}, respectively.

  There is another generic function \code{time<-} defined in package
  \pkg{zoo}. When that package is loaded its \code{time<-} gets the
  \code{"timeSeries"} method. Also, if \code{"time<-"} is called with an
  object from class other than \code{"timeSeries"}, the call is
  dispatched to \code{"zoo:time<-"} to apply a suitable method.
  
}

\value{
  for \code{time} and \code{getTime}, a \code{"timeDate"} object,

  for \code{time<-} and and \code{setTime}, the modified
  \code{"timeSeries"} object.
           
}

\examples{   
## Create Dummy 'timeSeries' -      
   X <- timeSeries(matrix(rnorm(24), 12), timeCalendar())
   
## Return Series Positions -
   getTime(X)
   time(X)  
  
## Add / Subtract one Day from X
   setTime(X) <- time(X) - 24*3600 # sec
   X
   time(X) <- time(X) + 24*3600 # sec
   X
}

\keyword{chron}
