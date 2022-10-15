\name{t}

\alias{t,timeSeries-method}

\title{Transpose 'timeSeries' objects}




\description{
     Returns the transpose of a 'timeSeries' object.
}


\usage{
\S4method{t}{timeSeries}(x)
}


\arguments{

    \item{x}{
    a 'timeSeries' object.
    }
    
}


\value{

  a matrix object
  
}


\examples{
## Dummy 'timeSeries' with NAs entries
   data <- matrix(1:24, ncol = 2)
   s <- timeSeries(data, timeCalendar())
   s
   
## Transpose 'timeSeries' -
   t(s)
}


\keyword{chron}

