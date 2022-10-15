\name{comment}

\alias{comment,timeSeries-method}
\alias{comment<-,timeSeries-method}

\title{Get and set comments for 'timeSeries' objects}


\description{
  Get or assign new comment to a \code{timeSeries} object.
}


\usage{
\S4method{comment}{timeSeries}(x)
\S4method{comment}{timeSeries}(x) <- value

}


\arguments{
  
  \item{x}{
    a \code{timeSeries} object.
  }
  \item{value}{
    a character vector, the comment.
  }
  
}

\details{

  Objects from class \code{"timeSeries"} have a slot for documentation.
  These functions get and change its contents.

}

\examples{
## Get description from a 'timeSeries' -
   comment(LPP2005REC)
   
## Add User to comment -
   comment(LPP2005REC) <- paste(comment(LPP2005REC), "by User Rmetrics")
   comment(LPP2005REC)
}


\keyword{chron}

