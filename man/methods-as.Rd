\name{as}
\alias{as}

\alias{as.timeSeries}
\alias{as.timeSeries.default}
\alias{as.timeSeries.data.frame} 
\alias{as.timeSeries.character}
\alias{as.timeSeries.ts} 
\alias{as.timeSeries.zoo} 

\alias{as.matrix,timeSeries-method}
\alias{as.data.frame,timeSeries-method}
\alias{as.list,timeSeries-method}
\alias{as.ts,timeSeries-method}

% S3 versions of the above
\alias{as.matrix.timeSeries}
\alias{as.data.frame.timeSeries}
\alias{as.list.timeSeries}
\alias{as.ts.timeSeries}

% for as(x, "class") where x is the 1st element of the signature
\alias{coerce,ANY,timeSeries-method}
\alias{coerce,data.frame,timeSeries-method}
\alias{coerce,character,timeSeries-method}
\alias{coerce,ts,timeSeries-method}

% for as(x, "class") where x is timeSeries
\alias{coerce,timeSeries,matrix-method}
\alias{coerce,timeSeries,data.frame-method}
\alias{coerce,timeSeries,list-method}
\alias{coerce,timeSeries,ts-method}
\alias{coerce,timeSeries,tse-method}



\title{Coercion and transformation to/from class 'timeSeries'}

\description{
  
  Functions and methods dealing with the coercion between \code{"timeSeries"}
  and other classes.

}


\usage{
## convert to 'timeSeries'
as.timeSeries(x, \dots)

## convert from 'timeSeries' to other classes
\S4method{as.matrix}{timeSeries}(x, \dots)
\S4method{as.ts}{timeSeries}(x, \dots)
\S4method{as.data.frame}{timeSeries}(x, row.names = NULL, optional = FALSE, \dots)
\S4method{as.list}{timeSeries}(x, \dots)
}


\arguments{

    \item{x}{ 
        the object to be converted, see Section \sQuote{Details} for the
        special case when \code{class(x)} is \code{"character"}.
    } 
    \item{row.names}{
        \code{NULL} or a character vector giving the row names for the
        data frame. Missing values are not allowed.
    }
    \item{optional}{
        a logical value. If \code{TRUE}, setting row names and converting
        column names (to syntactic names) is optional.
    }
    \item{\dots}{
        arguments passed to other methods.
    }
  
}

\details{
    
  Functions to create \code{"timeSeries"} objects from other objects and
  to convert \code{"timeSeries"} objects to other classes.

  \code{as.timeSeries} is a generic function to convert an object to
  \code{"timeSeries"}.  There are specialised methods for the following
  classes: \code{"ts"}, \code{"data.frame"}, \code{"character"}, and
  \code{"zoo"}. The default method is equivalent to calling
  \code{"timeSeries()"}, so \code{x} can be of any type that
  \code{"timeSeries()"} accepts.
  
  The \code{character} method of \code{as.timeSeries} is special, in
  that its contents are parsed and evaluated, then \code{as.timeSeries}
  is called on the returned value (passing also the \code{"..."}
  arguments. Care is needed to avoid infinite recursion here since
  currently the code doesn't guard against it.
  
}

\value{
    
  for \code{as.timeSeries}, an object of class \code{"timeSeries"}. \cr
    
  for \code{as.numeric}, \code{as.data.frame}, \code{as.matrix},
  \code{as.ts}, \code{as.list} -  a numeric vector, a data frame, a
  matrix, an object of class \code{ts}, or a \code{"list"},
  respectively.
  
}

\seealso{
  \code{\link{timeSeries}},
  class \code{\linkS4class{timeSeries}}
}

\examples{
## Create an Artificial 'timeSeries' Object
setRmetricsOptions(myFinCenter = "GMT")
charvec <- timeCalendar()
data <- matrix(rnorm(12))
TS <- timeSeries(data, charvec, units = "RAND")
TS

## Coerce to Vector
as.vector(TS)
   
## Coerce to Matrix
as.matrix(TS)
  
## Coerce to Data Frame
as.data.frame(TS)
}

\keyword{chron}
\keyword{ts}
