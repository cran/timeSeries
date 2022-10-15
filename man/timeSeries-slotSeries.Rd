\name{series-methods}

\docType{methods}


\alias{series}
\alias{series<-}

\alias{series,timeSeries-method}
\alias{series<-,timeSeries,matrix-method}
\alias{series<-,timeSeries,ANY-method}
\alias{series<-,timeSeries,data.frame-method}
\alias{series<-,timeSeries,vector-method}

\alias{coredata}
\alias{coredata<-}

\alias{coredata,timeSeries-method}
\alias{coredata<-,timeSeries,matrix-method}
\alias{coredata<-,timeSeries,ANY-method}
\alias{coredata<-,timeSeries,data.frame-method}
\alias{coredata<-,timeSeries,vector-method}



\title{Get and set the data component of a 'timeSeries'}


\description{
  Get and set the data component of a 'timeSeries'.
}


\usage{
series(x)
series(x) <- value
}


\arguments{

    \item{x}{
        a \code{timeSeries} object.
        }
    \item{value}{
        a \code{vector}, a \code{data.frame} or a \code{matrix} object of
        numeric data. 
        }
}

\details{

  \code{series} returns the \code{@.Data} slot of a \code{timeSeries}
  object in \code{matrix} form. 

  The assignment version of \code{series} replaces the values of the
  time series with \code{value}. The row and column names of
  \code{value} are used if not \code{NULL}, otherwise they are left as
  in \code{x}. The most natural use is when \code{value} has the same
  dimensions as \code{as.matrix(x)}, but if that is not the case the
  result is almost as if \code{value} was converted to
  \code{"timeSeries"} directly.
    
  \code{coredata} and its assignment counterpart are equivalent
  alternatives to \code{series} and its assignment part, respectively.

}


\seealso{
  \code{\link{timeSeries}}
}


\examples{
## A Dummy 'timeSeries' Object
   ts <- timeSeries()
   ts

## Get the Matrix Part - 
   mat <- series(ts)
   class(mat)
   mat

## Assign a New Univariate Series - 
   series(ts) <- rnorm(12)
   ts
   
## Assign a New Bivariate Series - 
   series(ts) <- matrix(rnorm(12), ncol = 2)
   ts
}


\keyword{programming}

