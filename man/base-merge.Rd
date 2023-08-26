\name{merge}                     
\docType{methods}

\alias{merge}
\alias{merge-methods}

\alias{merge,ANY,ANY-method}
\alias{merge,ANY,timeSeries-method}
\alias{merge,matrix,timeSeries-method}
\alias{merge,numeric,timeSeries-method}

\alias{merge,timeSeries,ANY-method}
\alias{merge,timeSeries,matrix-method}
\alias{merge,timeSeries,missing-method}
\alias{merge,timeSeries,numeric-method}
\alias{merge,timeSeries,timeSeries-method}


\title{Merge 'timeSeries' objects}

\description{

  Merges several object types with \code{"timeSeries"} objects.  The
  number of rows must match.
    
}

\usage{
merge(x, y, \dots)
}


\arguments{
  \item{x,y}{
    objects to merge, at least one of class \code{"timeSeries"}.
  }
  \item{...}{further objects to merge.}
}

%\details{
%}

\value{
  a \code{"timeSeries"} object
}

\section{Methods}{
  \describe{
  
    \item{\code{signature(x = "timeSeries", y = "missing")}}{
      %%  ~~describe this method here~~
    }
    
    \item{\code{signature(x = "timeSeries", y = "ANY")}}{
      %%  ~~describe this method here~~
    }
    
    \item{\code{signature(x = "timeSeries", y = "matrix")}}{
      %%  ~~describe this method here~~
    }
    
    \item{\code{signature(x = "timeSeries", y = "numeric")}}{
      %%  ~~describe this method here~~
    }
    
    \item{\code{signature(x = "timeSeries", y = "timeSeries")}}{
      %%  ~~describe this method here~~
    }


    \item{\code{signature(x = "ANY", y = "ANY")}}{
      %%  ~~describe this method here~~
    }
    
    \item{\code{signature(x = "ANY", y = "timeSeries")}}{
      %%  ~~describe this method here~~
    }
    
    \item{\code{signature(x = "matrix", y = "timeSeries")}}{
      %%  ~~describe this method here~~
    }
    
    \item{\code{signature(x = "numeric", y = "timeSeries")}}{
      %%  ~~describe this method here~~
    }
    
  }
}

\seealso{
  \code{\link{cbind}}
}

\examples{
## Load Series -
   x <- MSFT[1:12, ]

## Merge 'timeSeries' with missing Object -
   merge(x)

\dontshow{set.seed(1234)}
## Merge 'timeSeries' with numeric Object -
   y <- rnorm(12)
   class(y)
   merge(x, y)

## Merge 'timeSeries' with matrix Object -
   y <- matrix(rnorm(24), ncol=2)
   class(y)
   merge(x, y)

## Merge 'timeSeries' with matrix Object -
   y <- timeSeries(data=rnorm(12), charvec=time(x))
   class(y)
   merge(x, y)
}

\keyword{methods}
\keyword{chron}
\keyword{ts}
