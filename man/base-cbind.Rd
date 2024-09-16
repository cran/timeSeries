\name{cbind}
\alias{cbind}

\alias{rbind}
\alias{cbind2}
\alias{rbind2}

% \alias{cbind,timeSeries-method} 
% \alias{rbind,timeSeries-method} 

\alias{cbind.timeSeries}
\alias{rbind.timeSeries}

% \alias{c.timeSeries}

\alias{cbind2,ANY,timeSeries-method}
\alias{cbind2,timeSeries,ANY-method}
\alias{cbind2,timeSeries,missing-method}
\alias{cbind2,timeSeries,timeSeries-method}

\alias{rbind2,ANY,timeSeries-method}
\alias{rbind2,timeSeries,ANY-method}
\alias{rbind2,timeSeries,missing-method}
\alias{rbind2,timeSeries,timeSeries-method}


\title{Bind 'timeSeries' objects by column or row}

\description{
  
  Binds \code{"timeSeries"} objects either by column or by row.
  
}

% \S4method{merge}{timeSeries,timeSeries}(x, y, ...)
\usage{
\method{cbind}{timeSeries}(\dots, deparse.level = 1)
\method{rbind}{timeSeries}(\dots, deparse.level = 1)

\S4method{cbind2}{timeSeries,ANY}(x, y)
## other methods for 'cbind2' with the same arguments, see Details

\S4method{rbind2}{timeSeries,ANY}(x, y)
## other methods for 'rbind2' with the same arguments, see Details
}

\arguments{
  
  \item{x, y}{
    objects, at least one of whom is of class \code{"timeSeries"}.
  }

  \item{\dots}{
    further arguments to bind.
  }

  \item{deparse.level}{
    see the documentation of \code{base::cbind}.
  }

}

\details{

  These functions bind the objects by row \code{rXXX} or column
  (\code{cXXX}.

  \code{cbind} and \code{rbind} are S3 generics, so the
  \code{"timeSeries"} methods describe here are called only when the
  first argument is \code{"timeSeries"}.

  \code{cbind2} and \code{rbind2} are S4 generics which dispatch on the
  first two arguments. The \code{"timeSeries"} methods for these are
  invoked whenever at least one of the first two arguments is of class
  \code{"timeSeries"}.
  
  All functions can be called with more than two arguments. After the
  first two are merged, the result is merged with the third, and so on.
  
}

\value{
  an object of class \code{"timeSeries"}
}

\seealso{
  \code{\link{merge}} for another way to merge \code{"timeSeries"}
  object column-wise.
  
  \code{\link[base]{rbind}} and \code{\link[base]{cbind}} from base R,

  \code{\link[methods]{rbind2}} and \code{\link[methods]{cbind2}} from
  package \code{"methods"},
  
}

\examples{
## Load Microsoft Data Set -
   x <- MSFT[1:12, ]
   x

## Bind Columnwise -
   X <- cbind(x[, "Open"], returns(x[, "Open"]))
   colnames(X) <- c("Open", "Return")
   X

## Bind Rowwise - 
   Y <- rbind(x[1:3, "Open"], x[10:12, "Open"])
   Y
}

\keyword{chron}
