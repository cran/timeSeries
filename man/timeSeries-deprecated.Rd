\name{timeSeries-deprecated}

\alias{seriesData} % removed

\alias{removeNA}
\alias{substituteNA}
\alias{interpNA}



\title{Deprecated functions in 'timeSeries' package}

\usage{
removeNA(x, \dots)
substituteNA(x, type = c("zeros", "mean", "median"), \dots)
interpNA(x, method = c("linear", "before", "after"), \dots)
}

\arguments{

  \item{x}{

    a numeric matrix, or any other object which can be transformed into
    a matrix through \code{x = as.matrix(x, ...)}. If \code{x} is a
    vector, it will be transformed into a one-dimensional matrix.

  }

  \item{method}{
    for \code{interpNA}, how to interpolate the matrix column
    by column, see Section \sQuote{Details}.

  }
  \item{type}{
    Three alternative methods are provided to remove NAs from the
    data: 
    \code{type="zeros"} replaces the missing values with zeros,
    \code{type="mean"} replaces the missing values with the column mean,
    \code{type="median"} replaces the missing values with the column
    median.
  }
  \item{\dots}{
    arguments to be passed to the function \code{as.matrix}.
  }
}

\description{
  
  \tabular{ll}{

    \code{seriesData} (removed) extracts data slot from a 'timeSeries'.
    use \code{\link{as.matrix}} instead. \cr
    % \item{object}{
    %     [is][seriesData][seriesPositions][show][summary] -    
    %     an object of class \code{timeSeries}.
    %     }


    % \code{.time.timeSeries} \tab Extracts the time(s) from a
    % 'timeSeries', use \code{\link{time}} instead. \cr

    
    % \code{seriesPositions} \tab Extracts positions slot from a 'timeSeries', \cr
    % \code{newPositions<-} \tab Modifies positions of a 'timeSeries' object, \cr
  }

}

\keyword{internal}  
