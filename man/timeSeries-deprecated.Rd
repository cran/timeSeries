\name{timeSeries-deprecated}

\alias{removeNA}


\title{Deprecated functions in 'timeSeries' package}

\usage{
removeNA(x, \dots)
}

\arguments{

  \item{x}{

    a numeric matrix, or any other object which can be transformed into
    a matrix through \code{x = as.matrix(x, ...)}. If \code{x} is a
    vector, it will be transformed into a one-dimensional matrix.

  }

  \item{\dots}{
    arguments to be passed to the function \code{as.matrix}.
  }
}

\description{
  
    \code{seriesData} (removed) extracts data slot from a 'timeSeries'.
    use \code{\link{as.matrix}} instead.

    % \item{object}{
    %     [is][seriesData][seriesPositions][show][summary] -    
    %     an object of class \code{timeSeries}.
    %     }


    % \code{.time.timeSeries} \tab Extracts the time(s) from a
    % 'timeSeries', use \code{\link{time}} instead. \cr

    
    % \code{seriesPositions} \tab Extracts positions slot from a 'timeSeries', \cr
    % \code{newPositions<-} \tab Modifies positions of a 'timeSeries' object, \cr

}

\keyword{internal}  
