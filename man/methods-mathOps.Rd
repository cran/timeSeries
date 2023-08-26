\name{math}
\alias{math}

\alias{Math,timeSeries-method}

\alias{Math2,timeSeries-method}
\alias{Summary,timeSeries-method}

\alias{Ops,vector,timeSeries-method}
\alias{Ops,array,timeSeries-method}
\alias{Ops,ts,timeSeries-method}
\alias{Ops,timeSeries,vector-method}
\alias{Ops,timeSeries,array-method}
\alias{Ops,timeSeries,ts-method}
\alias{Ops,timeSeries,timeSeries-method}


%\alias{-,timeSeries,missing-method}
%\alias{+,timeSeries,missing-method}
\alias{cummax,timeSeries-method}
\alias{cummin,timeSeries-method}
\alias{cumprod,timeSeries-method}
\alias{cumsum,timeSeries-method}
\alias{trunc,timeSeries-method}
\alias{log,timeSeries-method}

%\alias{\%*\%,timeSeries,vector-method}
%\alias{\%*\%,timeSeries,ANY-method}
%\alias{\%*\%,ANY,timeSeries-method}

%\alias{quantile.timeSeries}
\alias{quantile,timeSeries-method}

%\alias{median.timeSeries}
\alias{median,timeSeries-method}


\title{Mathematical operations on 'timeSeries'}

\description{
  
  Functions and methods for mathematical operations on \code{"timeSeries"}.
  
}

\usage{
\S4method{Ops}{timeSeries,timeSeries}(e1, e2)
\S4method{Math}{timeSeries}(x)
\S4method{Math2}{timeSeries}(x, digits)

\S4method{quantile}{timeSeries}(x, \dots)
\S4method{median}{timeSeries}(x, na.rm = FALSE, \dots)
}


\arguments{
  \item{x}{
    an object of class \code{timeSeries}.
  }
  %  \item{center, scale}{ 
  %    [scale] - \cr
  %    either a logical value or a numeric vector of length equal to
  %    the number of columns of \code{x}.
  %  }
  %  \item{diff}{
  %    an integer indicating the order of the difference. By default 1.
  %  }
    
  \item{digits}{
    number of digits to be used in 'round' or 'signif'.
  }
    
  \item{e1, e2}{

    at least one of the two objects is from class \code{"timeSeries"}
    (for the methods described on this page).
    
  }
    
  %  \item{lag}{   
  %    an integer indicating which lag to use. By default 1.
  %  }

  \item{na.rm}{
    a logical value: should missing values be removed?
  }

  %  \item{pad}{
  %    [diffSeries] - \cr
  %    which value should get the padded values? By default \code{NA}.
  %    Another choice often used would be zero.
  %  } 
  %  \item{trim}{
  %    a logical value. By default \code{TRUE}, the first missing 
  %    observation in the return series will be removed. 
  %  }
  \item{\dots}{
    arguments to be passed.
  }
  
}

\details{

  The methods for the \code{Math} and \code{Math2} groups of
  mathematical functions return 'timeSeries' objects. Most of them work
  element-wise on the data part of the time series with the exception of
  \code{cummin}, \code{cummax}, \code{cumsum}, and \code{cumprod} which
  work columnwise.
  
  The \code{Ops} group includes mathematical operators. For the binary
  operators methods are defined for pairs of at least one 'timeSeries'
  object.  These work as expected on the data parts of the arguments.
  If the operation gives a value of the same dimension as the data part
  of the 'timeSeries' object, it replaces the original data in the
  object.

  There are also methods for \code{quantile} and \code{median}.
  
}

\value{

  the value from a mathematical or logical operation operating on
  objects of class \code{"timeSeries"} or the value computed by a
  mathematical function.
  
}

\seealso{
  \code{\link[=colCum]{colCumXXX}}
}

\examples{
## Create an Artificial 'timeSeries' Object - 
   setRmetricsOptions(myFinCenter = "GMT")
   charvec = timeCalendar()
   set.seed(4711)
   data = matrix(exp(cumsum(rnorm(12, sd = 0.1))))
   TS = timeSeries(data, charvec, units = "TS")
   TS

## Mathematical Operations: | +/- * ^ ... - 
   TS^2
   TS[2:4]
   OR = returns(TS)
   OR
   OR > 0
}

\keyword{chron}
\keyword{methods}
