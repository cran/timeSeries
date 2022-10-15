\name{print-methods}


\title{Print 'timeSeries' objects}


\alias{show,timeSeries-method}
\alias{print,timeSeries-method}

\description{
    
  Print 'timeSeries' objects.
    
}


\usage{
\S4method{show}{timeSeries}(object)

\S4method{print}{timeSeries}(x, FinCenter = NULL, format = NULL,
          style = c("tS", "h", "ts"), by = c("month", "quarter"), ...)
}


\arguments{

  \item{object,x}{  
    an object of class \code{timeSeries}.
  }
  \item{FinCenter}{
    a character with the the location of the  financial center named 
    as "continent/city". 
  }
  \item{format}{
    the format specification of the input character vector,
    a character string with the format in POSIX notation.
  }  
  \item{style}{
    a character string, one of \code{"tS"}, \code{"h"}, or \code{"ts"}.
  }
  \item{by}{
    a character string, one of \code{"month"}, \code{"quarter"}.
  }
  \item{\dots}{
    arguments passed to other methods.
  }
}

%   \item{recordIDs}{
%     for the \code{print} method, a logical value - should the
%     \code{recordIDs} be printed together with the data matrix and time
%     series positions?

\details{

  \code{show} does not have additional arguments.

  The \code{print} method allows to modify the way the object is shown
  by explicitly calling \code{print}.

  The default for \code{style} is \code{tS}. For univariate time series
  the \code{style = "h"} causes the object to be printed as a vector
  with the time stamps as labels. Finally, \code{style = "ts"} like
  objects from base R class \code{"ts"}. The last value is suitable for
  quarterly and monthly time series.

}


\value{

    Prints an object of class \code{timeSeries}.
        
}


\examples{
## Load Micsrosoft Data - 
   setRmetricsOptions(myFinCenter = "GMT")
   LPP <- MSFT[1:12, 1:4]

## Abbreviate Column Names - 
   colnames(LPP) <- abbreviate(colnames(LPP), 6)
   
## Print Data Set -
   print(LPP)
   
## Alternative Use, Show Data Set - 
   show(LPP)

## a short subseries to demo 'print'
   hC <- head(MSFT[ , "Close"])
   class(hC)
   print(hC)
   print(hC, style = "h")
}


\keyword{chron}

