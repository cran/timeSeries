\name{dimnames}

\alias{dim,timeSeries-method}
\alias{dim<-,timeSeries-method}

\alias{dimnames,timeSeries-method}
\alias{dimnames<-,timeSeries,list-method}

\alias{colnames<-,timeSeries-method}
\alias{colnames,timeSeries-method}

\alias{rownames,timeSeries-method}
\alias{rownames<-,timeSeries,timeDate-method}
\alias{rownames<-,timeSeries,ANY-method}

\alias{names,timeSeries-method}
\alias{names<-,timeSeries-method}


\title{Dimension and their names for 'timeSeries' objects}

\description{

  Get and assign names, row names, column names, and dim names of
  \code{"timeSeries"} objects.
    
}

% \usage{

% %\S4method{dim}{timeSeries}(x)
% %\S4method{dimnames}{timeSeries}(x)
% %\S4method{dimnames}{timeSeries}(x) <- value

% dim(x)

% dimnames(x)
% dimnames(x) <- value

% colnames(x)
% colnames(x) <- value

% rownames(x)
% rownames(x) <- value

% \method{is.array}{timeSeries}(x)

% }

%\arguments{
%   
%    \item{value}{
%        a valid value for names component of \code{dimnames(x)}. 
%         For a \code{"timeSeries"} object this is either \code{NULL} or a 
%         character vector of length the column dimension. Not, row names
%         cannot be assigne for a \code{"timeSeries"} object, the function
%        \code{rownames()} will stop and return an error message.
%        }
%    \item{x}{
%        an object of class \code{timeSeries}.
%        }
%       
%}

\details{

  \code{"timeSeries"} methods are available for base R functions working
  on dimension names, including \code{dim}, \code{dimnames},
  \code{colnames}, \code{rownames}, \code{names} and their assignment
  variants.

  \code{dim} is the dimension of the underlying data matrix.

  \code{rownames} gives the datetime stamps as a character
  vector. \code{rownames<-} sets them.

  \code{colnames} gives the values of \code{x@units}. These are
  conceptually the column names of the data matrix. \code{colnames<-}
  sets slot \code{units} of \code{x}.

  \code{dimnames} gives \code{list(rownames(x), colnames(x)}.
  \code{dimnames<-} calls \code{rownames} and \code{colnames} on
  \code{value[[1]]} and \code{value[[2]]}, respectively.

}

\note{

  (GNB; todo) The \code{"dim<-"}, currently converts \code{x} to a
  vector if \code{value} is \code{NULL}, otherwise it ignores
  \code{value}, does nothing and returns \code{x} unchanged. This
  behaviour should not be relied upon and may be changed in the future,
  e.g. by issuing warning when \code{value} is not \code{NULL}. Or
  throwing error altogether if assignment with \code{"dim<-"} is
  attempted.

}

\examples{
## Load Swiss Pension Fund Benchmark Data -
   X <- LPP2005REC[1:10, 1:3]
   
## Get Dimension - 
   dim(X)
   
## Get Column and Row Names -
   dimnames(X)
   
## Get Column / Row Names -
   colnames(X)
   rownames(X) 
   
## Try your own DIM - 
   DIM <- function(x) {c(NROW(x), NCOL(x))}
   DIM(X) 
   DIM(X[, 1])

## Try length / LENGTH - 
   length(X)
   length(X[, 1])
   LENGTH <- function(X) NROW(X)
   LENGTH(X)
   
## Columns / Rows - 
   ncol(X); NCOL(X)
   nrow(X); NROW(X)
                
## See also - 
   isUnivariate(X)
   isMultivariate(X)
}

\keyword{chron}
