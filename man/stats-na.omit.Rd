\name{na}
\alias{na}

\alias{na.omit}
\alias{na.omit,timeSeries-method}
\alias{na.omit.timeSeries}

\alias{removeNA}
\alias{substituteNA}
\alias{interpNA}


\title{Handle missing values in 'timeSeries' objects}

\description{
    
  Functions for handling missing values in \code{"timeSeries"} objects.
    
}

\usage{
\S4method{na.omit}{timeSeries}(object, method = c("r", "s", "z", "ir", "iz", "ie"), 
    interp = c("before", "linear", "after"), FUN, \dots)

## Deprecated:
removeNA(x, \dots)
substituteNA(x, type = c("zeros", "mean", "median"), \dots)
interpNA(x, method = c("linear", "before", "after"), \dots)
}

\arguments{

  \item{object}{ 
    an object of class \code{"timeSeries"}.
  }
  \item{method}{
    for \code{na.omit}, the method of handling NAs; 
    for \code{interpNA}, how to interpolate the matrix column
    by column, see Section \sQuote{Details}.

  }
  \item{interp, type}{
    Three alternative methods are provided to remove NAs from the
    data: 
    \code{type="zeros"} replaces the missing values with zeros,
    \code{type="mean"} replaces the missing values with the column mean,
    \code{type="median"} replaces the missing values with the column
    median.
  }
  \item{FUN}{
    a function or a name of a function, such as \code{"mean"} or
    \code{median}. \code{FUN} is applied to the non-NA values in each
    column to determine the replacement value. The call looks like
    \code{FUN(coli, na.rm = TRUE)}, so \code{FUN} should have argument
    \code{na.rm}. All arguments except \code{object} are ignored if
    \code{FUN} is specified.
    
  }
  
  \item{x}{

    a numeric matrix, or any other object which can be transformed into
    a matrix through \code{x = as.matrix(x, ...)}. If \code{x} is a
    vector, it will be transformed into a one-dimensional matrix.

  }

  \item{\dots}{
    arguments to be passed to the function \code{as.matrix}.
  }
}

\details{

  Functions for handling missing values in \code{"timeSeries"} objects 
  and in objects which can be transformed into a vector or a two 
    dimensional matrix.


    For \code{na.omit} argument \code{method} specifies the method how
    to handle NAs.  Can be one of the following strings:

    \describe{
        \item{method="s"}{\code{na.rm = FALSE}, skip, i.e. do nothing, }
        \item{method="r"}{remove NAs,}
        \item{method="z"}{substitute NAs by zeros,} 
	\item{method="ir"}{interpolate NAs and remove NAs at the
          beginning and end of the series,}

        \item{method="iz"}{interpolate NAs and substitute NAs at the
        beginning and end of the series,}

	\item{method="ie"}{interpolate NAs and extrapolate NAs at the
        beginning and end of theseries.}
    }

    For \code{interpNA} argument \code{method} specifies
    how to interpolate the matrix column
    by column. One of the following character strings:
    \code{"linear"}{},
    \code{"before"}{}, 
    \code{"after"}{}.
    For interpolation the function \code{approx} is used.
       

    
    The functions are listed by topic.
    \cr
    
    \tabular{ll}{  
    \code{na.omit} \tab Handles NAs, \cr
    \code{removeNA} \tab Removes NAs from a matrix object, \cr
    \code{substituteNA} \tab substitute NAs by zero, the column mean or median, \cr
    \code{interpNA} \tab interpolates NAs using R's "approx" function. }
    

    \bold{Missing Values in Price and Index Series:}
    
    Applied to \code{timeSeries} objects the function \code{removeNA}
    just removes rows with NAs from the series. For an interpolation
    of time series points one can use the function \code{interpNA}.
    Three different methods of interpolation are offered: \code{"linear"}
    does a linear interpolation, \code{"before"} uses the previous value,
    and \code{"after"} uses the following value. Note, that the 
    interpolation is done on the index scale and not on the time scale.
    
    \bold{Missing Values in Return Series:}
    
    For return series the function \code{substituteNA} may be useful. The 
    function allows to fill missing values either by \code{method="zeros"}, 
    the \code{method="mean"} or the \code{method="median"} value of the 
    appropriate columns.

}

\note{

    When dealing with daily data sets, there exists another function
    \code{alignDailySeries} which can handle missing data in un-aligned
    calendrical \code{"timeSeries"} objects.
    
    The functions \code{removeNA}, \code{substituteNA} and \code{interpNA} 
    are older implementations. Please use in all cases if possible the 
    new function \code{na.omit}.

    \strong{Additional remarks by GNB:}
    
    \code{removeNA(x)} is equivalent to \code{na.omit(x)} or
    \code{na.omit(x), methods = "r"}.

    \code{interpNA} can be replaced by a call to \code{na.omit} with
    argument method equal to \code{ir}, \code{iz}, or \code{ie}, and
    argument \code{"interp"} equal to the \code{"method"} argument for
    \code{interpNA} (note that the defaults are not the same).

    \code{substituteNA(x, type = "zeros")} is equivalent to
    \code{na.omit(x, method = "z")}. For other values of \code{type} one
    can use argument \code{FUN}, as in \code{na.omit(x, FUN = "mean")}.

    A final remark: the three deprecated functions are non-generic.
    \code{removeNA(x)} is completely redundant as it simply calls
    \code{na.omit}. The other two however may be useful for matrix-like
    objects. Please inform the maintainer of the package if you use them
    on objects other than from class \code{"timeSeries"} and wish them
    kept in the future.

}

\references{

Troyanskaya O., Cantor M., Sherlock G., Brown P., Hastie T., 
    Tibshirani R., Botstein D., Altman R.B., (2001); 
    \emph{Missing Value Estimation Methods for DNA microarrays}
    Bioinformatics 17, 520--525.

}

\seealso{
  \code{\link{alignDailySeries}}
}

\examples{
X <- matrix(rnorm(100), ncol = 5)  # Create a Matrix X
X[3, 5] <- NA                      # Replace a Single NA Inside
X[17, 2:4] <- c(NA, NA, NA)        # Replace Three in a Row Inside
X[13:15, 4] <- c(NA, NA, NA)       # Replace Three in a Column Inside
X[11:12, 5] <- c(NA, NA)           # Replace Two at the Right Border
X[20, 1] <- NA                     # Replace One in the Lower Left Corner
X
Xts <- timeSeries(X)  # convert X to timeSeries Xts

## Remove Rows with NAs
na.omit(Xts)

## Subsitute NA's with zeros or column means (formerly substituteNA())
na.omit(Xts, method = "z")
na.omit(Xts, FUN = "mean")
na.omit(Xts, FUN = "median")

## Subsitute NA's with a trimmed mean
na.omit(Xts, FUN = function(x, na.rm) mean(x, trim = 0.10, na.rm = na.rm))

## Interpolate NA's Linearily (formerly interpNA())
na.omit(X, method = "ir", interp = "linear")
na.omit(X, method = "iz", interp = "linear")
na.omit(X, method = "ie", interp = "linear")
   
## Take Previous Values in a Column
na.omit(X, method = "ir", interp = "before")
na.omit(X, method = "iz", interp = "before")
na.omit(X, method = "ie", interp = "before")


## examples with X (which is a matrix, not "timeSeries"
## (these examples are not run automatically as these functions are
## deprecated.) 
if(FALSE){
## Remove Rows with NAs - 
   removeNA(X)
   
## Subsitute NA's by Zeros or Column Means - 
   substituteNA(X, type = "zeros")
   substituteNA(X, type = "mean")
   
## Interpolate NA's Linearily - 
   interpNA(X, method = "linear")
   # Note the corner missing value cannot be interpolated!
   
## Take Previous Values in a Column - 
   interpNA(X, method = "before")
   # Also here, the corner value is excluded
}
}

\keyword{math}
