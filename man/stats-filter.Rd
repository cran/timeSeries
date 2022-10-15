\name{filter}
\alias{filter}

\alias{filter,timeSeries-method}


\title{Linear filtering on a time series}

\description{

  Applies linear filtering to a univariate \code{"timeSeries"}.
      
}

\usage{
\S4method{filter}{timeSeries}(x, filter, method = c("convolution", "recursive"), sides = 2, 
       circular = FALSE, init = NULL)
}

\arguments{
  \item{x}{
    an object from class \code{"timeSeries"}.
  }
  \item{filter}{
    coefficients of the filter.
  }
  \item{method}{
    \code{"convolution"} or \code{"recursive"}.
  }
  \item{sides,circular}{
    
    for convolution filters only.  Onesided if \code{sides = 1}, centred
    around lag 0 if \code{sides = 2}. Circular if \code{circular =
    TRUE.}

  }
    
  \item{init}{

    for recursive filters only. Values before the start of the time
    series.

  }
}

\details{

  \code{filter} is a generic function with default method \code{stats::filter}.
  The method for \code{"timeSeries"} is a wrapper for the latter.

  See \code{?stats::filter} for details about the arguments.
  
}

\seealso{
  base R function \code{\link[stats]{filter}}
}

\value{
  a \code{"timeSeries"} object
}


\examples{
## Creata a dummy signal 'timeSeries' - 
   data <- matrix(rnorm(100), ncol = 2)
   s <- timeSeries(data, units=c("A", "B"))
   head(s)
   
## Filter the series - 
   f <- filter(s, rep(1, 3))
   head(f)
   
## Plot and compare the first series - 
   plot(cbind(s[, 1], f[, 1]), plot.type="s")
}
