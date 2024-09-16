\name{timeSeries-method-stats}
\docType{methods}

\alias{sd-methods}
\alias{var-methods}
\alias{cov-methods}
\alias{cor-methods}

%\alias{dcauchy-methods}
%\alias{dnorm-methods}
%\alias{dt-methods}

%\alias{sd,timeSeries-method}
%\alias{var,timeSeries-method}
%\alias{cov,timeSeries-method}
%\alias{cor,timeSeries-method}
%\alias{dcauchy,timeSeries-method}
%\alias{dnorm,timeSeries-method}
%\alias{dt,timeSeries-method}


\title{Base R functions applied to 'timeSeries' objects}

\description{

  Many base R statistical functions work on (the data part of)
  \code{timeSeries} objects without the need for special methods, e.g.,
  \code{var}, \code{sd}, \code{cov}, \code{cor}, probability densities,
  and others. This page gives some examples with such functions.

}

%% 2023-05-29: these S4 methods don't exist! (and the functions are not generic!)
%\usage{
%\S4method{cov}{timeSeries}(x, y = NULL, use = "all.obs", 
%    method = c("pearson", "kendall", "spearman"))
%    
%\S4method{cor}{timeSeries}(x, y = NULL, use = "all.obs", 
%    method = c("pearson", "kendall", "spearman"))
%}
%
%\arguments{
%  \item{x}{
%    an univariate object of class \code{timeSeries}.
%  }
%  \item{y}{
%    \code{NULL} (default) or a \code{timeSeries} object with compatible
%    dimensions to \code{x}. The default is equivalent to \code{y = x}
%    (but more efficient).
%  }  
%  \item{use}{
%    an optional character string giving a method for computing
%    covariances in the presence of missing values. This must be (an
%    abbreviation of) one of the strings \code{"all.obs"},
%    \code{"complete.obs"} or \code{"pairwise.complete.obs"}.
%  }
%  \item{method}{ 
%    a character string indicating which correlation coefficient (or 
%    covariance) is to be computed. One of \code{"pearson"} (default), 
%    \code{"kendall"}, or \code{"spearman"}. Can be abbreviated.
%  }
%}
%
%\value{
%  covariance or correlation matrix
%}

\seealso{
  \code{\link{colStats}},
  \code{\link{colVars}}, and other \code{colXXX} functions
}
\examples{
## Load Microsoft Data Set - 
   data(MSFT)
   X = MSFT[, 1:4]
   X = 100 * returns(X)

## Compute Covariance Matrix - 
   cov(X[, "Open"], X[, "Close"])
   cov(X)

cor(X)   
}

\keyword{methods}
\keyword{chron}
