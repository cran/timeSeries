\name{colStats}
\alias{colStats} 

\alias{colSums,timeSeries-method}
\alias{colMeans,timeSeries-method}
\alias{colSds}
\alias{colVars}
\alias{colSkewness}
\alias{colKurtosis}        
\alias{colMaxs} 
\alias{colMins}         
\alias{colProds} 
\alias{colQuantiles} 

% \alias{colAvgs}           
\alias{colStdevs}  % removed, but leave the alias for now

% \alias{mean.timeSeries}           
% \alias{var.timeSeries}


\title{Column statistics}


\description{

  A collection of functions to compute column statistical properties of
  financial and economic time series data.

}


\usage{
colStats(x, FUN, \dots) 

colSds(x, \dots)
colVars(x, \dots)
colSkewness(x, \dots)
colKurtosis(x, \dots)
colMaxs(x, \dots)
colMins(x, \dots)
colProds(x, \dots)
colQuantiles(x, prob = 0.05, \dots)
}

% colAvgs(x,  \dots)

% \method{mean}{timeSeries}(x, \dots)
% \method{var}{timeSeries}(x, \dots)



\arguments{
  
  \item{x}{
    a rectangular object which can be transformed into a matrix
    by the function \code{as.matrix}.  
  }
  \item{FUN}{
    a function name, the statistical function to be applied.
  }
  \item{prob}{
    a numeric value in [0,1].
  }
  \item{\dots}{
    arguments to be passed.
  }
        
}

\details{

    \tabular{ll}{  
    \code{colStats}\tab calculates column statistics, \cr
    
    \code{colSums} \tab calculates column sums, \cr
    \code{colMeans} \tab calculates column means, \cr
    \code{colSds} \tab calculates column standard deviations, \cr
    \code{colVars} \tab calculates column variances, \cr 
    \code{colSkewness} \tab calculates column skewness, \cr
    \code{colKurtosis} \tab calculates column kurtosis, \cr
    \code{colMaxs} \tab calculates maximum values in each column, \cr
    \code{colMins} \tab calculates minimum values in each column, \cr
    \code{colProds} \tab computes product of all values in each column, \cr
    \code{colQuantiles} \tab computes quantiles of each column. }

}

\value{
  each function returns a numeric vector of the statistics, one for each
  column
}

\seealso{
  \code{\link{rollStats}}
}

\examples{
## Simulated Return Data in Matrix Form -
   x = matrix(rnorm(252), ncol = 2)
     
## Mean Columnwise Statistics -
   colStats(x, FUN = mean)
   
## Quantiles Column by Column -
   colQuantiles(x, prob = 0.10, type = 1)  
}

\keyword{univar}
