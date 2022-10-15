\name{periodical}

\alias{endOfPeriod}
                                    
\alias{endOfPeriodSeries}
\alias{endOfPeriodStats}
\alias{endOfPeriodBenchmarks}


\title{End-of-Period series, stats, and benchmarks}


\description{

    Computes periodical statistics back to a given period.
    
}


\usage{

endOfPeriodSeries(x, 
    nYearsBack = c("1y", "2y", "3y", "5y", "10y", "YTD"))

endOfPeriodStats(x, 
    nYearsBack = c("1y", "2y", "3y", "5y", "10y", "YTD"))
    
endOfPeriodBenchmarks(x, benchmark = ncol(x),
    nYearsBack = c("1y", "2y", "3y", "5y", "10y", "YTD"))
    
}


\arguments{

  \item{x}{

    an end-of-month recorded multivariate \code{"timeSeries"} object.
    One of the columns holds the benchmark series specified by argument
    \code{benchmark},
	
  } 
  \item{nYearsBack}{

    a period string. How long back should the series be treated? Options
    include values from 1 year to 10 years, and year-to-date: "1y",
    "2y", "3y", "5y", "10y", "YTD".

  }
  \item{benchmark}{
	  
    an integer giving the position of the benchmark series in
    \code{x}. By default this is the last column of \code{x}.

  }
}

\details{
    
  \code{endOfPeriodSeries} returns series back to a given period.

  \code{endOfPeriodStats} returns statistics back to a given period.

  \code{endOfPeriodBenchmarks} returns benchmarks back to a given
  period.
    
  \code{x} must be end of month data. Such series can be created using
  functions like \code{align}, \code{alignDailySeries},
  \code{daily2monthly}.

}

\value{

  for \code{endOfPeriodStats}, a data frame;

  for \code{endOfPeriodBenchmarks} - currently \code{NULL} (invisibly),
  the function is unfinished.

}

 
\examples{
## Load Series: Column 1:3 Swiss Market, Column 8 (4) Benchmark
   x <- 100 * LPP2005REC[, c(1:3, 8)]
   colnames(x)
   x <- daily2monthly(x)
   x
   
## Get the Monthly Series - 
   endOfPeriodSeries(x, nYearsBack="1y")
   
## Compute the Monthly Statistics - 
   endOfPeriodStats(x, nYearsBack="1y")
   
## Compute the Benchmark - 
   endOfPeriodBenchmarks(x, benchmark=4) 
}


\keyword{chron}

