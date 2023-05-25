\name{timeSeries-package}


\alias{timeSeries-package}


\docType{package}


\title{Utilities and tools package}


\description{

  Package \pkg{timeSeries} is part of the Rmetrics suit of R packages.
  It provides a class, \code{timeSeries}, particularly aimed at analysis
  of financial data, along with many methods, functions, and utilities
  for statistical and financial computations on time series.

}

\author{
Diethelm Wuertz [aut] (original code),
Tobias Setz [aut],
Yohan Chalabi [aut],
Martin Maechler [ctb] (<https://orcid.org/0000-0002-8685-9910>),
Georgi N. Boshnakov [cre, ctb]

Maintainer: Georgi N. Boshnakov <georgi.boshnakov@manchester.ac.uk>
}

\details{
  The following sections have not been updated for some time.
}
  

\section{timeSeries - S4 'timeSeries' Class}{

  % \code{getDataPart, series} \tab ... \cr

  \tabular{ll}{
    \code{\link{timeSeries}} \tab Creates a \code{"timeSeries"} from scratch\cr
    \code{\link{series}}, coredata \tab Extracts the data \cr
    \code{\link{getUnits}} \tab Extracts the time serie units \cr
    \code{\link{time}} \tab Extracts the positions of timestamps \cr
    \code{x@format} \tab Extracts the format of the timestamp \cr
    \code{\link{finCenter}} \tab Extracts the financial center \cr
    \code{x@recordIDs} \tab Extracts the record IDs \cr
    \code{x@title} \tab Extracts the title \cr
    \code{x@documentation} \tab Extracts the documentation
  } 
   
}


\section{Base Time Series Functions}{

  % \code{comment} \tab ? ... \cr
  
  \tabular{ll}{
    \code{\link[=apply,timeSeries-method]{apply}} \tab Applies a function to blocks of a \code{"timeSeries"} \cr
    \code{\link{cbind}} \tab Combines columns of two \code{"timeSeries"} objects \cr
    \code{\link{rbind}} \tab Combines rows of two \code{"timeSeries"} objects \cr
    \code{\link{diff}} \tab Returns differences of a \code{"timeSeries"} object \cr
    \code{\link[=dim,timeSeries-method]{dim}} \tab returns dimensions of a \code{"timeSeries"} object \cr
    \code{\link{merge}} \tab Merges two \code{"timeSeries"} objects \cr
    \code{\link[=rank,timeSeries-method]{rank}} \tab Returns sample ranks of a \code{"timeSeries"} object \cr
    \code{\link[=apply,timeSeries-method]{rev}} \tab Reverts a \code{"timeSeries"} object \cr
    \code{\link{sample}} \tab Resamples a \code{"timeSeries"} object \cr
    \code{\link{scale}} \tab Scales a \code{"timeSeries"} object \cr
    \code{\link[=sort,timeSeries-method]{sort}} \tab Sorts a \code{"timeSeries"} object \cr
    \code{\link[=start,timeSeries-method]{start}} \tab Returns start date/time of a \code{"timeSeries"} \cr
    \code{\link[=apply,timeSeries-method]{end}} \tab Returns end date/time of a \code{"timeSeries"} \cr
    \code{\link[=t,timeSeries-method]{t}} \tab Returns the transpose of a \code{"timeSeries"}  object  \cr
    \code{\link[=attach,timeSeries-method]{attach}} \tab Attaches a \code{"timeSeries"} to the search path
  }
}


\section{Subsetting 'timeSeries' Objects}{

    % \code{.subset_} \tab Subsets \code{"timeSeries"} objects \cr
    % \code{.findIndex} \tab Index search in a \code{"timeSeries"} object \cr
  
    \tabular{ll}{
    \code{[} \tab Subsets a \code{"timeSeries"} object \cr
    \code{[<-} \tab Assigns values to a subset \cr
    \code{$} \tab Subsets a \code{"timeSeries"} by column names \cr
    \code{$<-} \tab Replaces subset by column names \cr
    \code{\link[=head,timeSeries-method]{head}} \tab Returns the head of a \code{"timeSeries"} \cr
    \code{\link[=tail,timeSeries-method]{tail}} \tab Returns the tail of a time Series \cr
    \code{\link{na.omit}} \tab Handles NAs in a \code{"timeSeries"} object \cr
    \code{\link{removeNA}} \tab removes NAs from a matrix object \cr
    \code{\link{substituteNA}} \tab substitutes NAs by zero, column mean or median \cr
    \code{\link{interpNA}} \tab interpolates NAs using R's "approx" function }
    
}


\section{Mathematical Operation}{
    
    \tabular{ll}{  
    \code{\link[=Ops,timeSeries,timeSeries-method]{Ops}} \tab S4: Arith method for a \code{"timeSeries"} object \cr
    \code{\link[=Math,timeSeries-method]{Math}} \tab S4: Math method for a \code{"timeSeries"} object \cr
    \code{\link[=Math2,timeSeries-method]{Math2}} \tab S4: Maths method for a \code{"timeSeries"} object \cr
    \code{abs} \tab Returns absolute values of a \code{"timeSeries"} object \cr
    \code{sqrt} \tab Returns square root of a \code{"timeSeries"} object \cr
    \code{exp} \tab Returns the exponential values of a \code{"timeSeries"} object \cr
    \code{log} \tab Returns the logarithm of  a \code{"timeSeries"} object \cr
    \code{sign} \tab Returns the signs of a \code{"timeSeries"} object \cr
    \code{\link{diff}} \tab Differences a \code{"timeSeries"} object \cr 
    \code{\link{scale}} \tab Centers and/or scales a \code{"timeSeries"} object \cr
    \code{\link[=quantile,timeSeries-method]{quantile}} \tab Returns quantiles of an univariate \code{"timeSeries"}}

}


\section{Methods}{

    \tabular{ll}{
    \code{\link{as.timeSeries}} \tab Defines method for a \code{"timeSeries"} \cr
    \code{as.*.default} \tab Returns the input  \cr
    \code{as.*.ts} \tab Transforma a 'ts' object into a \code{"timeSeries"} \cr
    \code{as.*.data.frame} \tab Transforms a 'data.frame' intp a 'timeSeries \cr
    \code{as.*.character} \tab Loads and transforms from a demo file \cr
    \code{as.*.zoo} \tab Transforms a 'zoo' object into a \code{"timeSeries"} \cr 
    \code{as.vector.*} \tab Converts univariate \code{"timeSeries"} to vector \cr
    \code{as.matrix.*} \tab Converts \code{"timeSeries"} to matrix \cr
    \code{as.numeric.*} \tab Converts \code{"timeSeries"} to numeric \cr
    \code{as.data.frame.*} \tab Converts \code{"timeSeries"} to data.frame \cr
    \code{as.ts.*} \tab Converts \code{"timeSeries"} to ts \cr
    \code{as.logical.*} \tab Converts \code{"timeSeries"} to logical \cr
    \code{\link{is.timeSeries}} \tab Tests for a \code{"timeSeries"} object \cr
    \code{\link[=plot,timeSeries-method]{plot}} \tab Displays a X-Y \code{"timeSeries"} Plot \cr
    \code{\link[=lines,timeSeries-method]{lines}} \tab Adds connected line segments to a plot \cr
    \code{\link[=points,timeSeries-method]{points}} \tab Adds Points to a plot \cr
    \code{\link[=show,timeSeries-method]{show}} \tab Prints a 'timeSeries oobject}
    
}


\section{Financial time series functions}{

    \tabular{ll}{ 
    \code{\link{align}} \tab Aligns a \code{"timeSeries"} to time stamps \cr 
    \code{\link{cumulated}} \tab Computes cumulated series from a returns \cr
    \code{\link{alignDailySeries}} \tab Aligns a \code{"timeSeries"} to calendarical dates \cr
    \code{\link{rollDailySeries}} \tab Rolls a 'timeSeries daily\cr
    \code{\link{drawdowns}} \tab Computes series of drawdowns from financial returns \cr
    \code{\link{drawdownsStats}} \tab Computes drawdowns statistics \cr
    \code{\link{durations}} \tab Computes durations from a financial time series \cr  
    \code{\link{countMonthlyRecords}} \tab Counts monthly records in a \code{"timeSeries"} \cr
    \code{\link{rollMonthlyWindows}} \tab Rolls Monthly windows \cr
    \code{\link{rollMonthlySeries}} \tab Rolls a \code{"timeSeries"} monthly \cr   
    \code{\link{endOfPeriodSeries}} \tab Returns end of periodical series \cr
    \code{\link{endOfPeriodStats}} \tab Returns end of period statistics \cr
    \code{\link{endOfPeriodBenchmarks}} \tab Returns period benchmarks \cr 
    \code{\link{returns}} \tab Computes returns from prices or indexes \cr
    \code{\link{returns0}} \tab  Computes untrimmed returns from prices or indexes \cr   
    \code{\link{runlengths}} \tab Computes run lenghts of a \code{"timeSeries"} \cr
    \code{\link{smoothLowess}} \tab Smoothes a \code{"timeSeries"} \cr    
    \code{\link{smoothSpline}} \tab Smoothes a \code{"timeSeries"} \cr    
    \code{\link{smoothSupsmu}} \tab Smoothes a \code{"timeSeries"} \cr    
    \code{\link{splits}} \tab Detects \code{"timeSeries"} splits by outlier detection \cr
    \code{\link{spreads}} \tab Computes spreads from a price/index stream \cr
    \code{\link{turns}} \tab Computes turning points in a \code{"timeSeries"} object \cr
    \code{\link{turnsStats}} \tab Computes turning points statistics }

}


\section{Statistics Time Series functions}{

    \tabular{ll}{ 
    \code{\link{colCumsums}} \tab Computes cumulated column sums of a \code{"timeSeries"} \cr
    \code{\link{colCummaxs}} \tab Computes cumulated maximum of a \code{"timeSeries"} \cr
    \code{\link{colCummins}} \tab Computes cumulated minimum of a \code{"timeSeries"} \cr
    \code{\link{colCumprods}} \tab Computes cumulated pruduct values by column \cr
    \code{\link{colCumreturns}} \tab Computes cumulated returns by column \cr    
    \code{\link[=colSums,timeSeries-method]{colSums}} \tab Computes sums of all values in each column \cr
    \code{\link[=colMeans,timeSeries-method]{colMeans}} \tab Computes means of all values in each column \cr
    \code{\link{colSds}} \tab Computes standard deviations of all values in each column \cr
    \code{\link{colVars}} \tab Computes variances of all values in each column  \cr
    \code{\link{colSkewness}} \tab Computes skewness of all values in each column  \cr
    \code{\link{colKurtosis}} \tab Computes kurtosis of all values in each column  \cr
    \code{\link{colMaxs}} \tab Computes maxima of all values in each column  \cr
    \code{\link{colMins}} \tab Computes minima of all values in each column  \cr
    \code{\link{colProds}} \tab Computes products of all values in each column \cr
    \code{\link{colStats}} \tab Computes statistics of all values in each column  \cr    
    \code{\link{orderColnames}} \tab Returns ordered column names of a \code{"timeSeries"} \cr
    \code{\link{sortColnames}} \tab Returns alphabetically sorted column names \cr
    \code{\link{sampleColnames}} \tab Returns sampled column names of a \code{"timeSeries"} \cr
    \code{\link{pcaColnames}} \tab Returns PCA correlation ordered column names \cr
    \code{\link{hclustColnames}} \tab Returns hierarchically clustered columnames \cr
    \code{\link{statsColnames}} \tab Returns statisticall rearrange columnames \cr   
    \code{\link{orderStatistics}} \tab Computes order statistics of a \code{"timeSeries"} object \cr   
    \code{\link{rollMean}} \tab Computes rolling means of a \code{"timeSeries"} object \cr
    \code{\link{rollMin}} \tab Computes rolling minima of a \code{"timeSeries"} object \cr
    \code{\link{rollMax}} \tab Computes rolling maxima of a \code{"timeSeries"} object \cr
    \code{\link{rollMedian}} \tab Computes rolling medians of a \code{"timeSeries"} object \cr
    \code{\link{rollStats}} \tab Computes rolling statistics of a \code{"timeSeries"} objectcr \cr
    \code{\link{rowCumsums}} \tab Computes cumulated column sums of a \code{"timeSeries"} \cr   
    \code{\link{smoothLowess}} \tab Smoothes a series with lowess function \cr
    \code{\link{smoothSupsmu}} \tab Smoothes a series with supsmu function \cr
    \code{\link{smoothSpline}} \tab Smoothes a series with smooth.spline function }
}


\section{Misc Functions}{

    \tabular{ll}{  
    
    \code{\link{dummyDailySeries}} \tab Creates a dummy daily \code{"timeSeries"} object \cr
    \code{\link[=isMonthly,timeSeries-method]{isMonthly}} \tab Decides if the series consists of monthly records \cr 
    \code{\link[=isDaily,timeSeries-method]{isDaily}} \tab Decides if the series consists of daily records \cr 
    \code{\link[=isQuarterly,timeSeries-method]{isQuarterly}} \tab Decides if the series consists of Quarterly records \cr 
    \code{\link{description}}  \tab Creates default description string 
    % \code{\link{getArgs}} \tab Extracts arguments from a S4 method
    }

} 

\keyword{package}
\keyword{ts}
