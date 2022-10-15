\name{monthly}
\alias{monthly}

\alias{countMonthlyRecords}
\alias{rollMonthlyWindows}
\alias{rollMonthlySeries}


\title{Special monthly series}


\description{
    
    Functions and methods dealing with special monthly 
    \code{"timeSeries"} objects.      
}


\details{
  
    \code{countMonthlyRecords} computes a \code{"timeSeries"} that holds
    the number of monthly counts of the records.
    
    \code{rollMonthlyWindows} computes start and end dates for rolling
    time windows.
    
    \code{rollMonthlySeries} computes the statistics defined by the
    function \code{FUN} over a rolling window, internally computed by
    the function \code{rollMonthlyWindows}.  Note, the periods may be
    overlapping, may be dense, or even may have gaps.

}


\usage{   
countMonthlyRecords(x)

rollMonthlyWindows(x, period = "12m", by = "1m") 
rollMonthlySeries(x, period = "12m", by = "1m", FUN, \dots)
}


\arguments{

  \item{x}{
    a \code{"timeSeries"} object.
  }  
  \item{period,by}{

    character strings specifying the rollling period composed by the
    length of the period and its unit. Examples: \code{"3m"}
    \code{"6m"}, \code{"12m"}, and \code{"24m"} represent quarterly,
    semi-annual, annual and bi-annual shifts, respectively.  It is the
    responsibility of the user.  to determine proper start of the series

  } 

  \item{FUN}{
    
    the function for the statistic to be applied. For example,
    \code{colMean} in the case of aggregation.

  }
  \item{\dots}{
    arguments passed to the function \code{FUN}.
  }

}


\value{

  for \code{countMonthlyRecords} and \code{rollMonthlySeries}, a
  \code{"timeSeries"} object.\cr\cr
    
  for \code{rollMonthlyWindows}, a list with attribute
  \code{"control"} keeping the \code{start} and \code{end} dates of the
  series. The components of the list are:
  \item{from}{an object from class \code{"timeDate"}.}
  \item{to}{an object from class \code{"timeDate"}.}
    
}


\examples{
## Load Microsoft Daily Data Set:
   x <- MSFT 
   
## Count Monthly Records -
   counts <- countMonthlyRecords(x)
   counts
   
## Quaterly Non-Overlapping Time Periods -
   windows <- rollMonthlyWindows(counts[-1, ], period = "3m", by = "3m") 
   windows
   
## Nicely Reprint Results as a data.frame -
   data.frame(cbind(FROM=format(windows$from), TO=format(windows$to)))
   
## Compute the average number of monthly trading days per quarter -  
   rollMonthlySeries(counts[-1, ], period = "3m", by = "3m", FUN=mean)
}


\keyword{chron}

