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

\usage{   
rollMonthlyWindows(x, period = "12m", by = "1m") 

rollMonthlySeries(x, period = "12m", by = "1m", FUN, \dots)
countMonthlyRecords(x)
}

\arguments{

  \item{x}{
    a \code{"timeSeries"} object.
  }  
  \item{period,by}{

    character strings specifying the rollling period composed by the
    length of the period and its unit. Examples: \code{"3m"},
    \code{"6m"}, \code{"12m"}, and \code{"24m"} represent quarterly,
    semi-annual, annual and bi-annual shifts, respectively.  It is the
    responsibility of the user to determine proper start of the series.

  } 

  \item{FUN}{
    
    the function for the statistic to be applied. For example,
    \code{colMean} in the case of aggregation.

  }
  \item{\dots}{
    arguments passed to the function \code{FUN}.
  }

}

\details{
  
  \code{rollMonthlySeries} computes the statistics defined by the
  function \code{FUN} over rolling windows, internally computed by the
  function \code{rollMonthlyWindows}.  Note, the periods may be
  overlapping, may be dense, or even may have gaps.
    
  \code{countMonthlyRecords} computes a \code{"timeSeries"} that holds
  the number of records for each month, see examples. The dates are set
  to the end of the month.
    
  \code{rollMonthlyWindows} computes start and end dates for rolling
  time windows. Argument \code{period} specifies the length of the
  periods over which \code{FUN} is applied, while \code{by} gives the
  amount by which the window is shifted. Non-overlapping windows
  correspond to \code{by >= period}. 

}

\value{

  for \code{countMonthlyRecords} and \code{rollMonthlySeries},
  a \code{"timeSeries"} object.
    
  for \code{rollMonthlyWindows}, a list with attribute \code{"control"} keeping
  the \code{start} and \code{end} dates of the series.
  The components of the list are:
  \item{from}{an object from class \code{"timeDate"}.}
  \item{to}{an object from class \code{"timeDate"}.}
    
}

\seealso{
  \code{\link{isMonthly}},
  \code{\link{isRegular}}
}

\examples{
## load Microsoft daily dataset
x <- MSFT 

## count monthly records
head(x)   # 3 obs. for Sep 2000
counts <- countMonthlyRecords(x)
counts

## diy computation of the counts
diy <- rollMonthlySeries(x[ , 1], period = "1m", by = "1m", FUN = NROW)

## difference is only in some attributes (e.g. column names)
all.equal(diy,  counts)

   
## quaterly non-overlapping time periods
windows <- rollMonthlyWindows(counts[-1, ], period = "3m", by = "3m") 
windows
## nicely print results as a data.frame, each row is a time window
data.frame(cbind(FROM = format(windows$from), TO = format(windows$to)))
   
## compute the average number of monthly trading days per quarter
rollMonthlySeries(counts[-1, ], period = "3m", by = "3m", FUN = mean)
}

\keyword{chron}
