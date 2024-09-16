\name{readSeries}
\alias{readSeries}


\title{Read a 'timeSeries' from a text file}

\description{

  Reads a file in table format and creates a \code{"timeSeries"} object
  from it. The first column of the table must hold the timestamps.
    
}

\usage{
readSeries(file, header = TRUE, sep = ";", zone = "", 
    FinCenter = "", format, \dots)
}

\arguments{

  \item{file}{

    the filename of a spreadsheet dataset from which to import 
    the data records.
    
  }
  \item{header}{

    a logical value indicating whether the file contains the
    names of the variables as its first line.  If missing, the
    value is determined from the file format: 'header' is set to
    'TRUE' if and only if the first row contains one fewer fields
    than the number of columns.

  }
  \item{sep}{
    
    the field seperator used in the spreadsheet file to separate
    columns, by default \code{";"}. If \code{sep = ";"} and reading the
    series fails, then the reading is automatically repeated with
    \code{sep=","}.

  }
  \item{zone}{

    the time zone or financial center where the data were recorded.  By
    default \code{zone = ""} which is short for GMT.

  }  
  \item{FinCenter}{

    a character with the the location of the financial center named as
    "continent/city".

  }
  \item{format}{
    
    a character string with the format in POSIX notation specifying the
    timestamp format. The format has not to be specified if the
    first column in the file has the timestamp format specifier,
    e.g. "\%Y-\%m-\%d" for the short ISO 8601 format.
      
  }  
  \item{\dots}{
    
    Additional arguments passed to \code{read.table()} which is used to
    read the file.

  }
}


\details{

  The file is imported with \code{\link{read.table}}. Note the different
  default for argument \code{"sep"}.

  The first column of the table must hold the timestamps. Format of the
  timestamps can be either specified in the header of the first column
  or by the \code{format} argument.
    
}

\value{
  an object of class \code{"timeSeries"}
}

\seealso{
  \code{\link{as.timeSeries}},
  \code{\link{timeSeries}},
  \code{\link{dummyMonthlySeries}},
  \code{\link{dummyDailySeries}}
}

\examples{
## full path to an example file
fn <- system.file("extdata/msft.csv", package = "timeSeries")
## first few lines of the file
readLines(fn, n = 5)

## import the file
msft <- readSeries(fn)
head(msft)

## is msft the same as the data object MSFT?
all.equal(msft, MSFT) 
## ... almost, except for slot 'documentation'
c(msft@documentation, MSFT@documentation)
## actually, all.equal() says 'attribute', not slot. this is ok too:
c(attr(MSFT, "documentation"), attr(msft, "documentation"))
## make 'documentation' equal, here "", and compare again:
msft@documentation <- ""
all.equal(msft, MSFT) # TRUE
}
