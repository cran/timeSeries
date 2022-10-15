\name{timeSeries-class}
\Rdversion{1.1}
\docType{class}
\alias{timeSeries-class}

% \alias{[,timeSeries,ANY,index_timeSeries-method}
% \alias{[,timeSeries,character,character-method}
% \alias{[,timeSeries,character,index_timeSeries-method}
% \alias{[,timeSeries,character,missing-method}
% \alias{[,timeSeries,index_timeSeries,character-method}
% \alias{[,timeSeries,index_timeSeries,index_timeSeries-method}
% \alias{[,timeSeries,index_timeSeries,missing-method}
% \alias{[,timeSeries,matrix,missing-method}
% \alias{[,timeSeries,missing,character-method}
% \alias{[,timeSeries,missing,index_timeSeries-method}
% \alias{[,timeSeries,missing,missing-method}
% \alias{[,timeSeries,time_timeSeries,ANY-method}
% \alias{[,timeSeries,time_timeSeries,character-method}
% \alias{[,timeSeries,time_timeSeries,index_timeSeries-method}
% \alias{[,timeSeries,time_timeSeries,missing-method}
% \alias{[,timeSeries,timeDate,character-method}
% \alias{[,timeSeries,timeDate,index_timeSeries-method}
% \alias{[,timeSeries,timeDate,missing-method}
% \alias{[,timeSeries,timeSeries,index_timeSeries-method}
% \alias{[,timeSeries,timeSeries,missing-method}

% \alias{[<-,timeSeries,character,ANY-method}
% \alias{[<-,timeSeries,character,missing-method}
% \alias{[<-,timeSeries,timeDate,ANY-method}
% \alias{[<-,timeSeries,timeDate,missing-method}

% \alias{$,timeSeries-method}

% \alias{$<-,timeSeries,ANY-method}
% \alias{$<-,timeSeries,factor-method}
% \alias{$<-,timeSeries,numeric-method}

% \alias{aggregate,timeSeries-method}
% \alias{align,timeSeries-method}
% \alias{apply,timeSeries-method}

% \alias{as.data.frame,timeSeries-method}
% \alias{as.list,timeSeries-method}
% \alias{as.matrix,timeSeries-method}
% \alias{as.ts,timeSeries-method}

% \alias{attach,timeSeries-method}

% \alias{cbind2,ANY,timeSeries-method}
% \alias{cbind2,timeSeries,ANY-method}
% \alias{cbind2,timeSeries,missing-method}
% \alias{cbind2,timeSeries,timeSeries-method}

% \alias{coerce,ANY,timeSeries-method}
% \alias{coerce,character,timeSeries-method}
% \alias{coerce,data.frame,timeSeries-method}
% \alias{coerce,timeSeries,data.frame-method}
% \alias{coerce,timeSeries,list-method}
% \alias{coerce,timeSeries,matrix-method}
% \alias{coerce,timeSeries,ts-method}
% \alias{coerce,ts,timeSeries-method}

% \alias{colCummaxs,timeSeries-method}
% \alias{colCummins,timeSeries-method}
% \alias{colCumprods,timeSeries-method}
% \alias{colCumreturns,timeSeries-method}
% \alias{colCumsums,timeSeries-method}
% \alias{colMeans,timeSeries-method}

% \alias{colnames,timeSeries-method}
% \alias{colnames<-,timeSeries-method}

% \alias{colSums,timeSeries-method}
% \alias{comment,timeSeries-method}
% \alias{comment<-,timeSeries-method}
% \alias{coredata,timeSeries-method}
% \alias{coredata<-,timeSeries,ANY-method}
% \alias{coredata<-,timeSeries,matrix-method}
% \alias{cummax,timeSeries-method}
% \alias{cummin,timeSeries-method}
% \alias{cumprod,timeSeries-method}
% \alias{cumsum,timeSeries-method}
% \alias{cut,timeSeries-method}
% \alias{diff,timeSeries-method}

% \alias{dim,timeSeries-method}
% \alias{dim<-,timeSeries-method}

% \alias{dimnames,timeSeries-method}
% \alias{dimnames<-,timeSeries,list-method}

% \alias{end,timeSeries-method}
% \alias{filter,timeSeries-method}
% \alias{finCenter,timeSeries-method}
% \alias{finCenter<-,timeSeries-method}
% \alias{frequency,timeSeries-method}
% \alias{getDataPart,timeSeries-method}
% \alias{head,timeSeries-method}

\alias{initialize,timeSeries-method}

% \alias{is.na,timeSeries-method}
% \alias{is.unsorted,timeSeries-method}
% \alias{isDaily,timeSeries-method}
% \alias{isMonthly,timeSeries-method}
% \alias{isQuarterly,timeSeries-method}
% \alias{isRegular,timeSeries-method}

% \alias{lag,timeSeries-method}
% \alias{lines,timeSeries-method}
% \alias{median,timeSeries-method}

% \alias{merge,ANY,timeSeries-method}
% \alias{merge,matrix,timeSeries-method}
% \alias{merge,numeric,timeSeries-method}
% \alias{merge,timeSeries,ANY-method}
% \alias{merge,timeSeries,matrix-method}
% \alias{merge,timeSeries,missing-method}
% \alias{merge,timeSeries,numeric-method}
% \alias{merge,timeSeries,timeSeries-method}

% \alias{na.contiguous,timeSeries-method}
% \alias{na.omit,timeSeries-method}

% \alias{names,timeSeries-method}
% \alias{names<-,timeSeries-method}

% \alias{Ops,array,timeSeries-method}
% \alias{Ops,timeSeries,array-method}
% \alias{Ops,timeSeries,timeSeries-method}
% \alias{Ops,timeSeries,ts-method}
% \alias{Ops,timeSeries,vector-method}
% \alias{Ops,ts,timeSeries-method}
% \alias{Ops,vector,timeSeries-method}

% \alias{outlier,timeSeries-method}
% \alias{plot,timeSeries-method}
% \alias{points,timeSeries-method}
% \alias{print,timeSeries-method}
% \alias{quantile,timeSeries-method}
% \alias{rank,timeSeries-method}
% \alias{rbind2,ANY,timeSeries-method}
% \alias{rbind2,timeSeries,ANY-method}
% \alias{rbind2,timeSeries,missing-method}
% \alias{rbind2,timeSeries,timeSeries-method}
% \alias{returns,timeSeries-method}
% \alias{rev,timeSeries-method}
% \alias{rowCumsums,timeSeries-method}

% \alias{rownames,timeSeries-method}
% \alias{rownames<-,timeSeries,ANY-method}
% \alias{rownames<-,timeSeries,timeDate-method}

% \alias{sample,timeSeries-method}
% \alias{scale,timeSeries-method}
% \alias{series,timeSeries-method}
% \alias{series<-,timeSeries,ANY-method}
% \alias{series<-,timeSeries,matrix-method}
% \alias{setDataPart,timeSeries-method}
% \alias{show,timeSeries-method}
% \alias{sort,timeSeries-method}
% \alias{start,timeSeries-method}
% \alias{str,timeSeries-method}
% \alias{t,timeSeries-method}
% \alias{tail,timeSeries-method}
% \alias{time,timeSeries-method}
% \alias{window,timeSeries-method}


\title{Class 'timeSeries' in package timeSeries}

\description{

  Class \code{"timeSeries"} in package timeSeries.

}

\section{Objects from the Class}{

  The main functions for creating objects from class \code{"timeSeries"}
  \code{\link{timeSeries}} and \code{\link{as.timeSeries}}.

  Objects can also be created by calls of the form
  \code{new("timeSeries", .Data, units, positions, format, FinCenter,
  recordIDs, title, documentation)} but this is not recommended for
  routine work. 

}
\section{Slots}{

  The structure of the \code{"timeSeries"} objects should, in general,
  be considered internal. The accessor functions to get and set
  the components should be used to get and set values of the slots.

  \describe{
    \item{\code{.Data}:}{

      Object of class \code{"matrix"} containing the data, one column
      for each variable.

    }
    \item{\code{units}:}{
      
      Object of class \code{"character"}, the unit (or variable, or
      column) names of the time series object.

    }
    \item{\code{positions}:}{

      Object of class \code{"numeric"}, the datetime stamps. If the time
      series doesn't have datetime stamps, then \code{positions} is of
      length zero.

    }
    \item{\code{format}:}{

      Object of class \code{"character"}, a datetime format (such as
      \code{"\%Y-\%m-\%d"}). if there are no time stamps \code{"format"} is
      equal to \code{"counts"}.

    }
    \item{\code{FinCenter}:}{

      Object of class \code{"character"}, the financial center.

    }
    \item{\code{recordIDs}:}{
      Object of class \code{"data.frame"} ~~
    }
    \item{\code{title}:}{
      Object of class \code{"character"}, a title for printing.
    }
    \item{\code{documentation}:}{

      Object of class \code{"character"}, by default it is set to the
      current date.

    }
  }
}
\section{Extends}{
Class \code{"\linkS4class{structure}"}, from data part.
Class \code{"\linkS4class{vector}"}, by class "structure", distance 2, with explicit coerce.
}

\section{Methods}{
  
  Below is a list of the methods that have \code{"timeSeries"} in their
  signature. It can be useful for technical purposes, for example in
  reporting bugs but most methods that need explanation are documented
  with the corresponding functions and looking at their help pages is recommended.

  There are short explanations for Methods for functions that are not supposed to be
  called directly. 
  
  \describe{
    \item{[}{\code{signature(x = "timeSeries", i = "ANY", j = "index_timeSeries")}: ... }
    \item{[}{\code{signature(x = "timeSeries", i = "character", j = "character")}: ... }
    \item{[}{\code{signature(x = "timeSeries", i = "character", j = "index_timeSeries")}: ... }
    \item{[}{\code{signature(x = "timeSeries", i = "character", j = "missing")}: ... }
    \item{[}{\code{signature(x = "timeSeries", i = "index_timeSeries", j = "character")}: ... }
    \item{[}{\code{signature(x = "timeSeries", i = "index_timeSeries", j = "index_timeSeries")}: ... }
    \item{[}{\code{signature(x = "timeSeries", i = "index_timeSeries", j = "missing")}: ... }
    \item{[}{\code{signature(x = "timeSeries", i = "matrix", j = "missing")}: ... }
    \item{[}{\code{signature(x = "timeSeries", i = "missing", j = "character")}: ... }
    \item{[}{\code{signature(x = "timeSeries", i = "missing", j = "index_timeSeries")}: ... }
    \item{[}{\code{signature(x = "timeSeries", i = "missing", j = "missing")}: ... }
    \item{[}{\code{signature(x = "timeSeries", i = "time_timeSeries", j = "ANY")}: ... }
    \item{[}{\code{signature(x = "timeSeries", i = "time_timeSeries", j = "character")}: ... }
    \item{[}{\code{signature(x = "timeSeries", i = "time_timeSeries", j = "index_timeSeries")}: ... }
    \item{[}{\code{signature(x = "timeSeries", i = "time_timeSeries", j = "missing")}: ... }
    \item{[}{\code{signature(x = "timeSeries", i = "timeDate", j = "character")}: ... }
    \item{[}{\code{signature(x = "timeSeries", i = "timeDate", j = "index_timeSeries")}: ... }
    \item{[}{\code{signature(x = "timeSeries", i = "timeDate", j = "missing")}: ... }
    \item{[}{\code{signature(x = "timeSeries", i = "timeSeries", j = "index_timeSeries")}: ... }
    \item{[}{\code{signature(x = "timeSeries", i = "timeSeries", j = "missing")}: ... }

    \item{[<-}{\code{signature(x = "timeSeries", i = "character", j = "ANY")}: ... }
    \item{[<-}{\code{signature(x = "timeSeries", i = "character", j = "missing")}: ... }
    \item{[<-}{\code{signature(x = "timeSeries", i = "timeDate", j = "ANY")}: ... }
    \item{[<-}{\code{signature(x = "timeSeries", i = "timeDate", j = "missing")}: ... }

    \item{$}{\code{signature(x = "timeSeries")}: ... }

    \item{$<-}{\code{signature(x = "timeSeries", value = "ANY")}: ... }
    \item{$<-}{\code{signature(x = "timeSeries", value = "factor")}: ... }
    \item{$<-}{\code{signature(x = "timeSeries", value = "numeric")}: ... }

    \item{aggregate}{\code{signature(x = "timeSeries")}: ... }
    \item{align}{\code{signature(x = "timeSeries")}: ... }
    \item{apply}{\code{signature(X = "timeSeries")}: ... }
    \item{as.data.frame}{\code{signature(x = "timeSeries")}: ... }
    \item{as.list}{\code{signature(x = "timeSeries")}: ... }
    \item{as.matrix}{\code{signature(x = "timeSeries")}: ... }
    \item{as.ts}{\code{signature(x = "timeSeries")}: ... }
    \item{attach}{\code{signature(what = "timeSeries")}: ... }
    
    \item{cbind2}{\code{signature(x = "ANY", y = "timeSeries")}: ... }
    \item{cbind2}{\code{signature(x = "timeSeries", y = "ANY")}: ... }
    \item{cbind2}{\code{signature(x = "timeSeries", y = "missing")}: ... }
    \item{cbind2}{\code{signature(x = "timeSeries", y = "timeSeries")}: ... }

    \item{coerce}{\code{signature(from = "ANY", to = "timeSeries")} }
    \item{coerce}{\code{signature(from = "character", to = "timeSeries")} }
    \item{coerce}{\code{signature(from = "data.frame", to = "timeSeries")} }
    \item{coerce}{\code{signature(from = "timeSeries", to = "data.frame")} }
    \item{coerce}{\code{signature(from = "timeSeries", to = "list")}: }
    \item{coerce}{\code{signature(from = "timeSeries", to = "matrix")} }
    \item{coerce}{\code{signature(from = "timeSeries", to = "ts")}: }
    \item{coerce}{\code{signature(from = "ts", to = "timeSeries")}:
      \code{coerce} should not be called directly.
      Use \code{as(object, "target_class")} instead.
    }

    \item{colCummaxs}{\code{signature(x = "timeSeries")}: ... }
    \item{colCummins}{\code{signature(x = "timeSeries")}: ... }
    \item{colCumprods}{\code{signature(x = "timeSeries")}: ... }
    \item{colCumreturns}{\code{signature(x = "timeSeries")}: ... }
    \item{colCumsums}{\code{signature(x = "timeSeries")}: ... }
    \item{colMeans}{\code{signature(x = "timeSeries")}: ... }

    \item{colnames}{\code{signature(x = "timeSeries")}: ... }
    \item{colnames<-}{\code{signature(x = "timeSeries")}: ... }

    \item{colSums}{\code{signature(x = "timeSeries")}: ... }
    \item{comment}{\code{signature(x = "timeSeries")}: ... }
    \item{comment<-}{\code{signature(x = "timeSeries")}: ... }

    \item{coredata}{\code{signature(x = "timeSeries")}: ... }
    \item{coredata<-}{\code{signature(x = "timeSeries", value = "ANY")}: ... }
    \item{coredata<-}{\code{signature(x = "timeSeries", value = "matrix")}: ... }

    \item{cummax}{\code{signature(x = "timeSeries")}: ... }
    \item{cummin}{\code{signature(x = "timeSeries")}: ... }
    \item{cumprod}{\code{signature(x = "timeSeries")}: ... }
    \item{cumsum}{\code{signature(x = "timeSeries")}: ... }
    \item{cut}{\code{signature(x = "timeSeries")}: ... }
    \item{diff}{\code{signature(x = "timeSeries")}: ... }

    \item{dim}{\code{signature(x = "timeSeries")}: ... }
    \item{dim<-}{\code{signature(x = "timeSeries")}: ... }
    \item{dimnames}{\code{signature(x = "timeSeries")}: ... }
    \item{dimnames<-}{\code{signature(x = "timeSeries", value = "list")}: ... }

    \item{end}{\code{signature(x = "timeSeries")}: ... }
    \item{filter}{\code{signature(x = "timeSeries")}: ... }
    \item{finCenter}{\code{signature(x = "timeSeries")}: ... }
    \item{finCenter<-}{\code{signature(x = "timeSeries")}: ... }
    \item{frequency}{\code{signature(x = "timeSeries")}: ... }
    \item{getDataPart}{\code{signature(object = "timeSeries")}: ... }
    \item{head}{\code{signature(x = "timeSeries")}: ... }
    \item{initialize}{\code{signature(.Object = "timeSeries")}:

      don't call \code{"initialize"}, call \code{new("timeSeries", ...)}
      instead. Even better, call \code{timeSeries}.

    }
    \item{is.na}{\code{signature(x = "timeSeries")}: ... }
    \item{is.unsorted}{\code{signature(x = "timeSeries")}: ... }
    \item{isDaily}{\code{signature(x = "timeSeries")}: ... }
    \item{isMonthly}{\code{signature(x = "timeSeries")}: ... }
    \item{isQuarterly}{\code{signature(x = "timeSeries")}: ... }
    \item{isRegular}{\code{signature(x = "timeSeries")}: ... }
    \item{lag}{\code{signature(x = "timeSeries")}: ... }
    \item{lines}{\code{signature(x = "timeSeries")}: ... }
    \item{median}{\code{signature(x = "timeSeries")}: ... }

    \item{merge}{\code{signature(x = "ANY", y = "timeSeries")}: ... }
    \item{merge}{\code{signature(x = "matrix", y = "timeSeries")}: ... }
    \item{merge}{\code{signature(x = "numeric", y = "timeSeries")}: ... }
    \item{merge}{\code{signature(x = "timeSeries", y = "ANY")}: ... }
    \item{merge}{\code{signature(x = "timeSeries", y = "matrix")}: ... }
    \item{merge}{\code{signature(x = "timeSeries", y = "missing")}: ... }
    \item{merge}{\code{signature(x = "timeSeries", y = "numeric")}: ... }
    \item{merge}{\code{signature(x = "timeSeries", y = "timeSeries")}: ... }

    \item{na.contiguous}{\code{signature(object = "timeSeries")}: ... }
    \item{na.omit}{\code{signature(object = "timeSeries")}: ... }
    \item{names}{\code{signature(x = "timeSeries")}: ... }
    \item{names<-}{\code{signature(x = "timeSeries")}: ... }

    \item{Ops}{\code{signature(e1 = "array", e2 = "timeSeries")}: ... }
    \item{Ops}{\code{signature(e1 = "timeSeries", e2 = "array")}: ... }
    \item{Ops}{\code{signature(e1 = "timeSeries", e2 = "timeSeries")}: ... }
    \item{Ops}{\code{signature(e1 = "timeSeries", e2 = "ts")}: ... }
    \item{Ops}{\code{signature(e1 = "timeSeries", e2 = "vector")}: ... }
    \item{Ops}{\code{signature(e1 = "ts", e2 = "timeSeries")}: ... }
    \item{Ops}{\code{signature(e1 = "vector", e2 = "timeSeries")}: ... }

    \item{outlier}{\code{signature(x = "timeSeries")}: ... }
    \item{plot}{\code{signature(x = "timeSeries")}: ... }
    \item{points}{\code{signature(x = "timeSeries")}: ... }
    \item{print}{\code{signature(x = "timeSeries")}: ... }
    \item{quantile}{\code{signature(x = "timeSeries")}: ... }
    \item{rank}{\code{signature(x = "timeSeries")}: ... }

    \item{rbind2}{\code{signature(x = "ANY", y = "timeSeries")}: ... }
    \item{rbind2}{\code{signature(x = "timeSeries", y = "ANY")}: ... }
    \item{rbind2}{\code{signature(x = "timeSeries", y = "missing")}: ... }
    \item{rbind2}{\code{signature(x = "timeSeries", y = "timeSeries")}: ... }

    \item{returns}{\code{signature(x = "timeSeries")}: ... }
    \item{rev}{\code{signature(x = "timeSeries")}: ... }
    \item{rowCumsums}{\code{signature(x = "timeSeries")}: ... }

    \item{rownames}{\code{signature(x = "timeSeries")}: ... }
    \item{rownames<-}{\code{signature(x = "timeSeries", value = "ANY")}: ... }
    \item{rownames<-}{\code{signature(x = "timeSeries", value = "timeDate")}: ... }

    \item{sample}{\code{signature(x = "timeSeries")}: ... }
    \item{scale}{\code{signature(x = "timeSeries")}: ... }
    \item{series}{\code{signature(x = "timeSeries")}: ... }
    \item{series<-}{\code{signature(x = "timeSeries", value = "ANY")}: ... }
    \item{series<-}{\code{signature(x = "timeSeries", value = "matrix")}: ... }
    \item{setDataPart}{\code{signature(object = "timeSeries")}: ... }
    \item{show}{\code{signature(object = "timeSeries")}: ... }
    \item{sort}{\code{signature(x = "timeSeries")}: ... }
    \item{start}{\code{signature(x = "timeSeries")}: ... }
    \item{str}{\code{signature(object = "timeSeries")}: ... }
    \item{t}{\code{signature(x = "timeSeries")}: ... }
    \item{tail}{\code{signature(x = "timeSeries")}: ... }
    \item{time}{\code{signature(x = "timeSeries")}: ... }
    \item{window}{\code{signature(x = "timeSeries")}: ... }
  }
}

\seealso{

  \code{\link{timeSeries}} and
  \code{\link{as.timeSeries}} for creating and converting to
  \code{"timeSeries"},

  \code{\link{readSeries}} for importing from a text file,

  \code{\link{dummyDailySeries}} for creation of dummy daily and monthly time series,

  \code{\link{as.matrix}}, \code{\link{time}}, \code{\link{finCenter}},
  \code{\link{getUnits}}, \code{\link{dim}}, \code{\link{start}},
  etc., for accessing properties of the time series.
  
}
\examples{
## see the help page for timeSeries()
showClass("timeSeries")
}
\keyword{classes}
