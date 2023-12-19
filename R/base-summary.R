#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  ../../COPYING


## Author: Georgi N. Boshnakov

## setMethod("summary", c(object = "timeSeries"),
##           function(object, alwaysNA = TRUE, ...){
##               start = as.character(start(object))
##               end   = as.character(end(object))
##               
##               ## stats <- cbind(
##               ##     "Min." = colMins(object),
##               ##     "1st Qu." = colQuantiles(object, prob = 0.25, type = 1),
##               ##     "Median"  = colQuantiles(object, prob = 0.50, type = 1),
##               ##     "3rd Qu." = colQuantiles(object, prob = 0.75, type = 1),
##               ##     "Max."    = colMaxs(object)
##               ##     ## , check.names = FALSE
##               ## )
## 
##               stats <- summary(as.matrix(object))
## 
##               attr(stats, "start")     <- start
##               attr(stats, "end")       <- end
##               attr(stats, "nobs")      <- nrow(object)
##               attr(stats, "Format")    <- object@format
##               attr(stats, "FinCenter") <- object@FinCenter
## 
##               class(stats) <- c("timeSeries_summary", class("stats"))
##               stats
##           })

summary.timeSeries <- function(object, ...) {
    ## stats <- cbind(
    ##     "Min." = colMins(object),
    ##     "1st Qu." = colQuantiles(object, prob = 0.25, type = 1),
    ##     "Median"  = colQuantiles(object, prob = 0.50, type = 1),
    ##     "3rd Qu." = colQuantiles(object, prob = 0.75, type = 1),
    ##     "Max."    = colMaxs(object)
    ##     ## , check.names = FALSE
    ## )
    
    stats <- summary(as.matrix(object))
    
    attr(stats, "start")     <- as.character(start(object))
    attr(stats, "end")       <- as.character(end(object))  
    attr(stats, "nobs")      <- nrow(object)
    attr(stats, "Format")    <- object@format
    attr(stats, "FinCenter") <- object@FinCenter
    
    class(stats) <- c("timeSeries_summary", class("stats"))
    stats
}

print.timeSeries_summary <- function(x, quote = FALSE, ...) {
    cat("Start Record:", attr(x, "start")    , "\n")
    cat("End Record:  ", attr(x, "end")      , "\n")
    cat("Observations:", attr(x, "nobs")     , "\n")
    cat("Format:      ", attr(x, "Format")   , "\n")
    cat("FinCenter:   ", attr(x, "FinCenter"), "\n")
    cat("\n")

    class(x) <- class(x)[-1]
    attr(x, "start")     <- 
    attr(x, "end")       <- 
    attr(x, "nobs")      <- 
    attr(x, "Format")    <- 
    attr(x, "FinCenter") <- NULL
    
    print(x, quote = quote, ...)

    invisible(x)
}
