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


################################################################################
# METHOD:                   SUBSETTING METHODS ON DATA:
#  head,timeSeries           Returns the head of a 'timeSeries' object
#  tail,timeSeries           Returns the tail of a 'timeSeries' object
################################################################################

setGeneric("head")
setGeneric("tail")



head.timeSeries <- 
    function(x, n = 6, recordIDs = FALSE, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the head of a 'timeSeries' object

    # Arguments:
    #   x - a 'timeSeries' object.
    #   n - a single integer. If positive, number of the first n records (rows)
    #        to  be returned. If negative, all but the n first number of 
    #        elements of x are returned.
    #   recordIDs - a logical flag, should the record identification
    #       be shown? By default FALSE.
    #   ... - 

    # Value:
    #   Returns the tail of an object of class 'timeSeries'.
    
    # FUNCTION:

    # Head:
    if (recordIDs & dim(x)[1] == dim(x@recordIDs)[1])
        cbind(head.matrix(x, n = n, ...), head(x@recordIDs, n = n, ...))
    else
        head.matrix(x, n = n, ...)
}

          
# ------------------------------------------------------------------------------


##' @title Returns the tail of a 'timeSeries' object
##'
##' @param x   a 'timeSeries' object.
##' @param n   a single integer. If positive, number of the last n records (rows)
##'       to be returned. If negative, all but the n last number of 
##'       elements of x are returned.    
##' @param recordIDs  a logical flag, should the record identification
##'       be shown? By default FALSE.
##' @param ... 
##'
##' @return
##'   Returns the tail of an object of class 'timeSeries'.
##'
## Martin Maechler:  if("keepnums" %in% names(formals(tail.matrix))) ## R-devel (2020-01)
##  refactored somewhat by GNB;  *TODO:* is a similar thing needed for head.timeSeries?
tail.timeSeries <-
    if(getRversion() >= "4.0.0") {
        function(x, n = 6, recordIDs = FALSE, ...) { 
            if (recordIDs && nrow(x) == nrow(x@recordIDs))
                cbind(tail.matrix(x, n = n, keepnums = FALSE, ...),
                      tail(x@recordIDs, n = n, keepnums = FALSE, ...))
            else
                tail.matrix(x, n = n, keepnums = FALSE, ...)
        }
    } else {
        function(x, n = 6, recordIDs = FALSE, ...) {
            if (recordIDs & dim(x)[1] == dim(x@recordIDs)[1])
                cbind(tail.matrix(x, n = n, addrownums = FALSE, ...),
                      tail(x@recordIDs, n = n, addrownums = FALSE, ...))
            else
                tail.matrix(x, n = n, addrownums = FALSE, ...)
        }
    }

## (2024-01-05) GNB: stop making head() and tail() S4
## setMethod("head", "timeSeries", head.timeSeries)
## setMethod("tail", "timeSeries", tail.timeSeries)

################################################################################
