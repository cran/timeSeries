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
# FUNCTION:                        DESCRIPTION:
#  series,timeSeries                Get data slot from a'timeSeries'  
#  series<-,timeSeries,ANY          Set new data slot to a 'timeSeries'  
#  series<-,timeSeries,matrix       Set new data slot to a 'timeSeries'  
# SYNONYMES:                       DESCRIPTION:
#  coredata,timeSeries              Get data slot from a'timeSeries'  
#  coredata<-,timeSeries,ANY        Set new data slot to a 'timeSeries'  
#  coredata<-,timeSeries,matrix     Set new data slot to a 'timeSeries'  
################################################################################

# ------------------------------------------------------------------------------
## 2022-10-08 GNB:
##
##   TODO: In principle, we could just not define 'coredata<-' generic and do:
##
##       "coredata<-" <- "series<-"
##
##       but this doesn't seem desirable since 'coredata<-' may be exported by
##       other packages, too (e.g., zoo). Maybe, do it the other way round:
##       define the methods for 'coredata<-' and do
## 
##       "series<-" <- "coredata<-"
##
##       This may have the analogous problem since other packages may rely on a
##       generic 'series<-'. Admittedly, this is far less likely.
## 2023-05-27 GNB: renaming from .series_assign <-
'coredata<-.timeSeries' <-
    function(x, value)
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi

    # Description:
    #    Assign the series Data to a timeSeries object.

    # Arguments:
    #   object - a 'timeSeries' object

    # Value:
    #    Assign to be assign as series Data of a timeSeries.

    # FUNCTION:

    ## 2023-05-27 GNB: added this when converted the two S4 methods into a
    ##                 single S3 one.
    if(class(value)[1] != "matrix")
        value <- as.matrix(value)
    
    # if value same dimension as time series
    # we we can assign the value directly to @.Data
    # This can speed up math Ops significantly
    if (identical(dim(x), dim(value))) {
        x@.Data <- value
        if (!is.null(cn <- colnames(value)))
            colnames(x) <- cn
        return(x)
    }

    if (is.null(charvec <- rownames(value)))
        charvec <- rownames(x)
    if (is.null(units <- colnames(value)))
        units <- colnames(value)

    # now that we have charvec and units, better to remove
    # dimnames of value to avoid problems
    attr(value, "dimnames") <- NULL

    if (!identical(length(units), NCOL(value)))
        units <- NULL

    # if now same dim , drop charvec and returns .signalSeries
    if (!identical(length(charvec), NROW(value)))
        return(.signalSeries(value, units))

    format <- x@format
    zone <- FinCenter <- finCenter(x)
    title <- x@title
    documentation <- x@documentation
    recordIDs <-
        if (identical(NROW(x), NROW(value)))
            x@recordIDs
        else
            data.frame()

    # Return Value:
    timeSeries(data = value,
        charvec = charvec,
        units = units,
        format = format,
        zone = zone,
        FinCenter = FinCenter,
        recordIDs = recordIDs,
        title = title)
}

# ------------------------------------------------------------------------------

setMethod("series", "timeSeries", 
    function(x)
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi

    # Description:
    #    Returns the series Data of an ordered data object.

    # Arguments:
    #   x - a 'timeSeries' object

    # Value:
    #    Returns an object of class 'matrix'.

    # FUNCTION:

    # Get Data Slot:
    ans <- as.matrix(x)

    # Return Value:
    ans
}
)

# ------------------------------------------------------------------------------

setMethod("series<-", signature(x = "timeSeries", value = "ANY"), 
    function(x, value)
{
    # A function implemented by Yohan Chalabi
    
    # Return Value:
    callGeneric(x, as(value, "matrix"))
}
)

setMethod("series<-", signature(x = "timeSeries", value = "matrix"),
          `coredata<-.timeSeries`)

################################################################################
# COREDATA SYNONYM

## GNB: replacing the S4 generic coredata and its method with an S3 method,
##      which is exported and registered directly as a method for zoo::coredata.
##
##      The S4 coredata() in 'timeSeries' was not exported although is method
##      was 'seen' by 'zoo::coredata' when zoo was attached. I suspect that that
##      was by chance, not as a design in the S3/S4 methods handling in R. Of
##      course, coredata() was only visible when zoo was attached (or xts which
##      exports it).

## setMethod("coredata", "timeSeries", 
##     function(x)
## {
##     # A function implemented by Diethelm Wuertz and Yohan Chalabi
## 
##     # Description:
##     #    Returns the series Data of an ordered data object.
## 
##     # Arguments:
##     #   x - a 'timeSeries' object
## 
##     # Value:
##     #    Returns an object of class 'matrix'.
## 
##     # FUNCTION:
## 
##     # Get Data Slot:
##     ans <- as.matrix(x)
## 
##     # Return Value:
##     ans
## })

coredata.timeSeries <- function(x) as.matrix(x)

# ------------------------------------------------------------------------------

## GNB: replacing the S4 generic 'coredata<-' and its methods with an S3 method,
##      which is exported and registered directly as a method for zoo::coredata<-.
##
##      Note that although the S4 methods were seen when zoo was loaded, they
##      didn't work properly since they dispatch on two arguments, while the
##      function is S3.

## setMethod("coredata<-", signature(x = "timeSeries", value = "ANY"), 
##     function(x, value)
## {
##     # A function implemented by Diethelm Wuertz and Yohan Chalabi
##     
##     # Return Value:
##     callGeneric(x, as(value, "matrix"))
## })
## 
## setMethod("coredata<-", signature(x = "timeSeries", value = "matrix"),
##           .series_assign )
