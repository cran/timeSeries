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
# FUNCTION:                 DESCRIPTION:
#  is.timeSeries             Tests for a 'timeSeries' object
################################################################################


is.timeSeries <-
  function (x)
  {
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Tests for a 'timeSeries' object.
    
    # Arguments:
    #   object - a 'timeSeries' object to be tested.
    
    # Value:
    #   Returns 'TRUE' or 'FALSE' depending on whether its
    #   argument is of 'timeSeries' type or not.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Check:
    ans <- is(x, "timeSeries")
    
    # Return Value:
    ans
  }

# ------------------------------------------------------------------------------


is.signalSeries <-
  function(x)
  {
    !as.logical(length(x@positions))
  }


# ------------------------------------------------------------------------------


# YC: 
# Note if is.na returns a timeSeries object then we have problem
# with the function quantile...
setMethod("is.na", "timeSeries", function(x)
    setDataPart(x, is.na(getDataPart(x)))
)

## 2024-01-12 GNB:
##    TODO: create method for anyNA?
##          The default anyNA works fine (it calls is.na),
##          but a timeSeries method might gain some efficiency/

# ------------------------------------------------------------------------------

# something like this would be needed if is.unsorted again became an internal generic
#if(getRversion() >= "3.3.0") {
#    setGeneric("is.unsorted", signature = "x", useAsDefault = base::is.unsorted)
#}
##
## 2024-01-11 GNB: the notes below need consolidation, I wrote them as I worked on this.
##
##     1. is.unsorted is internal generic, though I didn't find in NEWS R- x.x notes about it
##        changing its status as internal generic.
##
##     2. changed the method to S3
##           setMethod("is.unsorted", "timeSeries",
##                     function(x, na.rm = FALSE, strictly = FALSE)
##                         callGeneric(x@positions, na.rm = na.rm, strictly = strictly))
## 
##     3. since is.unsorted is primitive, it may be better to define an S4 method but the
##        implicit generic created by setMethod has the wrong signature and creates a new
##        function which needs to be exported. So, an explicit setGeneric to limit the
##        dispatch only to 'x', see the example above by a previous maintainer.
##        I haven't tried that.
## 
## 2024-01-12 GNB:
##
##    This works:
is.unsorted.timeSeries <- function(x, na.rm = FALSE, strictly = FALSE) {
    is.unsorted(x@positions, na.rm = na.rm, strictly = strictly)
}
## (GNB: cont.)
##    (in the sense that it is dispatched) but it gives wrong results if there are NA's in
##    the data. Indeed we have
##
##        > base::is.unsorted
##        function (x, na.rm = FALSE, strictly = FALSE) 
##        {
##            if (length(x) <= 1L) 
##                return(FALSE)
##            if (!na.rm && anyNA(x)) 
##                return(NA)
##            if (na.rm && any(ii <- is.na(x))) 
##                x <- x[!ii]
##            .Internal(is.unsorted(x, na.rm, strictly))
##        }
##
##    the internal function is internal generic but before calling that, 'base::is.unsorted'
##    handles the 'NA' cases. The time series method for is.na() works on the data, and so we
##    get wrong result if there are NA's in the time series data.
##
##    The S4 method creates a new generic with default 'base::is.unsorted' so there is no
##    problem there.
##
##    The core problem in my (GNB) opinion is that the timeSeries methods for is.unsorted and
##    sort() have different semantics from that of the method for is.na (the former work on
##    the time stamps, while the latter works on the data).
##
##    So, returning the S4 method. Notice that we do not try to keep is.unsorted internal but
##    rather export the generic so that it is calledand the time series method kiks in for
##    timeSeries objects and avoids the above NA problem.
##
##    Note that we keep also the S3 method, so that if the S4 generic is not visible, the
##    result will be correct at least when there are no NA's in the data.
setMethod("is.unsorted", "timeSeries",
          function(x, na.rm = FALSE, strictly = FALSE)
              callGeneric(x@positions, na.rm = na.rm, strictly = strictly))

# if (getRversion() < "2.8.0") 
# {
#   setMethod("is.unsorted", "timeSeries", function(x, na.rm = FALSE)
#     callGeneric(x@positions, na.rm = na.rm))
# } else {
#   setMethod("is.unsorted", "timeSeries", function(x, na.rm = FALSE, strictly = FALSE)
#     callGeneric(x@positions, na.rm = na.rm, strictly = strictly))
# }


################################################################################
