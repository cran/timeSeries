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
# Note if is.na returns a timeSeries objects then we have problem
# with the function quantile...


setMethod("is.na", "timeSeries", function(x)
    setDataPart(x, is.na(getDataPart(x))))


# ------------------------------------------------------------------------------

if(getRversion() >= "3.3.0") {
## unfortunately this will give NA if the data part contains NA,
## since is.unsorted checks for NAs before dispatch
    setMethod("is.unsorted", "timeSeries", function(x, strictly = FALSE)
        callGeneric(x@positions, strictly = strictly))
} else {
    setMethod("is.unsorted", "timeSeries",
               function(x, na.rm = FALSE, strictly = FALSE)
         callGeneric(x@positions, na.rm = na.rm, strictly = strictly))

}

################################################################################

