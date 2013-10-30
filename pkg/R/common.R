## --------------------------------------------------------------------------
##
## Common auxiliary functions
##
## --------------------------------------------------------------------------

unuran.distr.class <- function (unr)
  ## ------------------------------------------------------------------------
  ## Get distribution class of UNU.RAN generator object 'unr'.
  ## ------------------------------------------------------------------------
  ## unr    : object of class "unuran"
  ## ------------------------------------------------------------------------
  ## return:
  ## string that contains name of class
  ## ------------------------------------------------------------------------
{
  unuran.details(unr, show=FALSE, return.list=TRUE, debug=FALSE)$distr.class
}

## --------------------------------------------------------------------------

get.runtime <- function(time)
        ## ------------------------------------------------------------------
        ## Get (extract) runtime.
        ## ------------------------------------------------------------------
        ## time: object of class "proc_time" (as returned by system.time()) 
        ## ------------------------------------------------------------------
        ## return:
        ## runtime (numeric)
        ## ------------------------------------------------------------------

{
        if (! is(time, "proc_time"))
                stop("invalid object")
        
        if (!is.na(time[4L])) 
                time[1L] <- time[1L] + time[4L]
        if (!is.na(time[5L])) 
                time[2L] <- time[2L] + time[5L]

        ## elapsed time
        ## time[3L]

        ## user + system time
        time[1L] + time[2L]
}

## --- End ------------------------------------------------------------------
