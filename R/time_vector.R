
#_______________________________________________________________________________
#----                         Time vector interface                         ----
#_______________________________________________________________________________

#' 
#' Time vector class.
#' 
#' @export
setClass(
  "time_vector",
  representation(
  ),
  contains="numeric"
)

#' 
#' Instantiate a new time vector
#' 
#' @param x time vector, numeric
#' @return a time vector
#' @export
TimeVector <- function(x) {
  return(new("time_vector", base::sort(unique(x))))
}

#_______________________________________________________________________________
#----                         Time sequence class                           ----
#_______________________________________________________________________________

#' 
#' Time sequence class.
#' 
#' @export
setClass(
  "time_sequence",
  representation(
    start="numeric",
    end="numeric",
    by="numeric"
  ),
  contains="time_vector"
)

#' 
#' Instantiate a new time sequence.
#' 
#' @param start starting value
#' @param end maximal value
#' @param by increment of the sequence
#' @return a sequence
#' @export
TimeSequence <- function(start, end, by) {
  return(new("time_sequence", start=start, end=end, by=by))
}

#_______________________________________________________________________________
#----                            as.numeric                                 ----
#_______________________________________________________________________________

setMethod("as.numeric", signature=c("time_vector"), definition=function(x) {
  return(base::sort(unique(x)))
})

setMethod("as.numeric", signature=c("time_sequence"), definition=function(x) {
  return(seq(from=x@start, to=x@end, by=x@by))
})

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("time_sequence", "json_element"), definition=function(object, json) {
  object <- mapJSONPropertiesToSlot(object, json)
  return(object)
})
