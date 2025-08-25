
#_______________________________________________________________________________
#----                     observations class                                ----
#_______________________________________________________________________________

checkObservations <- function(object) {
  check1 <- expectOne(object, "compartment")
  check2 <- character()
  if (object@dv %>% length() > 0 && object@dv %>% length() != object@times %>% length()) {
    check2 <- "Slots 'times' and dv' don't have the same length"
  }
  return(c(check1, check2))
}

#' 
#' Observations class.
#' 
#' @slot times any object that implements 
#' @slot compartment compartment index (integer) or name (character)
#' @slot dv observed values, numeric vector (FOR EXTERNAL USE)
#' @export
setClass(
  "observations",
  representation(
    times = "time_vector",
    compartment = "character",
    dv="numeric"
  ),
  contains = "pmx_element",
  prototype = prototype(compartment=as.character(NA), dv=numeric(0)),
  validity = checkObservations
)

#'
#' Create an observations list. Please note that the provided 'times' will 
#' automatically be sorted. Duplicated times will be removed.
#'
#' @param times observation times, numeric vector
#' @param compartment compartment index (integer) or name (character)
#' @return an observations list
#' @export
Observations <- function(times, compartment=NA) {
  if (is(times, "time_vector")) {
    # Do nothing
  } else {
    times <- TimeVector(times)
  }
  return(new("observations", times=times, compartment=as.character(compartment)))
}

setMethod("getName", signature = c("observations"), definition = function(x) {
  return(paste0("OBS [", "TIMES=c(", paste0(as.numeric(x@times), collapse=","), "), ", "CMT=", x@compartment, "]"))
})

#_______________________________________________________________________________
#----                     event_related_observations class                  ----
#_______________________________________________________________________________

setClass(
  "event_related_observations",
  representation(
  ),
  contains = "observations"
)

#'
#' Create an event-related observations list. Please note that the provided 'times' will 
#' automatically be sorted. Duplicated times will be removed.
#'
#' @param times observation times, numeric vector
#' @param compartment compartment index, integer
#' @return observations
#' @keywords internal
EventRelatedObservations <- function(times, compartment=NA) {
  return(new("event_related_observations", times=TimeVector(times), compartment=as.character(compartment)))
}

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

#' @rdname loadFromJSON
setMethod("loadFromJSON", signature=c("observations", "json_element"), definition=function(object, json) {
  if (is.numeric(unlist(json@data$times))) {
    object@times <- TimeVector(unlist(json@data$times))
    json@data$times <- NULL
  }
  object <- mapJSONPropertiesToSlot(object, json)
  return(object)
})

#_______________________________________________________________________________
#----                             sample                                    ----
#_______________________________________________________________________________

#' @rdname sample
setMethod("sample", signature = c("observations", "integer"), definition = function(object, n, ...) {
  args <- list(...)
  config <- processExtraArg(args, name="config", mandatory=TRUE, default=DatasetConfig())
  ids <- processExtraArg(args, name="ids", mandatory=TRUE, default=seq_len(n))
  armID <- processExtraArg(args, name="armID", mandatory=TRUE, default=as.integer(0))
  needsDV <- processExtraArg(args, name="needsDV", mandatory=TRUE, default=FALSE)
  
  if (is.na(object@compartment)) {
    obsCmt <- as.character(config@def_obs_cmt)
  } else {
    obsCmt <- object@compartment
  }
  isEventRelated <- is(object, "event_related_observations")
  times <- as.numeric(object@times)
  
  retValue <- tibble::tibble(
    ID=rep(ids, each=length(times)), ARM=as.integer(armID), TIME=rep(times, n),
    EVID=as.integer(0), MDV=as.integer(0), AMT=as.numeric(NA), CMT=obsCmt, RATE=as.numeric(0), DOSENO=as.integer(NA),
    INFUSION_TYPE=as.integer(NA), EVENT_RELATED=as.integer(isEventRelated)
  )
  if (needsDV) {
    if (object@dv %>% length() > 0) {
      dv <- object@dv
    } else {
      dv <- rep(as.numeric(0),  length(times))
    }
    retValue <- retValue %>% tibble::add_column(DV=rep(dv, n), .before="INFUSION_TYPE")
  }
  return(retValue)
})
