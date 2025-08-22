
toCampsisElement <- function(json) {
  type <- json$type
  
  if (type=="bolus" || type=="infusion") {
    if (!is.null(json$ii) && !is.null(json$addl)) {
      type <- paste0(type, "_wrapper")
    }  
  }
  
  object <- new(type)
  object <- loadFromJSON(object, JSONElement(json))
  return(object)
}

mapJSONPropertiesToSlot <- function(object, json) {
  json <- json@data
  properties <- names(json)
  properties <- properties[properties != "type"]
  
  for (property in properties) {
    value <- json[[property]]
    if (is.list(value)) {
      value <- unlist(value)
    }
    slot(object, property) <- value
  }
  return(object)
}

#' JSON to Campsis dataset.
#' 
#' @param object empty dataset
#' @param json json element
#' @return Campsis dataset
#' @importFrom jsonlite parse_json
#' 
jsonToCampsisDataset <- function(object, json) {
  json <- json@data
  dataset <- object
  
  if (length(json)==0) {
    return(dataset)
  }
  
  datasetAttributes <- json %>%
    purrr::detect(~.x$type=="arm_attributes")
  
  arms <- json %>%
    purrr::keep(~.x$type=="arm")
  
  # Iterating over arms
  for (arm in arms) {
    armElems <- arm$list
    
    armAttributes <- armElems %>%
      purrr::detect(~.x$type=="arm_attributes")
    
    currentArm <- Arm(subjects=armAttributes$subjects, label=armAttributes$label)
    
    armElems <- armElems %>%
      purrr::keep(~!(.x$type %in% c("arm_attributes")))
    
    for (x in armElems) {
      elem <- toCampsisElement(x)
      currentArm <- currentArm %>%
        campsismod::add(elem)
    }
    dataset <- dataset %>%
      add(currentArm)
  }
  
  # Iteration over elements in dataset
  datasetElems <- json %>%
    purrr::keep(~!(.x$type %in% c("arm", "arm_attributes")))
  
  for (x in datasetElems) {
    elem <- toCampsisElement(x)
    dataset <- dataset %>%
      campsismod::add(elem)
  }
  
  if (!is.null(datasetAttributes)) {
    dataset <- dataset %>%
      setSubjects(datasetAttributes$subjects) %>%
      setLabel(datasetAttributes$label)
  }
  
  return(dataset)
}
