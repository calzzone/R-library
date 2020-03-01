search_for_variable <- function(data, parameter="auto", pattern=NA, type="numeric") {
  var_names <- variable.names(data)
  #data <- data.frame(data)
  #colnames(data) <- var_names
  
  if (is.na(parameter) | parameter == "none" | parameter == 0 | parameter == "") {
    #print("no parameter")
    parameter <- NA
  }
  else if (parameter == "auto") {
    #print("search for parameter")
    locations <- grep(pattern, var_names, ignore.case = T)
    if (length(locations) < 1) parameter <- NA
    else parameter <- locations[1]
    #print(paste("found parameter", parameter))
  }
  else {
    #print (paste("parameter was provided:", parameter))
    if (is.numeric(parameter) )
      parameter <- parameter
    else if (parameter %in% var_names)
      parameter <- which(parameter==var_names)
    else parameter <- NA
  }
  
  
  
  if (is.na(parameter)) return(NA)
  if (!is.numeric(parameter)) return(NA)
  if (!between(parameter, 1, length(var_names))) return(NA)
  
  if(type == "numeric") if (is.numeric(data[[parameter]])) return (parameter)
  if(type == "binary") if (is.factor(data[[parameter]]) && 
                           length(levels( data[[parameter]] )) == 2) return (parameter)
  if(type == "factor") if (is.factor(data[[parameter]])) return (parameter)
  
  return(NA)
  #return (parameter)
}