#'GenerateTDDataset
#'
#' Version 0.1 
#' 2 Aug 2024
#' Authors: Rosa Gini, Davide Messina
#'
#' @param dt  pair of data.table objects
#' @param id pair of variable names: variable in either dataset where the identifier of the unit of observation is stored
#' @param start_d pair of variable names: variable in either dataset where the start of validity of the record is stored
#' @param end_d pair of variable names: variable in either dataset where the end of validity of the record is stored
#' @param variables pair of lists of variable names: groups of time-dependent variables in either dataset
#' @param default_values pair of lists of values: default value for the variables
 
# supports formats integer, numeric, character, factor, Date, POSIXct, IDate, and logical
 
GenerateTDDataset <- function(dt,
                              id, 
                              start_d,
                              end_d,
                              variables,
                              default_values = list(list(),list()),
                              default_missing_values = list(list(),list()),
                              replace_missing_periods_with_default = c(T,T)
                                   ) {
  
  # libraries
  if (!require("data.table")) install.packages("data.table")
  library(data.table)
  
  # Input validation
  if (!is.list(dt) || length(dt) != 2) {
    stop("Argument 'dt' should be a list of two data.table objects.")
  }
  if (!all(sapply(dt, is.data.table))) {
    stop("Both elements in 'dt' must be data.tables.")
  }
  if (!is.character(id) || length(id) != 2) {
    stop("Argument 'id' should be a character vector of length 2.")
  }
  if (!is.character(start_d) || length(start_d) != 2) {
    stop("Argument 'start_d' should be a character vector of length 2.")
  }
  if (!is.character(end_d) || length(end_d) != 2) {
    stop("Argument 'end_d' should be a character vector of length 2.")
  }
  if (!is.list(variables) || length(variables) != 2 || 
      !all(sapply(variables, function(x) is.list(x) && all(sapply(x, is.character))))) {
    stop("Argument 'variables' should be a list of two lists, each containing character vectors.")
  }
  if (!is.list(default_values) || length(default_values) != 2) {
    stop("Argument 'default_values' should be a list of two lists, even if empty.")
  }
  
  # Supported types
  supported_types <- c("integer", "numeric", "character", "factor", "Date", "POSIXct", "IDate", "logical")
  date_supported_types <- c("IDate", "Date", "integer")
  
  # Initialize list to store classes
  classesvars <- list(list(), list())
  
  # Check that all variables in the first list of 'variables' exist in the first dataset of 'dt'
  first_dt_vars <- names(dt[[1]])
  missing_vars_1 <- setdiff(unlist(variables[[1]]), first_dt_vars)
  if (length(missing_vars_1) > 0) {
    stop("The following variables are not found in the first dataset: ", paste(missing_vars_1, collapse = ", "))
  }
  
  # Check that all variables in the second list of 'variables' exist in the second dataset of 'dt'
  second_dt_vars <- names(dt[[2]])
  missing_vars_2 <- setdiff(unlist(variables[[2]]), second_dt_vars)
  if (length(missing_vars_2) > 0) {
    stop("The following variables are not found in the second dataset: ", paste(missing_vars_2, collapse = ", "))
  }
  
  # Check and validate 'start_d' and 'end_d' classes
  for (i in 1:2) {
    start_class <- class(dt[[i]][[start_d[i]]])
    end_class <- class(dt[[i]][[end_d[i]]])
    if (!start_class %in% date_supported_types) {
      stop(sprintf("The variable '%s' in the %s dataset has an unsupported type: %s. Expected types are of date type: %s.",
                   start_d[i], ifelse(i == 1, "first", "second"), start_class, paste(date_supported_types, collapse = ", ")))
    }
    if (!end_class %in% date_supported_types) {
      stop(sprintf("The variable '%s' in the %s dataset has an unsupported type: %s. Expected types are of date type: %s.",
                   end_d[i], ifelse(i == 1, "first", "second"), end_class, paste(date_supported_types, collapse = ", ")))
    }
  }
  
  # Store and validate classes of variables in the first dataset
  for (var in unlist(variables[[1]])) {
    var_class <- class(dt[[1]][[var]])
    classesvars[[1]][[var]] <- var_class
    if (!var_class %in% supported_types) {
      stop(sprintf("The variable '%s' in the first dataset has an unsupported type: %s. Please recast the variable to one of the supported types: %s.",
                   var, var_class, paste(supported_types, collapse = ", ")))
    }
  }
  
  # Store and validate classes of variables in the second dataset
  for (var in unlist(variables[[2]])) {
    var_class <- class(dt[[2]][[var]])
    classesvars[[2]][[var]] <- var_class
    if (!var_class %in% supported_types) {
      stop(sprintf("The variable '%s' in the second dataset has an unsupported type: %s. Please recast the variable to one of the supported types: %s.",
                   var, var_class, paste(supported_types, collapse = ", ")))
    }
  }
  
  # If -replace_missing_periods_with_default- if T in either dataset, then the corresponding list in the argument -default_values- should be non-empty and contain as many values as the corresponding list in the argument -variables-; moreover, each element in this list should have the same class as the corresponding element in the list in -variables-

  # verify that in either dataset, the period of times between start_d and end_d within values of -id- do not overlap
    
  # Function body
  # Implement the logic for generating the time-dependent dataset.

}
