#'GenerateTDDataset
#'
#' Version 0.1 
#' 2 Aug 2024
#' Authors: Rosa Gini, Davide Messina
#'
#' @param dt  pair of data.table objects
#' @param id_vars pair of variable names: variable in either dataset where the identifier of the unit of observation is stored
#' @param start_d_vars pair of variable names: variable in either dataset where the start of validity of the record is stored
#' @param end_vars pair of variable names: variable in either dataset where the end of validity of the record is stored
#' @param variables pair of lists of variable names: groups of time-dependent variables in either dataset
#' @param retain_periods_unobserved: pair of logicals (default: T): specified whether periods unobserved for either dataset must be retained 
#' @param replace_missing_periods_with_default: pair of logicals (default: F): specifies whether periods unobserved must be considered observed with default values in the variables (to be stored in the list of lists default_values). if this is F, periods unobserved are observed with NA values in the variables
#' @param default_values pair of lists of values: default value for the variables
#' 
# supports formats integer, numeric, character, factor, Date, POSIXct, IDate, and logical
 
GenerateTDDataset <- function(dt,
                              id_vars, 
                              start_d_vars,
                              end_d_vars,
                              variables,
                              retain_periods_unobserved  = c(T,T),
                              replace_missing_periods_with_default = c(F,F),
                              default_values = list(list(),list())
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
  if (!is.character(id_vars) || length(id_vars) != 2) {
    stop("Argument 'id_vars' should be a character vector of length 2.")
  }
  if (!is.character(start_d_vars) || length(start_d_vars) != 2) {
    stop("Argument 'start_d_vars' should be a character vector of length 2.")
  }
  if (!is.character(end_d_vars) || length(end_d_vars) != 2) {
    stop("Argument 'end_d_vars' should be a character vector of length 2.")
  }
  if (!is.list(variables) || length(variables) != 2 || 
      !all(sapply(variables, function(x) is.list(x) && all(sapply(x, is.character))))) {
    stop("Argument 'variables' should be a list of two lists, each containing character vectors.")
  }
  if (!is.list(default_values) || length(default_values) != 2) {
    stop("Argument 'default_values' should be a list of two lists, even if empty.")
  }
  
  # Supported types
  supported_types <- c("integer", "numeric", "character", "factor", "Date",  "IDate", "logical")
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
    start_class <- class(dt[[i]][[start_d_vars[i]]])
    end_class <- class(dt[[i]][[end_d_vars[i]]])
    if (!start_class %in% date_supported_types) {
      stop(sprintf("The variable '%s' in the %s dataset has an unsupported type: %s. Expected types are of date type: %s.",
                   start_d_vars[i], ifelse(i == 1, "first", "second"), start_class, paste(date_supported_types, collapse = ", ")))
    }
    if (!end_class %in% date_supported_types) {
      stop(sprintf("The variable '%s' in the %s dataset has an unsupported type: %s. Expected types are of date type: %s.",
                   end_d_vars[i], ifelse(i == 1, "first", "second"), end_class, paste(date_supported_types, collapse = ", ")))
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
  
  # Check 'retain_periods_unobserved' and 'replace_missing_periods_with_default' are logicals of length 2
  if (!is.logical(retain_periods_unobserved) || length(retain_periods_unobserved) != 2) {
    stop("Argument 'retain_periods_unobserved' should be a logical vector of length 2.")
  }
  if (!is.logical(replace_missing_periods_with_default) || length(replace_missing_periods_with_default) != 2) {
    stop("Argument 'replace_missing_periods_with_default' should be a logical vector of length 2.")
  }
  
  # Check 'default_values'
  for (i in 1:2) {
    if (replace_missing_periods_with_default[i]) {
      if (length(default_values[[i]]) != length(variables[[i]])) {
        stop(sprintf("Length of 'default_values' for the %s dataset must match the length of 'variables' list.", ifelse(i == 1, "first", "second")))
      }
      for (j in seq_along(default_values[[i]])) {
        if (class(default_values[[i]][[j]]) != classesvars[[i]][[variables[[i]][j]]]) {
          stop(sprintf("Default value for variable '%s' in the %s dataset must be of class '%s'.",
                       variables[[i]][j], ifelse(i == 1, "first", "second"), classesvars[[i]][[variables[[i]][j]]]))
        }
      }
    }
  }
  
  # # TO BE COMPLETED: Check for overlapping periods in either dataset using foverlaps
  # for (i in 1:2) {
  #   # Set keys using the dynamic column names
  #   setkeyv(dt[[i]], c(id_vars[i], start_d_vars[i], end_d_vars[i]))
  #   
  #   # Find overlaps within the dataset
  #   overlaps <- foverlaps(dt[[i]], dt[[i]], 
  #                         by.x = c(id_vars[i], start_d_vars[i], end_d_vars[i]), 
  #                         by.y = c(id_vars[i], start_d_vars[i], end_d_vars[i]),
  #                         nomatch = NULL)
  #   
  #   # Print overlaps to debug
  #   print(overlaps)
  #   
  #   # Filter out self-overlaps (where the observation overlaps with itself)
  #   TO DO!!!!
  # }
    
  ################################################
  # Function body
  #

  # Cast logical variables to integers and Dates to IDate
  for (i in 1:2) {
    for (var in unlist(variables[[i]])) {
      var_class <- class(dt[[i]][[var]])
      if (var_class == "logical") {
        dt[[i]][[var]] <- as.integer(dt[[i]][[var]])
      } else if (var_class %in% c("Date")) {
        dt[[i]][[var]] <- as.IDate(dt[[i]][[var]])
      }
    }
  }

  # Initialize lists for extreme_value and placeholder_value
  extreme_value <- list(list(), list())
  placeholder_value <- list(list(), list())

  # Calculate extreme_value and placeholder_value for each variable
  for (i in 1:2) {
    for (thisvar in unlist(variables[[i]])) {
      # Find the minimum value of non-missing values, except for charcters where we pick the maximum
      non_missing_vals <- dt[[i]][!is.na(get(thisvar)), get(thisvar)]
      if (length(non_missing_vals) > 0) {
        var_class <- class(dt[[i]][[thisvar]])
        if (var_class != "character"){
          min_val <- min(non_missing_vals, na.rm = TRUE)
          extreme_value[[i]][[thisvar]] <- min_val
          placeholder_value[[i]][[thisvar]] <- min_val - 1
        }else{
          max_val <- max(non_missing_vals, na.rm = TRUE)
          extreme_value[[i]][[thisvar]] <- max_val
          placeholder_value[[i]][[thisvar]] <- paste0(max_val,"Z")
          }
      } else {
        extreme_value[[i]][[thisvar]] <- NA
        placeholder_value[[i]][[thisvar]] <- NA
      }
    }
  }

  # Replace missing values with placeholder_value
  for (i in 1:2) {
    for (thisvar in unlist(variables[[i]])) {
      if (!is.na(placeholder_value[[i]][[thisvar]])) {
        dt[[i]][is.na(get(thisvar)), (thisvar) := placeholder_value[[i]][[thisvar]]]
      }
    }
  }
  
  # Add logical variable 'observed_1' and 'observed_2'
  dt[[1]][, observed_1 := TRUE]
  dt[[2]][, observed_2 := TRUE]
  
  
  # fill the gaps between intervals of the same id
  for (i in 1:2) {
    # Set keys using the dynamic column names
    setkeyv(dt[[i]], c(id_vars[i], start_d_vars[i], end_d_vars[i]))
    
    # Define end_prev and identify gaps
    dt[[i]][, end_prev := shift(get(end_d_vars[i]), type = "lag"), by = c(id_vars[i])]
    to_add <- dt[[i]][!is.na(end_prev) & end_prev < get(start_d_vars[i]) - 1, 
                      .(get(id_vars[i]), end_prev)]
    setnames(to_add, "V1", id_vars[i])
    to_add[, (start_d_vars[i]) := end_prev + 1]
    # to_add[, (end_d_vars[i]) := get(start_d_vars[i]) - 1]
    # mark that the new records are unobserved
    to_add <- to_add[,(paste0("observed_",i)) := FALSE]
    
    # Append the new rows to the original dataset
    dt[[i]] <- rbind(dt[[i]], to_add, fill = TRUE)
    #remove auxiliary variable
    dt[[i]][, end_prev := NULL]
    # add end the overall observation as a separate variable
    dt[[i]][,(paste0("last_observed_",i)) := max(get(end_d_vars[i])),  by = c(id_vars[i]) ]
    #remove end of each period
    dt[[i]][, (end_d_vars[i]) := NULL]
  }
  
  # uniform the id identifier start_d_vars[1]  
  if (id_vars[2] != id_vars[1]){
    setnames(dt[[2]], get(id_vars[2]), id_vars[1])
  }
  
  # create a copy of start_d_vars[1]  
  if (start_d_vars[2] != start_d_vars[1]){
    dt[[1]][, (start_d_vars[2]) := get(start_d_vars[1])]
  }
  
  # rbind the two datasets and sort 
  dt <- rbind(dt[[1]],dt[[2]], fill = T)
  setkeyv(dt, c(id_vars[1], start_d_vars[2]))

  # TO DO: collapse values observed on the same day across the two datasets
  
  # ...
  
  # TO DO: if requested, replace unobserved values with the default value
  
  # ...
  
  # TO DO: reproduce values in the future until there is a new value
  
  # ...
  
  # TO DO: generate end of observation time
  
  # ...
  
  # TO DO: if requested, remove unobserved times
  
  # ...

  # TO DO: restore missing values marked by the placeholder
  
  # ...
    
  # TO DO: recast logical variables from integers, and Dates from IDate
  
  # ...
  
  # for testing purposes: return dt
  return( dt)
}
