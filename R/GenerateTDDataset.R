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
#' @param TDvariables pair of lists of variable names: groups of time-dependent variables in either dataset
#' @param default_value_for_missing lists of list of values: default value for those TDvariables that have a default value; if this argument is not assigned for a variable, for that variable unobserved periods remain assigned at missing (default)
#' @param baseline_value lists of list of values: baseline value for those TDvariables that have a baseline value; if this argument is not assigned for a variable, for that variable the baseline periods, if unobserved, remain assigned at missing (default)
#' @param TD_variables_with_definite_value list of variables that extend the last observed value indefinitely to the future, even if to periods when they are not observed in the original dataset. if a variable is listed here, then default_value_for_missing cannot be assigned for that variable
#' 
# supports formats integer, numeric, character, factor, Date, IDate, and logical
 
GenerateTDDataset <- function(dt,
                              id_vars, 
                              start_d_vars,
                              end_d_vars,
                              TDvariables,
                              default_value_for_missing = list(),
                              baseline_value = list(),
                              TD_variables_with_definite_value = c()
                              ) {
  
  # libraries
  if (!require("data.table")) install.packages("data.table")
  library(data.table)
  if (!require("zoo")) install.packages("zoo")
  library(zoo)
  # store the unique list of TD_variables, that will be stored in the final dataset
  TD_variables_final <- unique(c(unlist(TDvariables[1]), unlist(TDvariables[2])))
  
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
  if (!is.list(TDvariables) || length(TDvariables) != 2 || 
      !all(sapply(TDvariables, function(x) is.list(x) && all(sapply(x, is.character))))) {
    stop("Argument 'TDvariables' should be a list of two lists, each containing character vectors.")
  }

  if ( length(TD_variables_final) < length(unique(unlist(TDvariables[1]))) +  length(unique(unlist(TDvariables[2]))) )  {
    stop("The TD variables of the two datasets cannot have the same name")
  }
  
  # Supported types
  supported_types <- c("integer", "numeric", "character", "factor", "Date",  "IDate", "logical")
  date_supported_types <- c("IDate", "Date", "integer")
  
  # Initialize list to store classes
  classesvars <- list(list(), list())
  classesvarsunique <- list()
  
  # Check that all variables in the first list of 'TDvariables' exist in the first dataset of 'dt'
  first_dt_vars <- names(dt[[1]])
  missing_vars_1 <- setdiff(unlist(TDvariables[[1]]), first_dt_vars)
  if (length(missing_vars_1) > 0) {
    stop("The following variables are not found in the first dataset: ", paste(missing_vars_1, collapse = ", "))
  }
  
  # Check that all variables in the second list of 'TDvariables' exist in the second dataset of 'dt'
  second_dt_vars <- names(dt[[2]])
  missing_vars_2 <- setdiff(unlist(TDvariables[[2]]), second_dt_vars)
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
  
   # TO DO: check that the following lists contain correct values and are not in contradiction with each other
  # default_value_for_missing
  # baseline_value
  # TD_variables_with_definite_value
  
  
  
  # Store and validate classes of TDvariables in the first dataset
  for (var in unlist(TDvariables[[1]])) {
    var_class <- class(dt[[1]][[var]])
    classesvars[[1]][[var]] <- var_class
    classesvarsunique[[var]] <- var_class
    if (!var_class %in% supported_types) {
      stop(sprintf("The variable '%s' in the first dataset has an unsupported type: %s. Please recast the variable to one of the supported types: %s.",
                   var, var_class, paste(supported_types, collapse = ", ")))
    }
  }
  
  # Store and validate classes of TDvariables in the second dataset
  for (var in unlist(TDvariables[[2]])) {
    var_class <- class(dt[[2]][[var]])
    classesvars[[2]][[var]] <- var_class
    if (!var_class %in% supported_types) {
      stop(sprintf("The variable '%s' in the second dataset has an unsupported type: %s. Please recast the variable to one of the supported types: %s.",
                   var, var_class, paste(supported_types, collapse = ", ")))
    }
  }
  # 
  # # Check 'replace_missing_periods_with_default' is a list of logicals
  # 
  # if (!is.logical(replace_missing_periods_with_default) || length(replace_missing_periods_with_default) != 2) {
  #   stop("Argument 'replace_missing_periods_with_default' should be a list of logicals")
  # }
  # 
  # # Check 'default_values'
  # for (i in 1:2) {
  #   if (replace_missing_periods_with_default[i]) {
  #     if (length(default_values[[i]]) != length(TDvariables[[i]])) {
  #       stop(sprintf("Length of 'default_values' for the %s dataset must match the length of 'TDvariables' list.", ifelse(i == 1, "first", "second")))
  #     }
  #     for (j in seq_along(default_values[[i]])) {
  #       if (class(default_values[[i]][[j]]) != classesvars[[i]][[TDvariables[[i]][j]]]) {
  #         stop(sprintf("Default value for variable '%s' in the %s dataset must be of class '%s'.",
  #                      TDvariables[[i]][j], ifelse(i == 1, "first", "second"), classesvars[[i]][[TDvariables[[i]][j]]]))
  #       }
  #     }
  #   }
  # }
  

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
  
  # # TO COMPLETE: if in dt[[i]] there are additional variables with respect to those in the function call, check that they are time-independent (TI), that is, that they have the same value across all records with same id_vars[[i]]
  
  TIvariables <- list()
  for (i in 1:2) {
    TIvariables[[i]] <- setdiff(names(dt[[i]]),c(unlist(TDvariables[[i]]),id_vars[[i]],start_d_vars[[i]],end_d_vars[[i]]) )
    }
  
  # ...
    

  
  ################################################
  # FUNCTION BODY
  ############################################

  # Cast logical TDvariables to integers and Dates to IDate
  for (i in 1:2) {
    for (var in unlist(TDvariables[[i]])) {
      var_class <- class(dt[[i]][[var]])
      if (var_class == "logical") {
        dt[[i]][[var]] <- as.integer(dt[[i]][[var]])
      } else if (var_class %in% c("Date")) {
        dt[[i]][[var]] <- as.IDate(dt[[i]][[var]])
      }
    }
  }

  # Initialize lists for extreme_value and placeholder_value
  extreme_value <- list()
  placeholder_value <- list()
  placeholder_value2 <- list()

  # Calculate extreme_value and placeholder_value for each variable
  for (i in 1:2) {
    for (thisvar in unlist(TDvariables[[i]])) {
      # Find the minimum value of non-missing values, except for characters where we pick the maximum
      non_missing_vals <- dt[[i]][!is.na(get(thisvar)), get(thisvar)]
      if (length(non_missing_vals) > 0) {
        var_class <- class(dt[[i]][[thisvar]])
        if (var_class != "character"){
          min_val <- min(non_missing_vals, na.rm = TRUE)
          extreme_value[[thisvar]] <- min_val
          placeholder_value[[thisvar]] <- min_val - 1
          placeholder_value2[[thisvar]] <- min_val - 2
        }else{
          max_val <- max(non_missing_vals, na.rm = TRUE)
          extreme_value[[thisvar]] <- max_val
          placeholder_value[[thisvar]] <- paste0(max_val,"Z")
          placeholder_value2[[thisvar]] <- paste0(max_val,"ZZ")
        }
      } else {
        extreme_value[[thisvar]] <- NA
        placeholder_value[[thisvar]] <- NA
      }
    }
  }
  
  

  # Replace missing values with placeholder_value
  for (i in 1:2) {
    for (thisvar in unlist(TDvariables[[i]])) {
      if (!is.na(placeholder_value[[thisvar]])) {
        dt[[i]][is.na(get(thisvar)), (thisvar) := placeholder_value[[thisvar]]]
      }
    }
  }
  
  # Add logical variable 'observed_1' and 'observed_2'
  dt[[1]][, observed_1 := 1]
  dt[[2]][, observed_2 := 1]
  
  
  # fill the gaps between intervals of the same id
  for (i in 1:2) {
    # Set keys using the dynamic column names
    setkeyv(dt[[i]], c(id_vars[i], start_d_vars[i], end_d_vars[i]))
    
    # Define end_prev and identify gaps
    dt[[i]][, end_prev := shift(get(end_d_vars[i]), type = "lag"), by = c(id_vars[i])]
    to_add <- copy(dt[[i]])[!is.na(end_prev) & end_prev < get(start_d_vars[i]) - 1, 
                      .(get(id_vars[i]), end_prev)]
   if (nrow(to_add) > 1){
      setnames(to_add, "V1", id_vars[i])
      to_add[, (start_d_vars[i]) := end_prev + 1]
      # to_add[, (end_d_vars[i]) := get(start_d_vars[i]) - 1]
      # mark that the new records are unobserved
      to_add <- to_add[,(paste0("observed_",i)) := 0]
      # Append the new rows to the original dataset
      dt[[i]] <- rbind(dt[[i]], to_add, fill = TRUE)
   }
    rm(to_add)
    #remove auxiliary variable
    dt[[i]][, end_prev := NULL]
    # add end the overall observation as a separate variable
    dt[[i]][,(paste0("last_day_observed_",i)) := max(get(end_d_vars[i])),  by = c(id_vars[i]) ]
    # add a row that contains the next day to the last
    to_add <- unique(copy(dt[[i]])[,.(get(id_vars[i]), get(paste0("last_day_observed_",i)))])
    setnames(to_add,c("V1","V2"),c(id_vars[i],end_d_vars[i]))
    to_add[, (start_d_vars[i]) := get(end_d_vars[i]) + 1]
    to_add <- to_add[,(paste0("observed_",i)) := 0]
    # Append the new rows to the original dataset
    dt[[i]] <- rbind(dt[[i]], to_add, fill = TRUE)
    rm(to_add)
    #remove end of each period
    dt[[i]][, (end_d_vars[i]) := NULL]
    # mark each observation in unobserved record with placeholder value 2
    for (thisvar in unlist(TDvariables[[i]])) {
      if (!is.na(placeholder_value2[[thisvar]])) {
        dt[[i]][is.na(get(thisvar)) & get(paste0("observed_",i)) == 0, (thisvar) := placeholder_value2[[thisvar]]]
      }
    }
  }
  
  # uniform the id identifier start_d_vars[1]
  id_vars_final <- id_vars[1]
  if (id_vars[2] != id_vars[1]){
    setnames(dt[[2]], get(id_vars[2]), id_vars_final)
  }
  
  # create a copy of start_d_vars[1]
  start_d_vars_final <- start_d_vars[1]
  if (start_d_vars[2] != start_d_vars[1]){
    dt[[2]][, (start_d_vars_final) := get(start_d_vars[2])]
  }
  # set the name of end_d_vars_final as end_d_vars[1]
  end_d_vars_final <- end_d_vars[1]
  
  #########################
  # rbind the two datasets and sort 
  dt_final <- rbind(dt[[1]],dt[[2]], fill = T)
  setkeyv(dt_final, c(id_vars_final, start_d_vars_final))
  
  ##############
  # collapse values of all the variables of dt observed on the same day: collapse all variables to their max by id_vars_final and start_d_vars_final, ignoring missing values

  cols_to_collapse <- setdiff(names(dt_final), c(id_vars_final,start_d_vars_final))
  
  collapsed_dt <- dt_final[, lapply(.SD, function(x) {
    # Check if all values are NA
    if (all(is.na(x))) {
      # Return NA of the appropriate type based on the class of x
      na_value <- switch(class(x)[1],
                         "integer" = NA_integer_,
                         "numeric" = NA_real_,
                         "character" = NA_character_,
                         "factor" = NA_character_,  # For factors, NA_character_ is returned since NA_factor_ does not exist
                         "logical" = NA)
      return(na_value)
    } else {
      # Calculate max, ignoring NA values
      return(max(x, na.rm = TRUE))
    }
  }), by = c(id_vars_final,start_d_vars_final), .SDcols = cols_to_collapse]
  
  # # Replace -Inf (result of max when all are NA) with NA
  # collapsed_dt[is.infinite(collapsed_dt)] <- NA
  
  # Replace original dt with collapsed_dt
  dt_final <- collapsed_dt
  rm(collapsed_dt)
  
  ###########
  # generate end of observation time (end_d_vars_final): this is NA for the last record of id_vars_final, and for the previous it is start_d_vars_final of the next record, - 1. then, order the names of the variables and put id_vars_final,start_d_vars_final,end_d_vars_final first
  
  dt_final[, (end_d_vars_final) := shift(get(start_d_vars_final), type = "lead"), by = c(id_vars[i])]
  
  dt_final[, (end_d_vars_final) := get(end_d_vars_final) - 1]
  
    ordervar <- c(id_vars_final,start_d_vars_final,end_d_vars_final, setdiff(names(dt_final), c(id_vars_final,start_d_vars_final,end_d_vars_final)))
  
  dt_final <- dt_final[, ..ordervar]
  
  # for TD_variables_final, replace NA values in the future with the last non-NA value
  for (thisvar in TD_variables_final) {
    if (thisvar %in% TD_variables_with_definite_value){
      dt_final[get(thisvar) == placeholder_value2[[thisvar]], (thisvar) := NA]
    }
    dt_final[, (thisvar) := zoo::na.locf(get(thisvar), na.rm = FALSE), by = id_vars_final]
  }
  
  # TO DO: reproduce values of TIvariables
  
  # ...
  
  
  ###########
  # optional replacements of missing values
  
  # TO DO: enact the various options to fill variables during their unobserved periods (in the case of numeric variables these are values "get(thisvar) < placeholder[[thisvar]]", in the other cases these are "get(thisvar) == placeholder2[[thisvar]]")
  # default_value_for_missing
  # baseline_value
  # TD_variables_with_definite_value
  
      
  # TO DO: unless an option is created that allows not to do so, remove unobserved times
  
  # ...

  # TO DO: restore missing values marked by the placeholder (in the case of numeric variables these are values "get(thisvar) < extremevalue[[thisvar]]", in the other cases these are "get(thisvar) == placeholder[[thisvar]]")
  
  # ...
    
  # TO DO: recast logical TDvariables from integers, and Dates from IDate
  
  # ...

  # TO DO: remove auxiliary variables
  
  # ...
  
  # for testing purposes: return dt_final
  return( dt_final)
}
