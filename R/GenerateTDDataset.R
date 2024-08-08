#'GenerateTDDataset
#'
#' Version 0.1 
#' 2 Aug 2024
#' Authors: Rosa Gini, Davide Messina
#'
#' @param datasets  pair of data.table objects
#' @param UoO_vars pair of variable names: variable in either dataset where the identifier of the unit of observation is stored
#' @param start_d_vars pair of variable names: variable in either dataset where the start of validity of the record is stored
#' @param end_vars pair of variable names: variable in either dataset where the end of validity of the record is stored
#' @param TD_variables pair of lists of variable names: groups of time-dependent variables in either dataset
#' @param default_value_for_unobserved lists of list of values: default value for those TD_variables that have a default value; if this argument is not assigned for a variable (default), for that variable unobserved periods either remain assigned at missing, or are governed by baseline_value and/or TD_variables_with_definite_value, if specified. note that missing values in observed periods are considered observed values and are never replaced
#' @param baseline_value lists of list of values: baseline value for those TD_variables that have a baseline value; if this argument is not assigned for a variable (default), for that variable the baseline periods, if unobserved, remain assigned at missing, unless default_value_for_unobserved is specified. if both default_value_for_unobserved and baseline_value are specified, then unobserved periods before the first observation are set set to baseline_value, and those after are set to default_value_for_unobserved. note that missing values in observed periods are considered observed values and are never replaced
#' @param TD_variables_with_definite_value list of variables that extend the last observed value to the future, until either there is a new observed period, or the study ends. if a variable is listed here, then it cannot have a value in default_value_for_unobserved. note that missing values in observed periods are considered observed values and are never replaced
#' 
# supports formats integer, numeric, character, factor, Date, IDate, and logical
 
GenerateTDDataset <- function(datasets,
                              UoO_vars, 
                              start_d_vars,
                              end_d_vars,
                              TD_variables,
                              default_value_for_unobserved = list(),
                              baseline_value = list(),
                              TD_variables_with_definite_value = c(),
                              keep_auxiliary_variables = F,
                              keep_observed_by = "either"
                              ) {
  
  # libraries
  if (!require("data.table")) install.packages("data.table")
  library(data.table)
  if (!require("zoo")) install.packages("zoo")
  library(zoo)
  # store the unique list of TD_variables, that will be stored in the final dataset
  TD_variables_final <- unique(c(unlist(TD_variables[1]), unlist(TD_variables[2])))
  
  # Input validation
  if (!is.list(datasets) || length(datasets) != 2) {
    stop("Argument 'datasets' should be a list of two data.table objects.")
  }
  if (!all(sapply(datasets, is.data.table))) {
    stop("Both elements in 'datasets' must be data.tables.")
  }
  if (!is.character(UoO_vars) || length(UoO_vars) != 2) {
    stop("Argument 'UoO_vars' should be a character vector of length 2.")
  }
  if (!is.character(start_d_vars) || length(start_d_vars) != 2) {
    stop("Argument 'start_d_vars' should be a character vector of length 2.")
  }
  if (!is.character(end_d_vars) || length(end_d_vars) != 2) {
    stop("Argument 'end_d_vars' should be a character vector of length 2.")
  }
  if (!is.list(TD_variables) || length(TD_variables) != 2 || 
      !all(sapply(TD_variables, function(x) is.list(x) && all(sapply(x, is.character))))) {
    stop("Argument 'TD_variables' should be a list of two lists, each containing character vectors.")
  }

  if ( length(TD_variables_final) < length(unique(unlist(TD_variables[1]))) +  length(unique(unlist(TD_variables[2]))) )  {
    stop("The TD variables of the two datasets cannot have the same name")
  }
  
  if (!is.logical(keep_auxiliary_variables) ) {
    stop("Argument 'keep_auxiliary_variables' should have a logical value")
  }
  
  if (!(keep_observed_by %in% c("both","either","first","second","none")) ) {
    stop('Argument "keep_observed_by" should have a value chosen between "both","first","second","none"')
  }
  
  # keep_observed_by = "both"
  
  # Supported types
  supported_types <- c("integer", "numeric", "character", "factor", "Date",  "IDate", "logical")
  date_supported_types <- c("IDate", "Date", "integer")
  
  # Initialize list to store classes
  classesvars <- list(list(), list())
  classesvarsunique <- list()
  
  # Check that all variables in the first list of 'TD_variables' exist in the first dataset of 'datasets'
  first_dt_vars <- names(datasets[[1]])
  missing_vars_1 <- setdiff(unlist(TD_variables[[1]]), first_dt_vars)
  if (length(missing_vars_1) > 0) {
    stop("The following variables are not found in the first dataset: ", paste(missing_vars_1, collapse = ", "))
  }
  
  # Check that all variables in the second list of 'TD_variables' exist in the second dataset of 'datasets'
  second_dt_vars <- names(datasets[[2]])
  missing_vars_2 <- setdiff(unlist(TD_variables[[2]]), second_dt_vars)
  if (length(missing_vars_2) > 0) {
    stop("The following variables are not found in the second dataset: ", paste(missing_vars_2, collapse = ", "))
  }
  
  # Check and validate 'start_d' and 'end_d' classes
  for (i in 1:2) {
    start_class <- class(datasets[[i]][[start_d_vars[i]]])
    end_class <- class(datasets[[i]][[end_d_vars[i]]])
    if (!start_class %in% date_supported_types) {
      stop(sprintf("The variable '%s' in the %s dataset has an unsupported type: %s. Expected types are of date type: %s.",
                   start_d_vars[i], ifelse(i == 1, "first", "second"), start_class, paste(date_supported_types, collapse = ", ")))
    }
    if (!end_class %in% date_supported_types) {
      stop(sprintf("The variable '%s' in the %s dataset has an unsupported type: %s. Expected types are of date type: %s.",
                   end_d_vars[i], ifelse(i == 1, "first", "second"), end_class, paste(date_supported_types, collapse = ", ")))
    }
  }
  
   # TO DO: check that the following arguments default_value_for_unobserved, baseline_value and TD_variables_with_definite_valuecontain correct values and are not in contradiction with each other, as follows:
  # default_value_for_unobserved[[thisvar]] must have the same class as thisvar
  # baseline_value[[thisvar]] must have the same class as thisvar
  # TD_variables_with_definite_value is a vector containing elements of the list TD_variables_final
  # if default_value_for_unobserved[[thisvar]] is assigned, then TD_variables_with_definite_value cannot contain thisvar
  
  
  # Store and validate classes of TD_variables in the first dataset
  for (var in unlist(TD_variables[[1]])) {
    var_class <- class(datasets[[1]][[var]])
    classesvars[[1]][[var]] <- var_class
    classesvarsunique[[var]] <- var_class
    if (!var_class %in% supported_types) {
      stop(sprintf("The variable '%s' in the first dataset has an unsupported type: %s. Please recast the variable to one of the supported types: %s.",
                   var, var_class, paste(supported_types, collapse = ", ")))
    }
  }
  
  # Store and validate classes of TD_variables in the second dataset
  for (var in unlist(TD_variables[[2]])) {
    var_class <- class(datasets[[2]][[var]])
    classesvars[[2]][[var]] <- var_class
    classesvarsunique[[var]] <- var_class
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
  #     if (length(default_values[[i]]) != length(TD_variables[[i]])) {
  #       stop(sprintf("Length of 'default_values' for the %s dataset must match the length of 'TD_variables' list.", ifelse(i == 1, "first", "second")))
  #     }
  #     for (j in seq_along(default_values[[i]])) {
  #       if (class(default_values[[i]][[j]]) != classesvars[[i]][[TD_variables[[i]][j]]]) {
  #         stop(sprintf("Default value for variable '%s' in the %s dataset must be of class '%s'.",
  #                      TD_variables[[i]][j], ifelse(i == 1, "first", "second"), classesvars[[i]][[TD_variables[[i]][j]]]))
  #       }
  #     }
  #   }
  # }
  

  # # TO BE COMPLETED: Check for overlapping periods in either dataset using foverlaps
  # for (i in 1:2) {
  #   # Set keys using the dynamic column names
  #   setkeyv(datasets[[i]], c(UoO_vars[i], start_d_vars[i], end_d_vars[i]))
  #   
  #   # Find overlaps within the dataset
  #   overlaps <- foverlaps(datasets[[i]], datasets[[i]], 
  #                         by.x = c(UoO_vars[i], start_d_vars[i], end_d_vars[i]), 
  #                         by.y = c(UoO_vars[i], start_d_vars[i], end_d_vars[i]),
  #                         nomatch = NULL)
  #   
  #   # Print overlaps to debug
  #   print(overlaps)
  #   
  #   # Filter out self-overlaps (where the observation overlaps with itself)
  #   TO DO!!!!
  # }
  
  # # TO COMPLETE: if in datasets[[i]] there are additional variables with respect to those in the function call, check that they are time-independent (TI), that is, that they have the same value across all records with same UoO_vars[[i]]
  
  TI_variables <- list()
  for (i in 1:2) {
    TI_variables[[i]] <- setdiff(names(datasets[[i]]),c(unlist(TD_variables[[i]]),UoO_vars[[i]],start_d_vars[[i]],end_d_vars[[i]]) )
  }
  TI_variables_final <- unique(c(unlist(TI_variables[1]), unlist(TI_variables[2])))
  
  # ...
    

  
  ################################################
  # FUNCTION BODY
  ############################################

  # Cast logical TD_variables to integers and Dates to IDate
  for (i in 1:2) {
    for (var in unlist(TD_variables[[i]])) {
      var_class <- class(datasets[[i]][[var]])
      if (var_class == "logical") {
        datasets[[i]][[var]] <- as.integer(datasets[[i]][[var]])
      } else if (var_class %in% c("Date")) {
        datasets[[i]][[var]] <- as.IDate(datasets[[i]][[var]])
      }
    }
  }

  # Initialize lists for extreme_value and placeholder_value
  extreme_value <- list()
  placeholder_value <- list()
  placeholder_value2 <- list()

  # Calculate extreme_value and placeholder_value for each variable
  for (i in 1:2) {
    for (thisvar in unlist(TD_variables[[i]])) {
      # Find the minimum value of non-missing values, except for characters where we pick the maximum
      non_missing_vals <- datasets[[i]][!is.na(get(thisvar)), get(thisvar)]
      if (length(non_missing_vals) > 0) {
        var_class <- class(datasets[[i]][[thisvar]])
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
    for (thisvar in unlist(TD_variables[[i]])) {
      if (!is.na(placeholder_value[[thisvar]])) {
        datasets[[i]][is.na(get(thisvar)), (thisvar) := placeholder_value[[thisvar]]]
      }
    }
  }
  
  # Add logical variable 'observed_1' and 'observed_2'
  datasets[[1]][, observed_1 := 1]
  datasets[[2]][, observed_2 := 1]
  
  
  # fill the gaps between intervals of the same id
  for (i in 1:2) {
    # Set keys using the dynamic column names
    setkeyv(datasets[[i]], c(UoO_vars[i], start_d_vars[i], end_d_vars[i]))
    
    # Define end_prev and identify gaps
    datasets[[i]][, end_prev := shift(get(end_d_vars[i]), type = "lag"), by = c(UoO_vars[i])]
    to_add <- copy(datasets[[i]])[!is.na(end_prev) & end_prev < get(start_d_vars[i]) - 1, 
                      .(get(UoO_vars[i]), end_prev)]
   if (nrow(to_add) > 1){
      setnames(to_add, "V1", UoO_vars[i])
      to_add[, (start_d_vars[i]) := end_prev + 1]
      # to_add[, (end_d_vars[i]) := get(start_d_vars[i]) - 1]
      # mark that the new records are unobserved
      to_add <- to_add[,(paste0("observed_",i)) := 0]
      # Append the new rows to the original dataset
      datasets[[i]] <- rbind(datasets[[i]], to_add, fill = TRUE)
   }
    rm(to_add)
    #remove auxiliary variable
    datasets[[i]][, end_prev := NULL]
    # add start and end of the overall observation as a separate variable
    datasets[[i]][,(paste0("first_day_observed_",i)) := min(get(start_d_vars[i])),  by = c(UoO_vars[i]) ]
    datasets[[i]][,(paste0("last_day_observed_",i)) := max(get(end_d_vars[i])),  by = c(UoO_vars[i]) ]
    # add a row that contains the next day to the last
    to_add <- unique(copy(datasets[[i]])[,.(get(UoO_vars[i]), get(paste0("last_day_observed_",i)))])
    setnames(to_add,c("V1","V2"),c(UoO_vars[i],end_d_vars[i]))
    to_add[, (start_d_vars[i]) := get(end_d_vars[i]) + 1]
    to_add <- to_add[,(paste0("observed_",i)) := 0]
    # Append the new rows to the original dataset
    datasets[[i]] <- rbind(datasets[[i]], to_add, fill = TRUE)
    rm(to_add)
    #remove end of each period
    datasets[[i]][, (end_d_vars[i]) := NULL]
    # mark each observation in unobserved record with placeholder value 2
    for (thisvar in unlist(TD_variables[[i]])) {
      if (!is.na(placeholder_value2[[thisvar]])) {
        datasets[[i]][is.na(get(thisvar)) & get(paste0("observed_",i)) == 0, (thisvar) := placeholder_value2[[thisvar]]]
      }
    }
  }
  
  # set the name of UoO_var_final as UoO_vars[1]
  UoO_var_final <- UoO_vars[1]
  if (UoO_vars[2] != UoO_vars[1]){
    setnames(datasets[[2]], get(UoO_vars[2]), UoO_var_final)
  }
  
  # set the name of start_d_var_final as start_d_vars[1]
  start_d_var_final <- start_d_vars[1]
  if (start_d_vars[2] != start_d_vars[1]){
    datasets[[2]][, (start_d_var_final) := get(start_d_vars[2])]
  }
  
  # set the name of end_d_var_final as end_d_vars[1]
  end_d_var_final <- end_d_vars[1]
  
  #########################
  # rbind the two datasets and sort 
  dt_final <- rbind(datasets[[1]],datasets[[2]], fill = T)
  setkeyv(dt_final, c(UoO_var_final, start_d_var_final))
  
  ##############
  # collapse values of all the variables of dt_final observed on the same day: collapse all variables to their max by UoO_var_final and start_d_var_final, ignoring missing values

  cols_to_collapse <- setdiff(names(dt_final), c(UoO_var_final,start_d_var_final))
  
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
  }), by = c(UoO_var_final,start_d_var_final), .SDcols = cols_to_collapse]
  
  # # Replace -Inf (result of max when all are NA) with NA
  # collapsed_dt[is.infinite(collapsed_dt)] <- NA
  
  # Replace original dt with collapsed_dt
  dt_final <- collapsed_dt
  rm(collapsed_dt)
  
  ###########
  # generate end of observation time (end_d_var_final): this is NA for the last record of UoO_var_final, and for the previous it is start_d_var_final of the next record, - 1. then, order the names of the variables and put UoO_var_final,start_d_var_final,end_d_var_final first
  
  dt_final[, (end_d_var_final) := shift(get(start_d_var_final), type = "lead"), by = c(UoO_vars[i])]
  
  dt_final[, (end_d_var_final) := get(end_d_var_final) - 1]

  dt_final <- dt_final[ !(is.na(get(end_d_var_final))), ]
  
    ordervar <- c(UoO_var_final, TI_variables_final, start_d_var_final,end_d_var_final, TD_variables_final, setdiff(names(dt_final), c(UoO_var_final, TI_variables_final, start_d_var_final,end_d_var_final, TD_variables_final)))
  
  dt_final <- dt_final[, ..ordervar]
  
  ######################################
  # fill missing values within the observed periods, for both TD_variables and c("observed_1","observed_2")
  for (thisvar in c(TD_variables_final,"observed_1","observed_2") ) {
    dt_final[, (thisvar) := zoo::na.locf(get(thisvar), na.rm = FALSE), by = UoO_var_final]
  }
  
  #finalise definition of c("observed_1","observed_2") but setting it at 0 when it is NA
  for (thisvar in c("observed_1","observed_2") ) {
    dt_final[is.na(get(thisvar)), (thisvar) := 0]
  }
  
  ################
  # reproduce values of TI_variables_final including first and last day observed
  
  cols_to_collapse <-  c(TI_variables_final,"first_day_observed_1","last_day_observed_1","first_day_observed_2","last_day_observed_2")

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
  }), by = c(UoO_var_final), .SDcols = cols_to_collapse]
  
  dt_final <- merge(dt_final[, c(TI_variables_final,"first_day_observed_1","last_day_observed_1","first_day_observed_2","last_day_observed_2") := NULL], collapsed_dt, by = UoO_var_final)
  rm(collapsed_dt)
  
  ###########
  # optional replacements of missing values
  
  ##################
  # TO COMPLETE: enact the various options to fill variables during their unobserved periods

  # baseline_value[[thisvar]]: if assigned, this is the value that must replace NA during the baseline period, if any 
  # default_value_for_unobserved[[thisvar]]: if assigned, this is the value that must replace placeholder2 everywhere else (if baseline_value[[thisvar]] is unassigned, this will replace both NAs and placeholder2)
  # TD_variables_with_definite_value: if thisvar is in this list, then all placeholder2 must be replaced by the corresponding last observed value of thisvar
  
  # TO CHECK IF THIs WORKS: fill missing values within the observed periods for TD_variables_with_definite_value
  for (thisvar in c(TD_variables_final) ) {
    if (thisvar %in% TD_variables_with_definite_value){
      if (classesvarsunique[[thisvar]] == "numeric"){
        dt_final[get(thisvar) < placeholder[[thisvar]], (thisvar) := NA]
      }else{
        dt_final[get(thisvar) == placeholder_value2[[thisvar]], (thisvar) := NA]
      }
      dt_final[, (thisvar) := zoo::na.locf(get(thisvar), na.rm = FALSE), by = UoO_var_final]
    }
  }
  
  
  # baseline_value[[thisvar]]
  
  # ...
  
  # default_value_for_unobserved[[thisvar]]
  
  # ...
 
      
  # remove unobserved times
  
  if (keep_observed_by == "either"){
    dt_final <- dt_final[ observed_1 == 1 | observed_2 == 1,]
  }
  if (keep_observed_by == "both"){
    dt_final <- dt_final[ observed_1 == 1 & observed_2 == 1,]
  }
  if (keep_observed_by == "first"){
    dt_final <- dt_final[ observed_1 == 1,]
  }
  if (keep_observed_by == "second"){
    dt_final <- dt_final[ observed_2 == 1,]
  }
  # if keep_observed_by == "none", ther the results keeps everything, including intervals of times internal between two intervals that are not observed by anyone
  if (keep_observed_by == "none"){
  }
  
  # ...

  # restore missing values marked by placeholder_value[[thisvar]] (in the case of numeric variables these are values "get(thisvar) < extremevalue[[thisvar]]", in the other cases these are "get(thisvar) == placeholder_value[[thisvar]]")
  
  for (thisvar in TD_variables_final){
    if (classesvarsunique[[thisvar]] == "numeric"){
      dt_final[get(thisvar) < extremevalue[[thisvar]], (thisvar) := NA ]
    }else{
      dt_final[get(thisvar) == placeholder_value[[thisvar]], (thisvar) := NA ]
      dt_final[get(thisvar) == placeholder_value2[[thisvar]], (thisvar) := NA ]
    }
  }
    
  # TO DO: recast logical TD_variables from integers, and Dates from IDate
  
  # ...
  # Cast logical TD_variables to integers and Dates to IDate
  for (thisvar in TD_variables_final) {
    var_class <- classesvarsunique[[thisvar]]
    if (var_class == "logical") {
      dt_final[[var]] <- as.logical(dt_final[[thisvar]])
    } else if (var_class %in% c("Date")) {
      dt_final[[var]] <- as.Date(dt_final[[thisvar]])
    }
  }


  # remove auxiliary variables, unless requested otherwise
  
  ordervar <- c(UoO_var_final, TI_variables_final, start_d_var_final,end_d_var_final, TD_variables_final)
  
  if (keep_auxiliary_variables == T){
    ordervar <- c(ordervar, setdiff(names(dt_final), c(UoO_var_final, TI_variables_final, start_d_var_final,end_d_var_final, TD_variables_final)))
  }
  
  dt_final <- dt_final[, ..ordervar]
  
  # for testing purposes: return dt_final
  return( dt_final)
}
