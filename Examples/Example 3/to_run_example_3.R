# These are the same data as Example 2: the first dataset is the cohort and it has already an additional variable, the other dataset has three variables partially unobserved

# Now we make assumptions  on the unobserved periods for the three variables in the second dataset: 

# diabetes is a chronic disease, so once it is found it is true forever; moreover, we assume that the person has sufficient lookback at cohort entry, so we assume the baseline value is 0

# most_recent_vaccine: vaccines are assumed to be observed whenever administered, so is there is no vaccine we assume it is not administered; the last date therefore carries over until observetion in the first dataset is available, but unlike diabetes it has no baseline value

# use_of_aspirin: periods of use are assumed to be observed, so we put to 0 all unobserved periods

rm(list=ls(all.names=TRUE))

if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source(file.path("..","..","R", "GenerateTDDataset.R"))

dir.create(file.path(thisdir,"g_output"), recursive = T)

if (!require("data.table")) install.packages("data.table")
library(data.table)

cohort_and_vars <- data.table::fread(file.path(thisdir,"i_input","cohort_and_vars.csv"))
other_vars <- data.table::fread(file.path(thisdir,"i_input","other_vars.csv"))


outputTD <- GenerateTDDataset(datasets = list(cohort_and_vars,other_vars),
                              UoO_vars = c("person_id","person_id"),
                              start_d_vars = c("st_d","st_d"),
                              end_d_vars = c("end_d","end_d"),
                              TD_variables = list(list("in_study","city"),list("diabetes","most_recent_vaccine","use_of_aspirin")),
                              keep_auxiliary_variables = T,
                              keep_records_observed_by = "first",
                              baseline_value = list(
                                "diabetes" = 0
                              ),
                              default_value_for_unobserved = list(
                                "use_of_aspirin" = 0
                              ),
                              TD_variables_with_definite_value = c("diabetes","most_recent_vaccine"),
                              keep_UoOs_observed_by = "first"
                              )

fwrite(outputTD, file = file.path(thisdir,"g_output","output.csv"))

