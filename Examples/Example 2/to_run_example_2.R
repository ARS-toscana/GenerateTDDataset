# Slightly more complex: the first dataset is the cohort and it has already an additional variable, the other dataset has three variables partially unobserved. Moreover, in the the cohort there is one UoO that is completely missing from the second dataset

# We do not make any assumption on the unobsered periods for the three variables in the second dataset, therefore in the output they have missing values whenever unobserved

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
                              keep_periods_observed_by = "none",
                              keep_UoOs_observed_by = "first"
                              #,
                              )

fwrite(outputTD, file = file.path(thisdir,"g_output","output.csv"))

