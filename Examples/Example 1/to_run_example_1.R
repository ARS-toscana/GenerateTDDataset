# Very simple example: the first dataset is a cohort with 1 period per subject, the second a dataset with one variable with some changes over time and incomplete periods

rm(list=ls(all.names=TRUE))

if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source(file.path("..","..","R", "GenerateTDDataset.R"))

dir.create(file.path(thisdir,"g_output"), recursive = T)

if (!require("data.table")) install.packages("data.table")
library(data.table)

cohort <- data.table::fread(file.path(thisdir,"i_input","cohort.csv"))
vardataset <- data.table::fread(file.path(thisdir,"i_input","variable.csv"))

# to test what happens with NA values, set a value of -var- to NA

vardataset <- vardataset[var == "",var := NA_character_]

outputTD <- GenerateTDDataset(datasets = list(cohort,vardataset),
                              UoO_vars = c("person_id","person_id"),
                              start_d_vars = c("study_entry","st_d"),
                              end_d_vars = c("study_exit","end_d"),
                              TD_variables = list(list("in_study"),list("var")) ,
                              keep_auxiliary_variables = T #,
                              )

fwrite(outputTD, file = file.path(thisdir,"g_output","output.csv"))

