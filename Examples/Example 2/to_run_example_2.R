# Slightly more complex: the cohort has already an additional variable, the other dataset has two variables

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


outputTD <- GenerateTDDataset(dt = list(cohort_and_vars,other_vars),
                              id_vars = c("person_id","person_id"),
                              start_d_vars = c("st_d","st_d"),
                              end_d_vars = c("end_d","end_d"),
                              variables = list(list("in_study","city"),list("diabetes","most_recent_vaccine")) #,
                              # replace_missing_periods_with_default = c(T,F),
                              # default_values = list(list(0),list())
                              )

# fwrite(outputTD, file = file.path(thisdir,"g_output","output.csv"))

