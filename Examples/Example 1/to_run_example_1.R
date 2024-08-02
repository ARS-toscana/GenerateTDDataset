rm(list=ls(all.names=TRUE))

if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source(file.path("..","..","R", "GenerateTDDataset.R"))

dir.create(file.path(thisdir,"g_output"), recursive = T)


cohort <- data.table::fread(file.path(thisdir,"i_input","cohort.csv"))
vardataset <- data.table::fread(file.path(thisdir,"i_input","variable.csv"))


outputTD <- GenerateTDDataset(dt = list(cohort,vardataset),
                              id = c("id","id"),
                              start_d = c("study_entry","st_d"),
                              end_d = c("study_exit","end_d"),
                              variables = list(list("in_study"),list("var"))
                              )

# fwrite(outputTD, file = file.path(thisdir,"g_output","output.csv"))

