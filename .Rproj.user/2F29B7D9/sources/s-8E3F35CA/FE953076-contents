rm(list = ls())

source("./code/cleaning_functions.R")

package.check(c("readxl","readr","dplyr","lubridate","stringr"))

#change path to read recently cleaned dataset
data<-readxl::read_excel("./input/REACH_-_Evaluation_établissements_de_santé_RaHFA_-_all_versions_-_False_-_2020-10-27-08-25-34.xlsx") %>% readr::type_convert()

data<-clean_headers(data) 
data<-rec_missing_all(data)


logbook<-data_check(data)
