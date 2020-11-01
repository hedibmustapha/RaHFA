rm(list = ls())

source("./code/cleaning_functions.R")

package.check(c("readxl","readr","dplyr","lubridate","stringr"))

#change path to read recently cleaned dataset
data<-readxl::read_excel("./input/data.xlsx") %>% readr::type_convert()

data<-clean_headers(data) 
data<-rec_missing_all(data)


logbook<-data_check(data)
