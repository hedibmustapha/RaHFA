logbook<-read.csv("./cleaning_log/cleaninglog_20201030-1230.csv")

clean<- impl_clean(data,"uuid",logbook,"uuid","question.name","new.value","old.value","action")



# sensitive_vars<-c("enum_name","follow_contact_respondent","followup_name","followup_position","followup_phone",
#                   "followup_date","enumerator_comments","whatsapp_location","_longitude","_latitude","_altitude","_precision")
# data<-remove_headers(data,sensitive_vars)
