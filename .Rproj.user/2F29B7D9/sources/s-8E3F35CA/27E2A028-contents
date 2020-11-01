clean_headers<-function(x){
  names(x)<-gsub("/",".",names(x),fixed=T)
  names(x)<-sub("[_]*","",names(x))
  return(x)
}

package.check <-function(packages) lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

remove_headers<-function(x,tobe_removed){
  x<-x[,names(x)!=""]
  x<-x[,!grepl(paste(tobe_removed,collapse="|"),names(x))]
  return(x)
}

rec_missing<-function(x,missings=c(NULL,'NULL','N/A','n/a',999,998,888,' ','(vide)','d/m','','NA','na')) {
  x[x %in% missings] <- NA
  return(x)
}

rec_missing_all<-function(data){lapply(data,rec_missing) %>% bind_cols}

check_outliers <-function(dbs,uuid,enum,district){
  cleantemplate<-data.frame(
    date=c(),
    uuid=c(),
    enumerator=c(),
    district=c(),
    question.name=c(),
    old.value=c(),
    probleme=c(),
    checkid=c()
  )
  outoutliers<-cleantemplate
  num<-which(lapply(dbs,class)=="integer"|sapply(dbs,class)=="numeric")
  for (l in num){
    lowerq = quantile(dbs[,l],na.rm=T)[2]
    upperq = quantile(dbs[,l],na.rm=T)[4]
    iqr = upperq - lowerq
    ti<-3
    extreme.threshold.upper = (iqr * ti) + upperq + 1 
    extreme.threshold.lower = lowerq - (iqr * ti) - 1
    outl<-which(dbs[,l]>extreme.threshold.upper|dbs[,l]<extreme.threshold.lower)
    if(length(outl)>0){
      doth<-data.frame(
        date=as.character(dbs[["today"]][outl]),
        uuid=as.character(dbs[[uuid]][outl]),
        enumerator=as.character(dbs[[enum]][outl]),
        district=as.character(dbs[[district]][outl]),
        question.name=rep(names(dbs)[l],length(outl)),
        old.value=as.character(dbs[[names(dbs)[l]]][outl]),
        new.value="-",
        probleme=rep("outlier: to check",length(outl)),
        checkid="outliers",
        action="check"
        
      )
      outoutliers<-rbind(outoutliers,doth)
    }
  }
  return(outoutliers)
}

pulluuid<-function(data,logiquetest){data$uuid[which(logiquetest)]}

makeslog<-function(data,logbook,checkid="empty",index,question.name,explanation,new.value="NULL",action="check"){
  if(length(index)>0){
    if(question.name=="all"){oldval<-"-"}else{oldval<-as.character(data[[question.name]][data$uuid%in%index])}
    newlog<-data.frame(
      date=as.character(data$today[data$uuid%in%index]),
      uuid= index,
      enumerator=data$enum_name[data$uuid%in%index],
      district=data$district[data$uuid%in%index],
      question.name = question.name,
      old.value=oldval,
      new.value=new.value,
      probleme = explanation,
      checkid= checkid,
      action=action)
    bind_rows(logbook,newlog)
  } else{
    logbook
  }
}

impl_clean<-function(data,uuid,dclean,uuid_log,qmname,newval,oldval,action){
  for (k in 1:nrow(dclean))
  {
    Taction<-dclean[[action]][k]
    x1<-as.character(dclean[[uuid_log]][k])
    if(any(data[[uuid]]==x1)){
      if(!is.na(Taction)&Taction!="note"&Taction!="nothing"){
        if(Taction=="remove"){
          data<-data[which(!data[[uuid]]%in%x1),]
        } else if(Taction=="recode_sm"|Taction=="recode"|Taction=="change") {
          Y<-as.character(dclean[[qmname]][k])
          val<-dclean[[newval]][k]
          data[[Y]]<-as.character(data[[Y]])
          data[which(data[[uuid]]==x1),which(names(data)==Y)]<-as.character(val)
        }
        #     
        # if(!is.na(dclean[[variabletoclean]][k])){
        #   Ytoclean<-dclean[[variabletoclean]][k]
        #   valtoclean<-dclean[[choicestoclean]][k]
        #   data[,which(names(data)==Ytoclean)]<-as.character(data[,which(names(data)==Ytoclean)])
        #   data[which(data[[uuid]]==X),which(names(data)==Ytoclean)]<-as.character(valtoclean)
        # }
      }
    }
  }
  write.csv(data,paste0("./clean_data/cleandata_",humanTime(),".csv"),row.names=F)
  return(data)
}

`%!in%` = Negate(`%in%`)


humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M")
}

data_check<-function(data){
  logbook<-data.frame(
    date=character(),
    uuid= character(),
    enumerator= character(),
    district= character(),
    question.name = character(),
    old.value = character(),
    new.value = character(),
    probleme = character(),
    checkid= character(),
    action=character()
  )
  
  starttime = ymd_hms(data[["start"]])
  endtime = ymd_hms(data[["end"]])
  data$length_survey<-difftime(as.POSIXct(endtime),as.POSIXct(starttime),units = "mins")
  sm_nsp <- names(data)[grep(".[.](nsp|ne_sait_pas|je_ne_sais_pas)", names(data))]
  sm_autre <- names(data)[grep(".[.]autre", names(data))]
  
  outliers<-check_outliers(data,"uuid","enum_name","district")
  
  index<-pulluuid(data,data$informed_consent=="non")
  logbook<- makeslog(data,logbook,"id001",index,"informed_consent","pas de consentement pour enquete",action = "remove")
  
  index<-pulluuid(data,data$length_survey<10 & data$a6_facility_treat_covid=="oui")
  logbook<- makeslog(data,logbook,"id002",index,"length_survey","enquete plus courte que 5min avec a6_facility_treat_covid=oui",action = "remove")
  
  index<-pulluuid(data,data$length_survey<25 & data$a6_facility_treat_covid=="oui")
  logbook<- makeslog(data,logbook,"id003",index,"length_survey","enquete plus courte que 25min avec a6_facility_treat_covid=oui",action = "check")
  
  data$occurrence_nsp <-apply(data,1,function(x) {grep("nsp|ne_sait_pas|je_ne_sais_pas",x) %>%  unlist %>% length}) +apply(data[sm_nsp],1,sum,na.rm=T)
  index<-pulluuid(data,data$occurrence_nsp>=9)
  logbook<- makeslog(data,logbook,"id004",index, "occurrence_nsp", "Nombre important de reponses nsp par enqueteur(>=9)", action = "check")
  
  data$occurrence_autre <-apply(data,1,function(x) {grep("autre",x) %>%  unlist %>% length}) + apply(data[sm_autre],1,sum,na.rm=T)
  index<-pulluuid(data,data$occurrence_autre>=7)
  logbook<- makeslog(data,logbook,"id005",index, "occurrence_autre", "Nombre important de reponses Autre par enqueteur(>=7)", action = "check")
  
  logbook<-bind_rows(outliers,logbook)
  write.csv(logbook,paste0("./cleaning_log/cleaninglog_",humanTime(),".csv"), row.names=F)
  
  return(logbook)
}