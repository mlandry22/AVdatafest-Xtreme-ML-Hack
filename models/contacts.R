setwd("/Users/mark/Documents/AV-Xtreme/models/")
library(data.table)

holidays<-fread("../input/spanish_holidays.csv")
holidays[,dtFormat:=as.Date(dtString)]
holidays[1:5]
holidayFeatures<-holidays[,.(national=max(ifelse(Holiday_Type=="National holiday",1,0))
                             ,local=max(ifelse(Holiday_Type=="Local holiday",1,0))
                             ,observance=max(ifelse(Holiday_Type=="Observance",1,0))
                             ,commonLocal=max(ifelse(Holiday_Type=="Common Local holidays",1,0))
                             ,ttl=.N),dtFormat]
getValidationDistribution<-function(modelString,folds=7,printSummary=TRUE,printAll=FALSE){
  scores<-list()
  scores[[1]]<-h2o.mse(h2o.getModel(paste0(modelString,"_cv_1")),valid = TRUE)^0.5
  scores[[2]]<-h2o.mse(h2o.getModel(paste0(modelString,"_cv_2")),valid = TRUE)^0.5
  scores[[3]]<-h2o.mse(h2o.getModel(paste0(modelString,"_cv_3")),valid = TRUE)^0.5
  scores[[4]]<-h2o.mse(h2o.getModel(paste0(modelString,"_cv_4")),valid = TRUE)^0.5
  scores[[5]]<-h2o.mse(h2o.getModel(paste0(modelString,"_cv_5")),valid = TRUE)^0.5
  scores[[6]]<-h2o.mse(h2o.getModel(paste0(modelString,"_cv_6")),valid = TRUE)^0.5
  scores[[7]]<-h2o.mse(h2o.getModel(paste0(modelString,"_cv_7")),valid = TRUE)^0.5
  if(printAll){print(unlist(scores))}
  if(printSummary){print(summary(unlist(scores)))}
  return(unlist(scores))
}  


cont_pre<-fread("../input/Train/Contacts_Pre_2017.csv")
cont_test<-fread("../input/Test/Contacts2017.csv")

cont_pre[,ID:=.I]
cont_pre[,ID:=ID+cont_test[,max(ID)]]
cont_pre[1:2,]; cont_test[1:2,]
cont_pre[,set:="train"]
cont_test[,set:="test"]
full<-rbind(cont_pre[,.(Date=START.DATE,CONTACT.TYPE,Contacts,ID,set)],cont_test)

full[,dtFormat:=as.Date(Date)]

cont_dates<-full[,unique(dtFormat)]
types<-cont_pre[,unique(CONTACT.TYPE)]
shell<-data.table(merge(x=cont_dates,y=types))
setnames(shell,c("dtFormat","CONTACT.TYPE"))
full<-merge(shell,full[,.(.N,Contacts=sum(Contacts),ID=min(ID),set=min(set))
                       ,.(dtFormat,CONTACT.TYPE)]
             ,by=c("dtFormat","CONTACT.TYPE")
             ,all.x=TRUE)
full[is.na(set),Contacts:=0]
full[is.na(set),set:="train"]

full[1:12]
full[,wkday:=as.POSIXlt(dtFormat)$wday]
full[,weekOfYear:=strftime(as.POSIXlt(dtFormat),format="%W")] 
full[,dayOfYear:=strftime(as.POSIXlt(dtFormat),format="%j")] 
full[,year:=pmin(2016,as.numeric(strftime(as.POSIXlt(dtFormat),format="%Y")))] 
full[,.N,weekOfYear]
full[1:2,]
full<-merge(full,holidayFeatures,by="dtFormat",all.x=TRUE)
full[1:20]
full[is.na(national),national:=0]
full[is.na(local),local:=0]
full[is.na(observance),observance:=0]
full[is.na(commonLocal),commonLocal:=0]
full[is.na(ttl),ttl:=0]

#full[,mean_type_wkday:=mean(Contacts,na.rm = TRUE),.(CONTACT.TYPE,wkday)]
#full[,sd_type_wkday:=sd(Contacts,na.rm = TRUE),.(CONTACT.TYPE,wkday)]

full[,nC:=sum(Contacts,na.rm=T),.(wkday,CONTACT.TYPE)]
full[,dC:=.N,.(wkday,CONTACT.TYPE)]
full[,tgt_wkday_Typej:=ifelse(set=="train",(nC-Contacts)/(dC-1),nC/dC)]


fwrite(full[set=="train"],"train_export.csv")
fwrite(full[set=="test"],"test_export.csv")

library(h2o)
h2o.init(nthreads = -1)

trainHex<-h2o.importFile("train_export.csv",destination_frame = "train.hex")
testHex<-h2o.importFile("test_export.csv",destination_frame = "test.hex")
trainHex$foldColumn<-h2o.year(trainHex$dtFormat)-2009
trainHex$logContacts<-log1p(trainHex$Contacts)
excludes<-c("logContacts","N","Resolution","Contacts","dtFormat","foldColumn","set","nC","dC","ID")
predictors<-colnames(trainHex)[!colnames(trainHex) %in% excludes]
gC2<-h2o.gbm(x=predictors,y="Contacts",training_frame = trainHex
             ,ntrees = 1000,score_tree_interval = 5,stopping_rounds = 2,stopping_tolerance = 0
             ,learn_rate = 0.02,max_depth = 8,sample_rate = 0.7,col_sample_rate = 0.7
             ,model_id = "gC.4",fold_column = "foldColumn")

getValidationDistribution("gC.4")

conPredict<-data.table(as.data.frame(h2o.cbind(
  testHex$ID
  ,h2o.predict(object = gC2,newdata=testHex[,predictors])$predict
)))
setnames(conPredict,c("ID","Contacts"))
conPredict[1:10,]
summary(conPredict)
fwrite(conPredict[,.(ID,Contacts=pmax(0,Contacts))][order(ID)],"Contacts.csv")

#cont_test[,dtFormat:=as.Date(Date)]
#cont_test[1:2];full[1:2]
#cont_test<-cont_test[order(dtFormat,CONTACT.TYPE)]
#full<-full[order(set,dtFormat,CONTACT.TYPE)]
#full[set=="test",ID:=cont_test[,ID]]

#library(Metrics)
#rmse(full[set=="train",Contacts],0)
#rmse(full[set=="train",Contacts],mean(full[set=="train",Contacts]))
#rmse(full[set=="train",Contacts],full[set=="train",mean_type_wkday])
#sum(se(full[set=="train",Contacts],full[set=="train",mean_type_wkday]))

#740*(20/74)
#200 in public
#540 in private
#rmse 115 = mse 13225; mse 13225 * 200 = se 2645000
#rmse  93 = mse xxxxx; mse xxxxx * 200 = se 1729800
# diff of 915200
# sqrt 915200 = 956.66; so fixing a single prediction by 1000 would do it
