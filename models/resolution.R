#setwd("/Users/mark/Documents/AV-Xtreme/models/")
library(data.table)

if(!exists("holidays")){
  holidays<-fread("../input/spanish_holidays.csv")
  holidays[,dtFormat:=as.Date(dtString)]
  holidays[1:5]
  holidayFeatures<-holidays[,.(national=max(ifelse(Holiday_Type=="National holiday",1,0))
                               ,local=max(ifelse(Holiday_Type=="Local holiday",1,0))
                               ,observance=max(ifelse(Holiday_Type=="Observance",1,0))
                               ,commonLocal=max(ifelse(Holiday_Type=="Common Local holidays",1,0))
                               ,ttl=.N),dtFormat]
}
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

#cont_pre<-fread("../input/Train/Contacts_Pre_2017.csv")
#contracts_end<-fread("../input/Train/Contracts_End.csv")
#contracts_new<-fread("../input/Train/Contracts_New.csv")
res<-fread("../input/Train/Resolution_Pre_2017.csv"
  ,select=c("Date","Category","Subject","Resolution"))
res_test<-fread("../input/Test/Resolution2017.csv")
res[,ID:=.I]
res[,ID:=ID+res_test[,max(ID)]]
res[,set:="train"]
res_test[,set:="test"]
res[1:2];res_test[1:2]
full<-rbind(res,res_test)
full[,dtFormat:=as.Date(Date)]

res_dates<-full[,.N,dtFormat][,dtFormat]
cats<-full[,.N,.(Category)][,Category]
subjects<-full[,.N,.(Subject)][,Subject]
types<-merge(cats,subjects)
setnames(types,c("Category","Subject"))
shell<-data.table(merge(x=res_dates,y=types))
setnames(shell,c("dtFormat","Category","Subject"))
trainR<-merge(shell
              ,full[,.(.N,Resolution=sum(Resolution),ID=min(ID),set=min(set))
                    ,.(dtFormat,Category,Subject)]
             ,by=c("dtFormat","Category","Subject")
             ,all.x=TRUE)
trainR[is.na(set),Resolution:=0]
trainR[is.na(set),set:="train"]
trainR[1:20]
trainR[,wkday:=as.POSIXlt(dtFormat)$wday]
trainR[,weekOfYear:=strftime(as.POSIXlt(dtFormat),format="%W")] 
trainR[,dayOfYear:=strftime(as.POSIXlt(dtFormat),format="%j")] 
trainR[,.N,weekOfYear]
trainR[1:2,]
trainR<-merge(trainR,holidayFeatures,by="dtFormat",all.x=TRUE)
trainR[1:10]
trainR[is.na(national),national:=0]
trainR[is.na(local),local:=0]
trainR[is.na(observance),observance:=0]
trainR[is.na(commonLocal),commonLocal:=0]
trainR[is.na(ttl),ttl:=0]

trainR[,nR:=sum(Resolution,na.rm=T),.(wkday,Category,Subject)]
trainR[,dR:=.N,.(wkday,Category,Subject)]
trainR[,tgt_wkday_Cat_Subj:=ifelse(set=="train",(nR-Resolution)/(dR-1),nR/dR)]
trainR[,recs_wkday_Cat_Subj:=ifelse(set=="train",dR-1,dR)]

fwrite(trainR,"fullR_export.csv")

library(h2o)
h2o.init(nthreads = -1)

fullRHex<-h2o.importFile("fullR_export.csv",destination_frame = "fullR.hex")
trainRHex<-fullRHex[fullRHex$set=="train",]
testRHex<-fullRHex[fullRHex$set=="test",]

trainRHex$foldColumn<-h2o.year(trainRHex$dtFormat)-2009
trainRHex$logResolution<-log1p(trainRHex$Resolution)
excludes<-c("logContacts","N","Resolution","Contacts","dtFormat","foldColumn","logResolution","set"
            ,"nR","dR","ID")
predictors<-colnames(trainRHex)[!colnames(trainRHex) %in% excludes]
gR1<-h2o.gbm(x=predictors,y="Resolution",training_frame = trainRHex
             ,ntrees = 1000,score_tree_interval = 5,stopping_rounds = 2,stopping_tolerance = 0
             ,learn_rate = 0.02,max_depth = 8,sample_rate = 0.7,col_sample_rate = 0.7
             ,model_id = "gR.2",fold_column = "foldColumn")

getValidationDistribution("gR.2")

gR1<-h2o.gbm(x=predictors,y="Resolution",training_frame = trainRHex
             ,ntrees = 1000,score_tree_interval = 50
             #,stopping_rounds = 2,stopping_tolerance = 0
             ,learn_rate = 0.02,max_depth = 8,sample_rate = 0.7,col_sample_rate = 0.7
             ,model_id = "gR.2",fold_column = "foldColumn")

resPredict<-data.table(as.data.frame(h2o.cbind(
  testRHex$ID
  ,h2o.predict(object = gR1,newdata=testRHex[,predictors])$predict
  )))
setnames(resPredict,c("ID","Resolution"))
resPredict[1:10,]
summary(resPredict)
fwrite(resPredict[,.(ID,Resolution=pmax(0,Resolution))][order(ID)],"Resolution.csv")

