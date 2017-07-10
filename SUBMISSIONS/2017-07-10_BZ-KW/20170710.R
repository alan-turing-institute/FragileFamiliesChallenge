library(data.table)

background <- fread('...../FFChallenge/background.csv')
train <- fread('...../Documents/FFChallenge/train.csv')
prediction <- fread('...../Documents/FFChallenge/prediction.csv')

### Plot
ggplot(data=train, aes(train$grit))+geom_histogram(breaks=seq(1,4,0.33))
ggplot(all, aes(x=hv5_dsraw, y=gpa)) + geom_point()
ggplot(tmp, aes(x=hv5_ppvtraw, y=gpa)) + geom_point(alpha=0.1) + stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')

submit<-merge(prediction, train, by='challengeID', suffixes=c('_pred','_train'), all=T)
submit<-merge(submit, background[,c('challengeID','hv5_ppvtraw')], by='challengeID', all.x=T)
###---Linear model---###
lm_submit <- function(dt, train_column, submit_column){
  dt[is.na(hv5_ppvtraw), hv5_ppvtraw:=mean(hv5_ppvtraw, na.rm=T)]
  model <- lm(get(train_column)~1+hv5_ppvtraw, data=dt)
  dt[,lm_result:=predict(model, dt[,'hv5_ppvtraw'])]
  dt[,c(submit_column):=lm_result]
  dt[!is.na(get(train_column)), c(submit_column):=get(train_column)]
  dt
}

submit<-lm_submit(submit, 'gpa_train', 'gpa')
submit<-lm_submit(submit, 'grit_train', 'grit')
submit<-lm_submit(submit, 'materialHardship_train', 'materialHardship')

###---Logistic model---###
glm_submit <- function(dt, train_column, submit_column){
  dt[is.na(hv5_ppvtraw), hv5_ppvtraw:=mean(hv5_ppvtraw, na.rm=T)]
  model <- glm(get(train_column)~1+hv5_ppvtraw, data=dt)
  dt[,glm_result:=predict(model, dt[,'hv5_ppvtraw'])]
  dt[,c(submit_column):=as.integer(ifelse(glm_result>0.5,1,0))]
  dt[!is.na(get(train_column)), c(submit_column):=get(train_column)]
  dt
}

submit<-glm_submit(submit, 'eviction_train', 'eviction')
submit<-glm_submit(submit, 'layoff_train', 'layoff')
submit<-glm_submit(submit, 'jobTraining_train', 'jobTraining')
submit_final <- submit[,c(colnames(prediction)), with=F]
write.csv(submit_final, 'C:/Users/bz247/Documents/FragileFamiliesChallenge/submit_20170710.csv', row.names = F)
