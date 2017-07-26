Skip to content
This repository
Search
Pull requests
Issues
Marketplace
Gist
@bz247
Sign out
Unwatch 9
Star 0
Fork 0 alan-turing-institute/FragileFamiliesChallenge
Code  Issues 0  Pull requests 0  Projects 0  Wiki Insights 
Branch: master Find file Copy pathFragileFamiliesChallenge/Bingyu_code/20170719.R
74570bf  7 days ago
@bz247 bz247 principle component analysis
1 contributor
RawBlameHistory     
73 lines (63 sloc)  3.12 KB
library(data.table)

dir='C:/Users/bz247/Documents'

background <- fread(paste0(dir, '/FFChallenge/background.csv'))
train <- fread(paste0(dir, '/FFChallenge/train.csv'))
prediction <- fread(paste0(dir, '/FFChallenge/prediction.csv'))

### Plot
ggplot(data=train, aes(train$grit))+geom_histogram(breaks=seq(1,4,0.33))
ggplot(all, aes(x=hv5_dsraw, y=gpa)) + geom_point()
ggplot(tmp, aes(x=hv5_ppvtraw, y=gpa)) + geom_point(alpha=0.1) + stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')

###-------------Predicting using hv5_ppvtraw-------------------###
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
  model <- glm(get(train_column)~1+hv5_ppvtraw, data=dt, family=binomial(link=logit), na.action = na.omit)
  dt[,glm_result:=predict(model, dt[,'hv5_ppvtraw'], type='response')]
  print(max(dt$glm_result))
  print(min(dt$glm_result))
  dt[,c(submit_column):=as.integer(ifelse(glm_result>0.5,1,0))]
  dt[!is.na(get(train_column)), c(submit_column):=get(train_column)]
  dt
}

submit<-glm_submit(submit, 'eviction_train', 'eviction')
submit<-glm_submit(submit, 'layoff_train', 'layoff')
submit<-glm_submit(submit, 'jobTraining_train', 'jobTraining')
submit_final <- submit[,c(colnames(prediction)), with=F]
write.csv(submit_final, paste0(dir, '/FragileFamiliesChallenge/submit_20170719.csv'), row.names = F)

###---------------Principle Component Analysis-----------------------###
meta_background <- read.csv(paste0(dir,'/FFChallenge/variables-metadata-master/ffc_variable_types.csv'))
conti_variables <- as.vector(meta_background[meta_background$variable_type=='continuous',]$variable)
conti_variables[1] <- 'challengeID'
background_conti<-background[,conti_variables, with=FALSE]
no.of.na <- colSums(is.na(background_conti))
cols.to.keep <- as.vector(which(no.of.na<dim(background_conti)[1]))
background_conti <- background_conti[,cols.to.keep, with=F] ### Remove all NA columns
no.of.na <- colSums(is.na(background_conti))
nm <- names(background_conti)#[no.of.na2 != 0]
background_conti[, (nm) := lapply(nm, function(x_nm) { #impute nan
  x<-get(x_nm)
  x<-as.numeric(x)
  tryCatch(x[is.na(x)] <- mean(x, na.rm=T),
           warning=function(w) cat(x_nm))
  x
})]

background_conti[, (nm) := lapply(nm, function(x_v) { #remove constant columns
  x<-get(x_v)
  if (var(x)==0) {return(NULL)}
  else {return(x)}
})]
pca <- prcomp(background_conti[,], scale=T)
Contact GitHub API Training Shop Blog About
© 2017 GitHub, Inc. Terms Privacy Security Status Help