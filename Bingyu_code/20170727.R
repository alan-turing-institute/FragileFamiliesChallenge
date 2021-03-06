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

#no.of.na <- colSums(is.na(background_conti))
#cols.to.keep <- as.vector(which(no.of.na<dim(background_conti)[1]))
#background_conti <- background_conti[,cols.to.keep, with=F] ### Remove all NA columns
#no.of.na <- colSums(is.na(background_conti))
nm <- names(background_conti)#[no.of.na2 != 0]
background_conti[, (nm) := lapply(nm, function(x_nm) { #impute nan
  x<-get(x_nm)
  x<-as.numeric(x)
  tryCatch(x[is.na(x)] <- mean(x, na.rm=T),
           warning=function(w) cat(x_nm))
  x
})]

background_conti[, (nm) := lapply(nm, function(x_nm) { #remove constant columns
  x<-get(x_nm)
  if (var(x)==0) {return(NULL)}
  else {return(x)}
})]
pca <- prcomp(background_conti[,], scale=T)
pca_df <- as.data.frame(cbind(background_conti$challengeID, pca$x))
colnames(pca_df)[1] <- 'challengeID'

### Continuous y
submit_pca<-merge(prediction, train, by='challengeID', suffixes=c('_pred','_train'), all=T)
submit_pca<-as.data.frame(merge(submit_pca, pca_df[,1:50], by='challengeID', all.x=T))

lm_submit <- function(df, train_column, submit_column){
  df_model <- df[,grep('PC', colnames(df))]
  df_model <- cbind(df[,train_column], df_model)
  colnames(df_model)[1] <- train_column
  model <- lm(formula(paste(train_column,'~.')), data=df_model)
  df[,submit_column]=predict(model, df_model)
  index <- !is.na(df[[train_column]])
  df[[submit_column]][index]<- df[[train_column]][index]
  df
}
submit_pca<-lm_submit(submit_pca, 'gpa_train', 'gpa')
submit_pca<-lm_submit(submit_pca, 'grit_train', 'grit')
submit_pca<-lm_submit(submit_pca, 'materialHardship_train', 'materialHardship')

### Categorical y
glm_submit <- function(df, train_column, submit_column){
  df_model <- df[,grep('PC', colnames(df))]
  df_model <- cbind(df[,train_column], df_model)
  colnames(df_model)[1] <- train_column
  print('number of values being 1')
  print(sum(df_model[,train_column], na.rm=T))
  model <- glm(formula(paste(train_column,'~.')), data=df_model, family=binomial(link=logit), na.action=na.omit)
  model_pred=predict(model, df_model, type='response')
  df[,submit_column] <- as.integer(ifelse(model_pred>0.5, 1, 0))
  index <- !is.na(df[[train_column]])
  df[[submit_column]][index]<- df[[train_column]][index]
  print('number of values predicted 1')
  print(sum(df[,submit_column], na.rm=T))
  df
}

submit_pca<-glm_submit(submit_pca, 'eviction_train', 'eviction')
submit_pca<-glm_submit(submit_pca, 'layoff_train', 'layoff')
submit_pca<-glm_submit(submit_pca, 'jobTraining_train', 'jobTraining')
submit_final <- submit_pca[,c(colnames(prediction))]
write.csv(submit_final, paste0(dir, '/FragileFamiliesChallenge/Bingyu_code/submit_20170719.csv'), row.names = F)

###------------Selecting Meaningful Categorical Variables with ANOVA----------------###
background_df <- as.data.frame(background)
background_df <- background_df[, colSums(is.na(background_df))<nrow(background_df)] # filter out all NaN columns
cate_col <- which(colnames(background_df) %in% meta_background[meta_background$variable_type=='categorical',]$variable) # select categorical variables
background_cate <- background_df[, cate_col]
hist(colSums(is.na(background_df)), breaks=20) ### About has less than 50 missing values, the other half more than 500 missing values
tmp<- apply(background_cate, 2, function(x) length(unique(x)))
hist(log10(tmp), breaks=20) # Number of levels: most have less than 10
back_cate_1 <- background_cate[,(tmp<15)&(tmp>1)]
back_cate_1 <- back_cate_1[, colSums(is.na(back_cate_1))<50]
back_cate_1 <- cbind(background_df$challengeID,back_cate_1) # a subset of columns that have less than 50 missing values and 1~15 categorical levels
colnames(back_cate_1)[1] <- 'challengeID'
test_df <- merge(train, back_cate_1, by='challengeID', all.y = T)
test_df <- as.data.frame(test_df)

col_names <- colnames(test_df)
anova_col <- vector() # a vector of categorical column names that have anova test p-value < 0.1 for response variable 'gpa'
for (i in 540:6832) {
  tmp_df <- test_df[test_df[,i]>0,] # only keep positive categories, as negative usually means no response
  if (length(tmp_df$challengeID)==0) {anova_col<-anova_col}
  else{
    tryCatch(test_aov <- aov(as.formula(paste("gpa~", paste(col_names[i]), sep = "")),data=tmp_df, na.action=na.omit), error=function(e){print('catch error')})
    p_value <- summary(test_aov)[[1]][['Pr(>F)']][1] # extract p-value from the anova test
    if(!is.null(p_value)){
      if (!is.na(p_value) & p_value<0.1) {
        anova_col <- c(anova_col, col_names[i]) # add to list
      }
    }
  }
}
write(anova_col, paste0(dir, '/FragileFamiliesChallenge/Bingyu_code/anova_col.txt'))

### ----------------------------- LASSO -----------------------------###
background_df <- as.data.frame(background)
### select columns that have low anova p-values with gpa
anova_col <- scan(paste0(dir, '/FragileFamiliesChallenge/Bingyu_code/anova_col.txt'))
cate_col <- which(colnames(background_df) %in% c('challengeID', anova_col)) # select categorical variables
### select columns that have few levels
level_col <- apply(background_df, 2, function(x) length(unique(x)))
select_from_meta <- meta_background[meta_background$variable_type=='categorical',]
select_from_meta <- select_from_meta[grep('Construct', select_from_meta$label),]
level_col_constr_cate <- level_col[c('challengeID', as.character(select_from_meta$variable))]
cate_col <- c('challengeID', names(level_col_constr_cate[level_col_constr_cate>1 & level_col_constr_cate<6]))
### prepare the matrix for glmnet
cate_df <- background_df[, cate_col]
cate_df[,2:ncol(cate_df)][is.na(cate_df[,2:ncol(cate_df)])] <- 'navalue'
cate_df[,2:ncol(cate_df)] <- lapply(cate_df[,2:ncol(cate_df)], factor)
cate_df <- merge(train, cate_df, by='challengeID', all.y=T)

coef_glmnet <- function(response, s){
  f1 <- as.formula(paste(response, "~ -1+", paste(names(cate_df)[8:ncol(cate_df)],collapse="+")))
  design_matrix <- model.matrix(f1, model.frame(~., cate_df, na.action=na.pass))
  print(dim(design_matrix))
  row <- !is.na(cate_df[[response]])
  print(dim(design_matrix[row,]))
  cvfit <- cv.glmnet(design_matrix[row,], cate_df[[response]][row])
  coef_glm <- coef(cvfit, s=s)
  glm_selected <- row.names(coef_glm)[which(coef_glm!=0)]
  glm_selected <- glm_selected[2:length(glm_selected)]
  print(length(glm_selected))
  
  new_design_matrix <- design_matrix[, c(glm_selected)]
  print(rankMatrix(new_design_matrix)[1])
  df <- as.data.frame(new_design_matrix)
  df <- as.data.frame(lapply(df, as.factor))
  df <- cbind(cate_df[[response]], df)
  colnames(df)[1] <- 'train'
  model <- lm(train ~., data=df)
  model_predict <- predict(model, df)
  print(head(model_predict, 10))
  index <- !is.na(df$train)
  print(head(index))
  model_predict[index]<-df$train[index]
  print(head(model_predict, 10))
  print(length(model_predict))
  return(model_predict)
}
gpa_pred <- coef_glmnet('gpa', 0.0002)
grit_pred <- coef_glmnet('grit', 0.0002)
mthd_pred <- coef_glmnet('materialHardship', 0.0005)

logistic_glmnet <- function(response, s, accept){
  f1 <- as.formula(paste(response, "~ -1+", paste(names(cate_df)[8:ncol(cate_df)],collapse="+")))
  design_matrix <- model.matrix(f1, model.frame(~., cate_df, na.action=na.pass))
  print(dim(design_matrix))
  row <- !is.na(cate_df[[response]])
  print(dim(design_matrix[row,]))
  cvfit <- cv.glmnet(design_matrix[row,], cate_df[[response]][row], alpha=1, family='binomial')
  coef_glm <- coef(cvfit, s=s)
  glm_selected <- row.names(coef_glm)[which(coef_glm!=0)]
  glm_selected <- glm_selected[2:length(glm_selected)]
  print(length(glm_selected))
  
  new_design_matrix <- design_matrix[, c(glm_selected)]
  print(rankMatrix(new_design_matrix)[1])
  df <- as.data.frame(new_design_matrix)
  df <- as.data.frame(lapply(df, as.factor))
  df <- cbind(cate_df[[response]], df)
  colnames(df)[1] <- 'train'
  model <- glm(train ~., data=df, family='binomial')
  model_predict <- predict(model, df, type='response')
  model_predict <- as.integer(ifelse(model_predict>accept,1,0))
  print(head(model_predict, 10))
  index <- !is.na(df$train)
  print(sum(model_predict==1))
  model_predict[index]<-df$train[index]
  print(head(model_predict, 10))
  print(sum(model_predict==1))
  print(sum(df$train==1, na.rm=T))
  return(model_predict)
}
eviction_pred <- logistic_glmnet('eviction', 0.002, 0.23)
layoff_pred <- logistic_glmnet('layoff', 0.001, 0.31)
jobTraining_pred <- logistic_glmnet('jobTraining', 0.001, 0.33)

submit <- as.data.frame(cbind(rep(1:4242), gpa_pred, grit_pred, mthd_pred, eviction_pred, layoff_pred, jobTraining_pred))
colnames(submit) <- c('challengeID', 'gpa', 'grit', 'materialHardship', 'eviction', 'layoff', 'jobTraining')
write.csv(submit, paste0(dir, '/FragileFamiliesChallenge/SUBMISSIONS/2017-07-29/submit_20170729.csv'), row.names = F)


