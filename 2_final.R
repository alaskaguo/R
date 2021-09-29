rm(list=ls())
library(readxl)
library(readr)
library(stringr)
library(ggplot2)
library(survival)
library(survminer)
library(dplyr)
library(Hmisc)
library(tableone)
library(Matching)
library(survey)


path = 'D:\\myresearch\\房颤列线图2\\data'
setwd(path)

#####
##### 从以下开始
dataset = read.csv('data_filled.csv')
#dataset = read.csv('datapsm.csv')
dataset = dataset[,-1]

#dataset <- dataset[,!colnames(dataset)%in%c('distance','subclass','noac','BSA','Weightkg','Heightcm','Lymph.','Neut.','PT','Neut.','APTT','LVPWTmm','IVSmm','nlr')]

## 重新指定分类变量
## 制作单一值计数统计表
data = dataset
unique_count = as.list(apply(data,2,function(x){length(unique(x))}))
unique_status <- data.frame()
for (i in 1:length(unique_count)){
  unique_status <- rbind(unique_status,data.frame(names(unique_count[i]),unique_count[i][[1]] ))
}
colnames(unique_status) <- c('items','unique_status')

## 删除只有一个值的列
data = data[,!colnames(data)%in%unique_status[unique_status$unique_status ==1,1]]
colnames(data)
data <- apply(data,2,function(x)as.numeric(x))
## 把所有与数据转为数字
data = data.frame(apply(data,2,function(x)as.numeric(x)))
str(data)
colnames(data)

##  数据种类小于3的变量转为字符串
to_char = unique_status[unique_status$unique_status<3,][,'items']
to_char
data[,colnames(data)%in%to_char] <- apply(data[,colnames(data)%in%to_char],2,function(x)as.character(x))
str(data)
dataset = data


#dataset$LATSEC = ifelse(dataset$LATSEC == 1,T,F)

##  对连续变量进行正太分布检验  适用于5000以下样本量
contiouns_vars = select_if(dataset,is.numeric)
plist = as.data.frame(apply(contiouns_vars, 2, function(x)shapiro.test(x)$p.value))
## 更改P值表列名
colnames(plist) = "pvalue"## 提取非正太分布数据行名
nonnormal_vars = rownames(subset(plist,pvalue < 0.05))
#nonnormal_vars

##分为训练集和验证集
set.seed(121)               #random number generator
{
  ind <- sample(2, nrow(dataset), replace = TRUE, prob = c(0.7, 0.3))
  trainset <- dataset[ind==1, ]    #the training data set
  testset <- dataset[ind==2, ]     #the test data set
  dataset$group = ind
}
table(dataset$group)

vars = colnames(dataset)
vars
## 设置分组变量
var_group = "group"


{
  ## 生成表1对象， 不分组
  tableOne <- CreateTableOne(vars = vars , data = dataset)
  ## 打印表1
  tableone = as.data.frame(print(tableOne,
                                 explain = F,
                                 showAllLevels = FALSE,
                                 nonnormal = nonnormal_vars,
                                 printToggle = F,
                                 dropEqual = T,
                                 varLabels = T))
  #write.csv(tableone,"table_1.csv")
  ## creat table1 by group
  
  ## 生成表1对象，按重点变量标准分组
  tableone_1 = CreateTableOne(vars = vars, strata = var_group, data = dataset
                              ,addOverall = TRUE
  )   
  
  
  tableone_11 = as.data.frame(print(tableone_1,
                                    explain = F,
                                    showAllLevels = FALSE,
                                    nonnormal = nonnormal_vars,
                                    printToggle = F,
                                    dropEqual = T,
                                    varLabels = T)
  )
  
  
  tableone_12 = as.data.frame(print(tableone_1,
                                    explain = F,
                                    showAllLevels = FALSE,
                                    #nonnormal = nonnormal_vars,
                                    printToggle = F,
                                    dropEqual = T,
                                    varLabels = T)
  )
}
write.csv(tableone_11,"table_1_IQR.csv")
write.csv(tableone_12,"table_1_MSD.csv")


label = "LATSEC"    ##设置因变量

#outcome = c('LATSEC','group')

##单因素logistic回归
table_uni_logi <- data.frame()

uni_logi_formulas <- sapply(vars[!vars%in%c(label,var_group)],function(x)uni_logi_formula <- as.formula(str_c(c('as.numeric(',label, ')~',x),collapse = '')))
univ_logi_models  <- lapply(uni_logi_formulas,function(x){glm(x,family = binomial(link = "logit"),data = trainset)})
univ_logi_results<-lapply(univ_logi_models,function(x)ShowRegTable(x))
univ_logi_results_2 = lapply(univ_logi_results, function(x)as.data.frame(x)[2,])

uni_logi_table <- data.frame()
for (i in 1:length(univ_logi_results_2)){
  uni_logi_table <- rbind(uni_logi_table,univ_logi_results_2[i][[1]])
}
colnames(uni_logi_table) <- c("ci","pvalue")

## 去掉P值列里的小于号
uni_logi_table$pvalue <- str_replace(uni_logi_table$pvalue,'<','')
uni_logi_table$pvalue <- as.numeric(uni_logi_table$pvalue)

write.csv(uni_logi_table,file = "table_2.csv",fileEncoding = "utf-8")


## 计算多重共线性,noac变量有共线性,必须先删除
library(car)

## 'noac'必须排除,否则共线性无法计算,原因不详
var_exclude = c('noac','BSA','Weightkg','Heightcm','Lymph.','Neut.','PT','Neut.','APTT','LVPWTmm','IVSmm','Hb','RBC','statin')     ## 没有排除CHA评分

##初始计算
vif_formula_1 = as.formula(str_c(str_c(c(label,'~'),collapse = ""),str_c(colnames(dataset)[!colnames(dataset)%in%c(label,'noac')],collapse = "+"),collapse = ''))
vif_formula_1

vif_exam_1 <- lm(vif_formula_1 , data = dataset)
vif(vif_exam_1)
vifs_1 <- vif(vif_exam_1)
vifs_1 <- as.data.frame(vifs_1)

var_del = c(label,var_exclude)

vif_formula_2 <- as.formula(str_c(str_c(c(label,'~'),collapse = ""),str_c(colnames(dataset)[!colnames(dataset)%in%var_del],collapse = "+"),collapse = ''))
vif_formula_2
vif_exam_2 <- lm(vif_formula_2, data = dataset)
#summary(vif_exam)
vifs_2 <- vif(vif_exam_2)
vifs_2
vifs_2 <- as.data.frame(vifs_2)

vifs_1$items = rownames(vifs_1)
vifs_2$items = rownames(vifs_2)
vifs <- merge(vifs_1,vifs_2,by = 'items',all = T)


## 筛选P<p_cutoff的变量
p_cutoff = 0.2
{
  uni_logi_table$pvalue =  as.numeric(as.character(uni_logi_table$pvalue))
  rownames(uni_logi_table) <- str_replace_all(rownames(uni_logi_table),'1','')
  multi_logi_vars = rownames(uni_logi_table[uni_logi_table$pvalue < p_cutoff,])[!rownames(uni_logi_table[uni_logi_table$pvalue < p_cutoff,])%in%c(var_del,'cha2ds2vasc')]
  multi_logi_vars
  multi_logi_formulas = as.formula(str_c(str_c(c('as.numeric(',label,")~"),collapse = ''),str_c(multi_logi_vars,collapse = "+"),collapse = ''))
  
  multi_logi_formulas
  
  multi_logi_model = glm(multi_logi_formulas,
                         family = binomial(link = "logit"),
                         data = trainset)
  ShowRegTable(multi_logi_model)
  #summary(multi_model)
  ## 后退法
  multi_logi_model_2<-step(object = multi_logi_model,trace = 0)
  #summary(multi_model_2)
  #multi_model_2
}  
#anova(object = multi_model_2,test = "Chisq")
## 整理多因素分析结果
{
  table_3 = as.data.frame(ShowRegTable(multi_logi_model_2))
  ## 修整表3
  table_3$item = rownames(table_3)
  table_3$p = as.character(table_3$p)
  table_3[table_3$p == "<0.001",]$p = 0.001
  table_3$p = as.numeric(table_3$p)
  table_3$item = str_replace(table_3$item,"1","")
}
write.csv(table_3,"table_3.csv")


## 绘制列线图
{
  library(rms)
  ## 第三步 按照nomogram要求“打包”数据，绘制nomogram的关键步骤,??datadist查看详细说明
  
  ##提取纳入的变量名
  var = table_3[(table_3$p<0.05) & (table_3$item != "(Intercept)"),]$item
  #var = var[3:6]
  #norm_vars = str_c(var,collapse = "+")
  #var_add = c('Drink')
  norm_vars = str_c(var,collapse = "+")
  
  norm_formula = paste(str_c('as.numeric(',label,")~"),norm_vars)
  norm_formula
  
  dd = datadist(trainset)
  oldoption <- options(datadist="dd") 
  ## 第四步 构建模型
  ## 构建logisitc回归模型
  f1 <- lrm(as.formula(norm_formula), data = trainset,x = T,y = T) 
  
  ## 绘制logisitc回归的风险预测值的nomogram图
  nom <- nomogram(f1, fun= function(x)1/(1+exp(-x)), # or fun=plogis
                  lp=F, funlabel="Risk") 
  
  pdf("nomogram_classical.pdf",width = 8,height = 6)
  plot(nom)    ## 生成列线图 手动保存
  dev.off()
}


sum(is.na(dataset))

## 根据计算得分
{
  library(nomogramFormula)
  options(oldoption)
  #results <- formula_lp(nomogram = nom,power = 1)
  results <- formula_rd(nomogram = nom)
  ## 计算得分需要把所有数据转为数字类型
  trainset$points <- points_cal(formula = results$formula,rd=
                                  as.data.frame(apply(trainset,2,function(x)as.numeric(x))))
  testset$points <- points_cal(formula = results$formula,rd=
                                 as.data.frame(apply(testset,2,function(x)as.numeric(x))))
}

## 根据得分绘制roc曲线
{
  event_train = trainset$LATSEC
  prob_train = trainset$points
  event_test = testset$LATSEC
  prob_test = testset$points
}
library(pROC)
{
  ## 设定事件实际发生列和预测得分列
  roc_trainset = roc(event_train,prob_train,smooth = F,percent = T)
  roc_cha2_trainset = roc(event_train,trainset$cha2ds2vasc,smooth = F,percent = T)
  #roc_cha2v_trainset = roc(event_train,trainset$cha2v,smooth = F,percent = T)
  
  roc_testset = roc(event_test,prob_test,smooth = F,percent = T)
  roc_cha2_testset = roc(event_test,testset$cha2ds2vasc,smooth = F,percent = T)
  #roc_cha2v_testset = roc(event_test,testset$cha2v,smooth = F,percent = T)
}

#noi_train_cha2 = coords(roc_cha2_trainset, "best", ret=c("threshold","spe","sen","npv","ppv"),
                   transpose = F,best.method = "y")

## 生成重点指标表格

{
  noi_train = coords(roc_trainset, "best", ret=c("threshold","spe","sen","npv","ppv"),
                     transpose = F,best.method = "y")
  rownames(noi_train) = c("trainset") 
  noi_test  = coords(roc_testset, "best", ret=c("threshold","spe","sen","npv","ppv"),
                     transpose = F,best.method = "y")
  rownames(noi_test) = c("testset")
  
  noi_total = rbind(noi_train,noi_test)
  
  
  noi_test_ci = ci.coords(roc_testset, "best", ret=c("threshold","spe","sen","npv","ppv"),
                          transpose = F,best.method = "y")
  
  noi_train_ci = ci.coords(roc_trainset, "best", ret=c("threshold","spe","sen","npv","ppv"),
                           transpose = F,best.method = "y")
}

{
  noi_table_train = data.frame()
  for (i in 1:length(noi_total)){
    
    temp = data.frame("item" = names(noi_train_ci[i]),
                      "value" = noi_train[1,names(noi_train_ci[i])],
                      "lowerci" = noi_train_ci[i][[1]][1],
                      "upperci" = noi_train_ci[i][[1]][3])
    noi_table_train = rbind(noi_table_train,temp)
  }
  
  auc_table_train = data.frame("item" = "auc_train",
                               "value"= auc(roc_trainset),
                               "lowerci" = ci.auc(roc_trainset)[1],
                               "upperci" = ci.auc(roc_trainset)[3])
  
  noi_table_train = rbind(noi_table_train,auc_table_train)
  
  
  noi_table_test = data.frame()
  for (i in 1:length(noi_total)){
    
    temp = data.frame("item" = names(noi_test_ci[i]),
                      "value" = noi_test[1,names(noi_test_ci[i])],
                      "lowerci" = noi_test_ci[i][[1]][1],
                      "upperci" = noi_test_ci[i][[1]][3])
    
    noi_table_test = rbind(noi_table_test,temp)
  }
  auc_table_test = data.frame("item" = "auc_test",
                              "value"= auc(roc_testset),
                              "lowerci" = ci.auc(roc_testset)[1],
                              "upperci" = ci.auc(roc_testset)[3])
  noi_table_test = rbind(noi_table_test,auc_table_test)
  noi_table = cbind(noi_table_train,noi_table_test)
}
write.csv(noi_table,"table_4.csv")


## plot trainset ROC
{
  pdf("roc_train.pdf",width = 3.9,height = 3.9)
  plot(roc_trainset,
       #print.auc = T,
       print.thres = "best",
       col = "#802a2a",
       #lwd = 2,   #线宽
       #auc.polygon.col="#33802a2a",auc.polygon=T,
       max.auc.polygon=T,
       main = "train Cohort",
       #grid=c(0.1, 0.2), grid.col=c("green", "red"),
       add = F)
  
  plot(roc_cha2_trainset,
       #print.auc = T,
       print.thres = "best",col = "red",
       #auc.polygon.col="#33802a2a",auc.polygon=T,
       #max.auc.polygon=T,
       #main = "cha2",
       #grid=c(0.1, 0.2), grid.col=c("green", "red"),
       add = T)
  legend("bottomright", legend=c(str_c(c("Model auc:",round(auc(roc_trainset)[[1]],2),"%"),collapse = ""), 
                                 str_c(c("CHADS2 auc:",round(auc(roc_cha2_trainset)[[1]],2),"%"),collapse = "") 
                                 ),
         col=c("#802a2a", "red","blue"), lwd=2)
  dev.off()
}

## plot validate ROC
{
  pdf("roc_validate.pdf",width = 3.9,height = 3.9)
  plot(roc_testset,
       #print.auc = T,
       print.thres = "best",
       col = "#802a2a",
       #lwd = 2,   #线宽
       #auc.polygon.col="#33802a2a",auc.polygon=T,
       max.auc.polygon=T,
       main = "Validate Cohort",
       #grid=c(0.1, 0.2), grid.col=c("green", "red"),
       add = F)
  
  plot(roc_cha2_testset,
       #print.auc = T,
       print.thres = "best",col = "red",
       #auc.polygon.col="#33802a2a",auc.polygon=T,
       #max.auc.polygon=T,
       #main = "cha2",
       #grid=c(0.1, 0.2), grid.col=c("green", "red"),
       add = T)
  
  legend("bottomright", legend=c(str_c(c("Model auc:",round(auc(roc_testset)[[1]],2),"%"),collapse = ""), 
                                 str_c(c("CHADS2 auc:",round(auc(roc_cha2_testset)[[1]],2),"%"),collapse = "")),
         col=c("#802a2a", "red","blue"), lwd=2)
  
  dev.off()
}

## 绘制校正曲线
{
  ## 构建校准曲线对象，对象为多因素分析模型
  cal_trainset<- calibrate(f1,method='boot', B=1000)     # 构建校准曲线对象
  pdf("cal_train.pdf",width = 3.9,height = 3.9)
  plot(cal_trainset,xlim=c(0,1.0),ylim=c(0,1.0)) 
  dev.off()
  ## 生成验证集校正曲线模型对象，公式为testset实际事件和预测结果，注意顺序
  f2 = lrm(LATSEC~points,data = testset,x = T,y = T)
  
  cal_test = calibrate(f2,method = "boot",B = 1000,data = testset)
  pdf("cal_validate.pdf",width = 3.9,height = 3.9)
  plot(cal_test,xlim=c(0,1.0),ylim=c(0,1.0)) 
  dev.off()
}

trainset$LATSEC <- as.numeric(trainset$LATSEC)
testset$LATSEC  <- as.numeric(testset$LATSEC)
## 绘制Decision Curve
{
  
  library(rmda)
  
  dcv_trainset =  decision_curve(as.formula(norm_formula)
                                 ,data = trainset,
                                 family = binomial(link ='logit'), thresholds = seq(0,1, by = 0.01),
                                 confidence.intervals= 0.95,study.design = 'case-control',
                                 population.prevalence= 0.3
  )
  dcv_trainset_cha2 =  decision_curve(as.formula(LATSEC~cha2ds2vasc)
                                 ,data = trainset,
                                 family = binomial(link ='logit'), thresholds = seq(0,1, by = 0.01),
                                 confidence.intervals= 0.95,study.design = 'case-control',
                                 population.prevalence= 0.3
  )
  
  pdf("fig2e.pdf",width = 3.8,height = 3.0)
  plot_decision_curve(list(dcv_trainset, dcv_trainset_cha2),       ## 可以是曲线对象的List,两条线画一个图上
                      curve.names=c("Normogram",'cha2ds2vasc'),
                      cost.benefit.axis =FALSE,
                      col = c('red','blue'),
                      confidence.intervals = F,
                      standardize = F)
  
  dev.off()
  dcv_testset =  decision_curve(as.formula(norm_formula)
                                ,data = testset,
                                family = binomial(link ='logit'), thresholds = seq(0,1, by = 0.01),
                                confidence.intervals= 0.95,study.design = 'case-control',
                                population.prevalence= 0.3
  )
  dcv_testset_cha2 =  decision_curve(as.formula(LATSEC~cha2ds2vasc)
                                      ,data = testset,
                                      family = binomial(link ='logit'), thresholds = seq(0,1, by = 0.01),
                                      confidence.intervals= 0.95,study.design = 'case-control',
                                      population.prevalence= 0.3
  )
  pdf("fig2f.pdf",width = 3.8,height = 3.0)
  plot_decision_curve(list(dcv_testset, dcv_testset_cha2),        ## 可以是曲线对象的List,两条线画一个图上
                      curve.names=c("Normogram",'cha2ds2vasc'),
                      cost.benefit.axis =FALSE,
                      col = c('red','blue'),
                      confidence.intervals = F,
                      standardize = F)
  dev.off()
}

##  拟合优度检验
{
  library(ResourceSelection)
  fit4 <- glm(norm_formula,data = trainset,family = "binomial")
  h1 = hoslem.test(fit4$y,fitted(fit4),g=10)
  
}

{
  fit5 <- glm(norm_formula,data = testset,family = "binomial")
  h2 = hoslem.test(fit5$y,fitted(fit5),g=10)
  
}

h1
h2
