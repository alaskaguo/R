rm(list=ls())
library(dplyr)
library(tableone)
library(stringr)
library(readxl)
library(ggplot2)
path = "D:\\myresearch\\PDEANDAF\\data"
setwd(path)

dataset = read.csv("data_2_clean.csv",row.names = 1)
str(dataset)

length(unique(dataset$wbc))

unique_counts = data.frame(apply(dataset,2,function(x){
  length(unique(x))
  }))

to_var = rownames(filter(unique_counts,counts<5))

dataset[to_var] = apply(dataset[to_var],2,function(x)as.factor(x))

##  对连续变量进行正太分布检验  适用于5000以下样本量
contiouns_vars = select_if(dataset,is.numeric)
plist = as.data.frame(apply(contiouns_vars, 2, function(x)shapiro.test(x)$p.value))
## 更改P值表列名
colnames(plist) = "pvalue"## 提取非正太分布数据行名
nonnormal_vars = rownames(subset(plist,pvalue < 0.05))
nonnormal_vars

vars = colnames(dataset)

## 设置分组变量
var_group = "lat_sec"

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
                            #,addOverall = TRUE
                            )   ##  需要先把overall对象生成


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

write.csv(tableone_11,"table_1.csv")
write.csv(tableone_12,"table_12.csv")


label = "lat_sec"    ##设置因变量

dataset$lat_sec = as.numeric(dataset$lat_sec)          ## 因变量必须是数字格式

vars = vars[!vars%in%label]
univ_foumulas = sapply(vars, function(x)as.formula(paste(label,"~",x)))
univ_foumulas
univ_models = lapply(univ_foumulas,function(x){glm(x,
                                                   family = binomial(link = "logit"),
                                                   data = dataset)})

univ_results = lapply(univ_models,
                      function(x){
                        x1 = summary(x)
                        CI = exp(confint(x)) ##提取置信区间
                        OR = exp(coef(x))   ##提取OR值
                        ORCI = paste0(round(OR[[2]],2),"(",round(CI[2,1],2),"~",round(CI[2,2],2),")")  ##生成表格内容
                        P = abs(round(x1$coefficients[,"Pr(>|z|)"][[2]],3))   ##提取P值
                        B = round(x1$coefficients[,"Estimate"][[2]],3)
                        res = c(B,ORCI,P)
                        names(res) = c("B","OR(95%CI)","P value")
                        return(res)
                      })

res = t(as.data.frame(univ_results,check.names = F))
table_2 = as.data.frame(res)
write.csv(table_2,file = "table_2.csv",fileEncoding = "utf-8")

## 筛选P<0.2的变量
p_cutoff = 0.1
table_2$`P value` =  as.numeric(as.character(table_2$`P value`))
multi_vars = paste(rownames(table_2[table_2$`P value` < p_cutoff,])[c(-29,-28)],collapse = "+")

multi_vars




multi_formulas = paste(label,"~",multi_vars)
multi_formulas
multi_model = glm(multi_formulas,
                  family = binomial(link = "logit"),
                  data = dataset)
multi_model
summary(multi_model)



multi_model_2<-step(object = multi_model,trace = 0)
summary(multi_model_2)

#anova(object = multi_model_2,test = "Chisq")

summary(multi_model_2)$coef

table_3 = as.data.frame(ShowRegTable(multi_model_2))

write.csv(table_3,"table_3.csv")

table_3$item = rownames(table_3)

table_3$p = as.character(table_3$p)

table_3[table_3$p == "<0.001",]$p = 0.001

table_3$p = as.numeric(table_3$p)

table_3$item = str_replace(table_3$item,"1","")




library(rms)
## 第三步 按照nomogram要求“打包”数据，绘制nomogram的关键步骤,??datadist查看详细说明
dd=datadist(dataset)
options(datadist="dd") 

##提取纳入的变量名

var = table_3[table_3$p<0.05,]$item[-1]


norm_vars = paste(var,collapse = "+")
norm_formula = paste(label,"~",norm_vars)
norm_formula

## 第四步 构建模型
## 构建logisitc回归模型
f1 <- lrm(as.formula(norm_formula), data = dataset,,x = T,y = T) 

## 绘制logisitc回归的风险预测值的nomogram图
nom <- nomogram(f1, fun= function(x)1/(1+exp(-x)), # or fun=plogis
                lp=F, funlabel="Risk")

plot(nom)    ## 生成列线图 手动保存

## 预测阳性概率
dataset$prob = predict(multi_model_2,dataset,
                        type = "response",
                        legacy.axes = T,percent = T
                       )  

## 绘制ROC曲线
library(pROC)
## 设定事件实际发生列和预测结果列
event = dataset$lat_sec
prob = dataset$prob

roc_dataset = roc(event,prob,smooth = F,percent = T)

plot(roc_dataset,print.auc = T,print.thres = "best",
     col = "#802a2a",lwd = 4,
     auc.polygon.col="#33802a2a",auc.polygon=T,
     max.auc.polygon=T,
     main = "total Cohort",
     grid=c(0.1, 0.2), grid.col=c("green", "red"),
     add = F)

## 生成重点指标表格
noi = coords(roc_dataset, "best", ret=c("threshold", "specificity", "sensitivity","tpr","npv"),
                   transpose = F,best.method = "y")

## 构建校准曲线对象，对象为多因素分析模型
cal_dataset<- calibrate(f1,method='boot', B=1000)     # 构建校准曲线对象

plot(cal_dataset,xlim=c(0,1.0),ylim=c(0,1.0)) 

## 生成验证集校正曲线模型对象，公式为实际事件和预测结果，注意顺序
f2 = lrm(lat_sec~prob,data = testset,x = T,y = T)

cal_test = calibrate(f2,method = "boot",B = 1000)


## 绘制Decision Curve
library(rmda)
dcv_dataset =  decision_curve(as.formula(norm_formula)
                          ,data = dataset,
                          family = binomial(link ='logit'), thresholds = seq(0,1, by = 0.01),
                          confidence.intervals= 0.95,study.design = 'case-control',
                          population.prevalence= 0.3
)

plot_decision_curve(dcv_dataset,         ## 可以是曲线对象的List,两条线画一个图上
                    curve.names="Normogram",
                    cost.benefit.axis =FALSE,
                    col = c('red','blue'),
                    confidence.intervals = F,
                    standardize = F)

