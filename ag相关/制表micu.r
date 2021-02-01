rm(list=ls())
{
library(dplyr)
library(tableone)
library(stringr)
library(readxl)
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(tidyr)
library(survival)
library(survminer)
library(broom)
}
path = "F:/myresearch/micu/data"
setwd(path)


dataset = read.csv("rawdata_cleaned.csv",row.names = 1)
head(dataset)
dataset = dataset[!colnames(dataset)%in%c("aniongap_max")]

colnames(dataset)

outcomename = c("los_hospital", "hospital_expire_flag","los_icu","survive_days","survive_30days","survive_90days","survive_180days","survive_365days",
                "aki_stage_7day","aki_7day","aki_stage_48hr","aki_48hr","not_survive")
str(dataset)
colnames(dataset)

to_factor = c( "gender","hospital_expire_flag","survive_30days","survive_90days",
               "survive_180days","survive_365days","not_survive","rrt","vent","endotrachflag","aki_stage_7day","aki_7day","aki_stage_48hr","aki_48hr",
               "congestive_heart_failure","cardiac_arrhythmias","hypertension","chronic_pulmonary",
               "diabetes_uncomplicated","renal_failure","fluid_electrolyte","ag_group_min")

varsToFactor = to_factor
dataset[varsToFactor] = apply(dataset[varsToFactor],2,function(x)as.factor(x))
str(dataset)

## 提取65岁以上患者
#dataset = dataset[dataset$admission_age>=65,]

##  对连续变量进行正太分布检验  适用于5000以下样本量
contiouns_vars = select_if(dataset,is.numeric)
plist = as.data.frame(apply(contiouns_vars, 2, function(x)shapiro.test(x)$p.value))
## 更改P值表列名
colnames(plist) = "pvalue"## 提取非正太分布数据行名
nonnormal_vars = rownames(subset(plist,pvalue < 0.05))
vars = colnames(dataset)[!colnames(dataset)%in%c("SUBJECT_ID","HADM_ID","icustay_id","subject_id")]




##  对连续变量进行正太分布检验  适用于5000以上样本量
contiouns_vars = select_if(dataset,is.numeric)

plist = as.data.frame(apply(contiouns_vars, 2, function(x)ks.test(x,"pnorm")$p.value))
## 更改P值表列名
colnames(plist) = "pvalue"## 提取非正太分布数据行名

nonnormal_vars = rownames(subset(plist,pvalue < 0.05))


vars = colnames(dataset)[!colnames(dataset)%in%c("SUBJECT_ID","HADM_ID","icustay_id","subject_id")]

## 设置分组变量
var_group = "ag_group_min"

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
tableone_1 = CreateTableOne(vars = vars, strata = var_group, data = dataset,addOverall = TRUE)   ##  需要先把overall对象生成
  
  
tableone_11 = as.data.frame(print(tableone_1,
                                  explain = F,
                                  showAllLevels = FALSE,
                                  nonnormal = nonnormal_vars,
                                  printToggle = F,
                                  dropEqual = T,
                                  varLabels = T)
                           )
write.csv(tableone_11,"table_1.csv")

#write.csv(dataset,"to_spss.csv")



dataset$status= as.numeric(dataset$not_survive)

fit = surv_fit(Surv(survive_days,status)~ag_group_min,data = dataset)

ggsurvplot(fit
           ,risk.table = T
           ,pval = T
           ,surv.median.line = "hv"
           ,conf.int = F
           ,xlim=c(0,365)
           ,ggtheme = theme_bw()
           #,ggtheme = theme_minimal()
           #,legend = "right"      #将图例移动到下方
           ,legend.title = "Group"    #改变图例名称
           ,legend.labs = c("AG<12", "12<=AG<=14","AG>14")
           ,title="Overall survival" #标题
           ,linetype = "strata"          # 改变线条类型
           ,pval.method=TRUE
           ,ylab="Cumulative survival (percentage)",xlab = "Days after MICU admission"
           ,break.x.by = 100     ## 设置X轴ticks间距
)

surv_diff = survdiff(Surv(survive_days,status)~ag_group_min,data = dataset)
surv_diff




##  死亡率柱状图
{
mortality = dataset%>%select(c("survive_30days","survive_90days","survive_180days","survive_365days","ag_group_min"
                               #,"hospital_expire_flag"
                               ))
  
mortality_1 = data.frame(sapply(mortality%>%filter(ag_group_min == 1),
                                function(x){sum(as.numeric(x))/dim(mortality%>%filter(ag_group_min == 1))[1]}))

mortality_1$days= rownames(mortality_1)
mortality_1$group = "1"
colnames(mortality_1) = c("mortality","days","group")
mortality_1 = mortality_1[!c(mortality_1$mortality == 1),]

mortality_2 = data.frame(sapply(mortality%>%filter(ag_group_min == 2),
                                function(x){sum(as.numeric(x))/dim(mortality%>%filter(ag_group_min == 2))[1]}))
mortality_3 = data.frame(sapply(mortality%>%filter(ag_group_min == 3),
                                function(x){sum(as.numeric(x))/dim(mortality%>%filter(ag_group_min == 3))[1]}))
mortality_2$days= rownames(mortality_2)
mortality_2$group = "2"
colnames(mortality_2) = c("mortality","days","group")
mortality_2 = mortality_2[-5,]

mortality_3$days= rownames(mortality_3)
mortality_3$group = "3"
colnames(mortality_3) = c("mortality","days","group")
mortality_3 = mortality_3[-5,]

morta = rbind(mortality_1,mortality_2,mortality_3)
morta$days = factor(morta$days,levels = c("survive_30days","survive_90days","survive_180days","survive_365days"))
}


ggplot(morta,aes(x = days,y=mortality,fill=group))+
  geom_bar(stat = "identity",width = 0.6, position = position_dodge(0.7))+
  geom_text(aes(label = round(morta$mortality*100,2)),size = 3, colour = 'black', vjust = 0, hjust = .3, position = position_dodge(0.9))+
  ylim(0,0.7)+
  theme_minimal()+
  scale_fill_brewer(palette = "Set2")


## table2 creation
### cox回归
colnames(dataset)
## 构建连续变量
covar_conti_1 = c("aniongap_min")
covar_conti_2 = c("gender","admission_age","aniongap_min")
covar_conti_3 = vars[!vars%in%append(outcomename,"ag_group_min")] 
## 构建分位数变量
covar_1 = c("ag_group_min")
covar_2 = c("gender","admission_age","ag_group_min")
covar_3 = vars[!vars%in%append(outcomename,"aniongap_min")]
## 设定生存数变量
dataset$status= as.numeric(dataset$survive_30days)

## 构建方程（连续）
mult_formula_conti_1 = as.formula(paste('Surv(survive_days,status)~',covar_conti_1))
mult_formula_conti_2 = as.formula(paste('Surv(survive_days,status)~',str_c(covar_conti_2,collapse = "+")))
mult_formula_conti_3 = as.formula(paste('Surv(survive_days,status)~',str_c(covar_conti_3,collapse = "+")))
## 构建方程（分位数）
multi_formula_1 = as.formula(paste('Surv(survive_days,status)~',covar_1))
multi_formula_2 =  as.formula(paste('Surv(survive_days,status)~',str_c(covar_2,collapse = "+")))
multi_formula_3 =  as.formula(paste('Surv(survive_days,status)~',str_c(covar_3,collapse = "+")))
## make formula for p trend, change key variat to numeric
multi_formula_p1 =  as.formula(paste('Surv(survive_days,status)~',"as.numeric(ag_group_min)"))
multi_formula_p2 =  as.formula(paste('Surv(survive_days,status)~',str_c(covar_2[!covar_2%in%c("ag_group_min")],collapse = "+"),"+as.numeric(ag_group_min)"))
multi_formula_p3 =  as.formula(paste('Surv(survive_days,status)~',str_c(covar_3[!covar_3%in%c("ag_group_min")],collapse = "+"),"+as.numeric(ag_group_min)"))
#multi_formula_4
multi_formula_p1
multi_formula_p2
multi_formula_p3
## 构建结局列表
survivedays = c("survive_30days","survive_90days","survive_180days","survive_365days")

## model I  计算
table2_1 = data.frame()
for (i in 1:length(survivedays)){
print(survivedays[i])  
## 设定终点
dataset$status= as.numeric(dataset[,survivedays[i]])
## 执行回归（连续）
multi_cox_conti_1<-coxph(mult_formula_conti_1,data=dataset)
## 执行回归（分位数）
multi_cox_1<-coxph(multi_formula_1,data=dataset)
## p for trend regression
multi_cox_p1 = coxph(multi_formula_p1,data = dataset)
summary(multi_cox_p1)
table_temp = rbind(tidy(multi_cox_conti_1,exponentiate = TRUE)%>%filter(term == "aniongap_min"),
                   tidy(multi_cox_1,exponentiate = TRUE)%>%filter(grepl("ag_group_min",term)),
                   tidy(multi_cox_p1,exponentiate = TRUE)%>%filter(grepl("ag_group_min",term))
                   )

table2_1 = rbind(table2_1,table_temp)
}

## model II  计算
table2_2 = data.frame()
for (i in 1:length(survivedays)){
  print(survivedays[i])  
  ## 设定终点
  dataset$status= as.numeric(dataset[,survivedays[i]])
  ## 执行回归（连续）
  multi_cox_conti_2<-coxph(mult_formula_conti_2,data=dataset)
  ## 执行回归（分位数）
  multi_cox_2<-coxph(multi_formula_2,data=dataset)
  ## p for trend regression
  multi_cox_p2 = coxph(multi_formula_p2,data = dataset)
  table_temp = rbind(tidy(multi_cox_conti_2,exponentiate = TRUE)%>%filter(term == "aniongap_min"),
                     tidy(multi_cox_2,exponentiate = TRUE)%>%filter(grepl("ag_group_min",term)),
                     tidy(multi_cox_p2,exponentiate = TRUE)%>%filter(grepl("ag_group_min",term))
  )
  
  table2_2 = rbind(table2_2,table_temp)
}

## model III 计算
table2_3 = data.frame()
for (i in 1:length(survivedays)){
  print(survivedays[i])  
  ## 设定终点
  dataset$status= as.numeric(dataset[,survivedays[i]])
  ## 执行回归（连续）
  multi_cox_conti_3<-coxph(mult_formula_conti_3,data=dataset)
  ## 执行回归（分位数）
  multi_cox_3<-coxph(multi_formula_3,data=dataset)
  ## p for trend regression
  multi_cox_p3 = coxph(multi_formula_p3,data = dataset)
  table_temp = rbind(tidy(multi_cox_conti_3,exponentiate = TRUE)%>%filter(term == "aniongap_min"),
                     tidy(multi_cox_3,exponentiate = TRUE)%>%filter(grepl("ag_group_min",term)),
                     tidy(multi_cox_p3,exponentiate = TRUE)%>%filter(grepl("ag_group_min",term))
  )
  
  table2_3 = rbind(table2_3,table_temp)
}

table2 = cbind(table2_1,table2_2,table2_3)

write.csv(table2,"table2.csv")


### 

ggplot(dataset,aes(x=aniongap_min))+
  geom_histogram()

max(dataset$aniongap_min)

min(dataset$aniongap_min)

data30bar = dataset%>%select(c("aniongap_min","survive_30days"))

data30bar[data30bar$aniongap_min<=10,]["group"] =1
data30bar[data30bar$aniongap_min>=20,]["group"] =11
data30bar[(data30bar$aniongap_min>10) & (data30bar$aniongap_min<20),]["group"]=
  data30bar[(data30bar$aniongap_min>10) & (data30bar$aniongap_min<20),]["aniongap_min"] -9

fig2_table = data.frame()

#=1

for(i in 1:11){
  data = data30bar[data30bar$group ==i,]
  mortaliy30 = round(table(data$survive_30days)[2]*100/length(data[,1]),2)
  temp = data.frame(i,mortaliy30)
  colnames(temp) = c("group","mortality")
  fig2_table = rbind(fig2_table,temp)
}

ggplot(fig2_table,aes(x=group,y=mortality))+
  geom_bar(stat = "identity",width = 0.6, position = position_dodge(0.7),fill = "darkgreen")+
  geom_smooth(se=F) +
  #stat_smooth(method = lm, se =T)+
  #ylim(0,0.7)+
  theme_minimal()+
  scale_fill_brewer(palette = "Set2")
  