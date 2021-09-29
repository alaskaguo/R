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
library(MatchIt)


path = 'f:\\myresearch\\房颤数据挖掘'
setwd(path)
data = read.csv('data_filled.csv',row.names = 1)

## 制作单一值计数统计表
unique_count = as.list(apply(data,2,function(x){length(unique(x))}))
unique_status <- data.frame()
for (i in 1:length(unique_count)){
  unique_status <- rbind(unique_status,data.frame(names(unique_count[i]),unique_count[i][[1]] ))
}
colnames(unique_status) <- c('items','unique_status')

## 删除只有一个值的列
data = data[,!colnames(data)%in%unique_status[unique_status$unique_status ==1,1]]

##  数据种类小于5的变量转为字符串
to_char = unique_status[unique_status$unique_status<5,][,'items']

data[,colnames(data)%in%to_char] <- apply(data[,colnames(data)%in%to_char],2,function(x)as.character(x))
data[,!colnames(data)%in%to_char] <- apply(data[,!colnames(data)%in%to_char],2,function(x)as.numeric(x))
str(data)

data$Procedure_1 <- ifelse(data$Procedure_1 == '1',T,F)

table(data$Procedure_1)

##  对连续变量进行正太分布检验
contiouns_vars = select_if(data,is.numeric)
plist = as.data.frame(apply(contiouns_vars, 2, function(x)shapiro.test(x)$p.value))
## 更改P值表列名
colnames(plist) = "pvalue"
## 提取非正太分布数据行名
nonnormal_vars = rownames(subset(plist,pvalue < 0.05))

strata = 'Procedure_1'
## 生成自变量名向量
vars = colnames(data)[!colnames(data)%in%c(strata,'Surgery_2')]
## 生成表1对像，按训练集分许
tab_Unmatched <- CreateTableOne(vars = vars, strata = strata, data = data,
                               addOverall = TRUE)
## 打印表1
tabUnmatched = as.data.frame(print(tab_Unmatched,
                                   explain = F,
                                   showAllLevels = F,
                                   nonnormal = nonnormal_vars,
                                   printToggle = F,
                                   dropEqual = T,
                                   varLabels = T,
                                   smd = T))
write.csv(tabUnmatched,"匹配前基线表.csv")

## Fit model
var_psm = colnames(data)[str_detect(colnames(data),'_psm')]
match_formula = as.formula(str_c(c('as.numeric(',strata,')~',str_c(var_psm,collapse = "+"))))    ## 生成公式
match_formula

psModel <- glm(match_formula, 
               family  = binomial(link = "logit"),
               data    = data)
#计算倾向评分ps
data$psvalue <- predict(psModel,type="response")

data$Procedure_1

#计算逆概率权重IPTW
data$iptw <- ifelse(data$Procedure_1 == T,1/data$psvalue,1/(1-data$psvalue))

#1-提取IPTW后的数据
dataIPTW = svydesign(ids=~1,data=data,weights= ~iptw) 

#2-再次构建Table-1
tab_IPTW = svyCreateTableOne(vars=vars, strata=strata,data=dataIPTW,test=T) 

#标准化差结果
print(tab_IPTW,showAllLevels=TRUE,smd=TRUE)
tableIPTW = as.data.frame(print(tab_IPTW,
                                explain = F,
                                showAllLevels = F,
                                nonnormal = nonnormal_vars,
                                printToggle = F,
                                dropEqual = T,
                                varLabels = T,
                                smd = T))
write.csv(tableIPTW,"IPTW基线表.csv")

## 进行PSM
match.it = matchit(match_formula, data = data, method="nearest" ,ratio=1)  ## 生成匹配对象

df_match <- match.data(match.it)

##  提取匹配后结局数据
table_mathced <- CreateTableOne(vars = vars, strata = "Procedure_1", data = df_match, test = TRUE,
                               addOverall = TRUE)

tablemathced = as.data.frame(print(table_mathced,
                                   explain = F,
                                   showAllLevels = F,
                                   nonnormal = nonnormal_vars,
                                   printToggle = F,
                                   dropEqual = T,
                                   varLabels = T,
                                   smd = T))
write.csv(tablemathced,"匹配后基线表.csv")

table_1 = cbind(tabUnmatched,tablemathced,tableIPTW)
write.csv(table_1,'table_1.csv')

#查看是否有SMD>10%的混杂因素  ## 对象是creatableone对象
addmargins(table(ExtractSmd(tab_IPTW) > 0.1))


library(ggplot2)
#提取作图数据
dataPlot <- data.frame(variable=rownames(ExtractSmd(tab_Unmatched)),
                       Unmatched=as.numeric(ExtractSmd(tab_Unmatched)),
                       IPTW=as.numeric(ExtractSmd(tab_IPTW)),
                       matched = as.numeric(ExtractSmd(table_mathced))
                      )


#指定将要出现在图中的变量
dataPlotMelt<-melt(data= dataPlot,
                   id.vars=c("variable"),
                   variable.name= "Method",
                   value.name= "SMD")

#
varNames <-as.character(dataPlot$variable)[order(dataPlot$Unmatched)]
#
dataPlotMelt$variable<- factor(dataPlotMelt$variable,
                               levels = varNames)
#画图
ggplot(data = dataPlotMelt,
       mapping = aes(x = variable, y = SMD, 
                     group = Method, 
                     color = Method,
                     shape = Method )) +
  #geom_line() +
   geom_point(size=4) +
   geom_hline(yintercept = 0.1, 
             color = "red",
             lty=2,
             size = 0.1) +
  coord_flip() +
  theme_bw(base_size = 18)

## 绘制生存曲线

time = 'Time_to_relapse'
status = 'as.numeric(recurrence)'
strata = 'Procedure_1'

## 以下为固定代码,数据集需要改变
km_formula = as.formula(str_c(c('Surv(',time,',',status,') ~', strata),collapse = ''))
km_formula
#km_formula
fit_1 <- surv_fit(km_formula, data = data)    ### 注意,用surv_fit,不用surfit

ggsurvplot(fit_1,
           pval = TRUE, conf.int = F,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"),
           data=data,
           fun = "event"
)
## 固定代码到此结束

#km_formula
fit_2 <- surv_fit(km_formula, data = df_match)    ### 注意,用surv_fit,不用surfit

ggsurvplot(fit_2,
           pval = TRUE, conf.int = F,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"),
           data=df_match,
           fun = "event"
)

fit_3<- surv_fit(km_formula,  
                 weights=data$iptw,# 创建生存对象 
                 data = data) # 数据集来源

ggsurvplot(fit_3,
           pval = TRUE, conf.int = F,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"),
           data=df_match,
           fun = "event"
)



