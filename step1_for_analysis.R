{
rm(list = ls())
library(GEOquery)
library(dplyr)
library(tidyverse)
library(stringr)
library(limma)
GSE_name = 'GSE57065'
path = str_c('D:\\myresearch\\sepsis\\GEOdata\\',GSE_name)
setwd(path)
}
{
gset = getGEO(GSE_name,destdir=".",getGPL = T)   #get GSE raw file and GLP file
#str(gset)
gset_1 = gset[[1]]     ## 获取ExpressionSet对象，包括的表达矩阵和分组信息
exprSet=exprs(gset_1)  ## get express matrix
pdata=pData(gset_1)  ##get group information
}
#make group dataframe
group_info <- pdata[,c(1:2,34:37)]



#table(group_list$group)
group_list <- factor(group_info$`collection time:ch1`)
# boxplot check
tiff(filename = str_c(GSE_name,"_boxplot_orginal.tif"),width = 8, height = 6, units = "in", pointsize = 12,
     compression = "lzw",
     bg = "white", res = 300, family = "", restoreConsole = TRUE,
     type = "windows")
boxplot(exprSet,outline = F,notch = T,las = 2,col = group_list)
dev.off()
#normalize data
exprSet = normalizeBetweenArrays(exprSet)
tiff(filename = str_c(GSE_name,"_boxplot_normalized.tif"),width = 8, height = 6, units = "in", pointsize = 12,
     compression = "lzw",
     bg = "white", res = 300, family = "", restoreConsole = TRUE,
     type = "windows")
boxplot(exprSet,outline=FALSE, notch=T,col=group_list, las=2)
dev.off()

#log2 change
ex <- exprSet
qx <- as.numeric(quantile(ex, c(0., 0.25, 0.5, 0.75, 0.99, 1.0), na.rm=T))
LogC <- (qx[5] > 100) ||
  (qx[6]-qx[1] > 50 && qx[2] > 0) ||
  (qx[2] > 0 && qx[2] < 1 && qx[4] > 1 && qx[4] < 2)

if (LogC) { ex[which(ex <= 0)] <- NaN
exprSet <- log2(ex)
print("log2 transform finished")}else{print("log2 transform not needed")}


# annotate gene No.2
GPL_name = annotation(gset_1)
GPL = getGEO(GPL_name,destdir = '.')
GPL_anno <- Table(GPL)    #get gene annotation dataframe, 'Table' is from bioconductor

probe2symbol_df <- GPL_anno[,c("ID","Gene Symbol")] 
colnames(probe2symbol_df) <- c("probe_id","symbol")


# annotate gene  NO.1 
#BiocManager::install('hugene10sttranscriptcluster.db')
#library(hugene10sttranscriptcluster.db)
#probe2symbol_df <- toTable(get("hugene10sttranscriptclusterSYMBOL"))


exprSet <- as.data.frame(exprSet)  #change express matrix to dataframe
exprSet$probe_id <- rownames(exprSet)  #make a new column of probe_id by rowname
exprSet$probe_id <- as.character(exprSet$probe_id)
#match probe_id and gene symbol
exprSet <- exprSet %>% 
  inner_join(probe2symbol_df,by="probe_id") %>% #合并探针的信息
  dplyr::select(-probe_id) %>% #去掉多余信息
  dplyr::select(symbol, everything()) %>% #重新排列，
  mutate(rowMean =rowMeans(.[grep("GSM", names(.))])) %>% #求出平均数(这边的.真的是画龙点睛)
  arrange(desc(rowMean)) %>% #把表达量的平均值按从大到小排序
  distinct(symbol,.keep_all = T) %>% # symbol留下第一个
  dplyr::select(-rowMean) %>% #反向选择去除rowMean这一列
  tibble::column_to_rownames(colnames(.)[1]) # 把第一列变成行名并删除

exprSet <- as.matrix(exprSet)
write.csv(exprSet,file  = str_c(GSE_name,"_exprSet_annoted.csv"))
save.image('for_analysis.RData')
