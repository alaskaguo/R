rm(list = ls())
library(GEOquery)
library(stringr)
GSE_name = 'GSE11971'
path = str_c('D:/myresearch/GEO/')
setwd(path)
dir.create(GSE_name)

setwd(str_c(path,GSE_name))

gset = getGEO(GSE_name,destdir=".",getGPL = T)   #get GSE raw file and GLP file





