#!/usr/bin/env python
# coding: utf-8

# In[41]:


import pandas as pd
import numpy as np
import seaborn as sns
import os
import re
import matplotlib.pyplot as plt


# In[2]:


path = r"F:\myresearch\micu\data"
os.chdir(path)


# In[3]:


data = pd.read_csv("rawdata.csv")


# In[11]:


unique_count = data.apply(lambda x:len(x.unique()))


# In[13]:


data[list(unique_count[unique_count.values<5].index)] = data[list(unique_count[unique_count.values<5].index)].astype("object")


# In[21]:


## 离群值查看
for i in data.select_dtypes("int64").columns:
    plt.figure()
    sns.boxplot(x=data[i]);


# In[51]:


## 手工输入删除列和修改列
to_del = ["chloride_max","glucose_min_x","hematocrit_min","potassium_max","sodium_max","wbc_max","sysbp_max","glucose_max_y","glucose_min_y"]

to_clean = ["urineoutput","creatinine_max","chloride_min","glucose_max_x","platelet_min","ptt_max","inr_max","pt_max","sodium_min","bun_max",
            "wbc_min","sysbp_min","resprate_max","spo2_min","tempc_max","weight_first"]


# In[32]:


data.drop(columns=to_del,axis=1,inplace=True)


# In[34]:


data.drop(columns=["ag_group_max"],axis=1,inplace=True)


# In[37]:


data.drop(columns=["ethnicity_grouped"],axis=1,inplace=True)


# In[42]:


## 计算四分位间距  暂时没用
lower_q=np.quantile(data.wbc_min,0.25,interpolation='lower')#下四分位数
higher_q=np.quantile(data.wbc_min,0.75,interpolation='higher')#上四分位数
int_r=higher_q-lower_q  #四分位距


# In[39]:


## 分类变量汇总
data.select_dtypes("object").apply(lambda x:x.value_counts())


# In[46]:


def cap(x,quantile=[0.01,0.99]):
    """
    盖帽法处理异常值
    Args：
    x：pd.Series列，连续变量
    quantile：指定盖帽法的上下分位数范围
    """
    # 生成分位数
    Q01,Q99=x.quantile(quantile).values.tolist()
    # 替换异常值为指定的分位数
    if Q01 > x.min():
        x = x.copy()
        x.loc[x<Q01] = Q01
    
    if Q99 < x.max():
        x = x.copy()
        x.loc[x>Q99] = Q99

    return(x)


# In[58]:


data[to_clean] = data[to_clean].apply(lambda x:cap(x))


# In[59]:


data.to_csv("rawdata_cleaned.csv")


# In[55]:


## 查看清洗后数据
for i in data_1.columns:
    plt.figure()
    sns.boxplot(x=data_1[i]);


# In[ ]:




