#!/usr/bin/env python
# coding: utf-8

# In[218]:


import pandas as pd
import vaex
import os
import datetime
import re
from scipy import stats
from tableone import TableOne


# In[140]:


path = r"E:\mimiciii"
os.chdir(path)


# In[141]:


admissons = pd.read_csv(r".\Derived\tables\icustay_with_survival.csv")

icustays = pd.read_csv(r".\zipfiles\icustays.csv.gz") 
print("总病例数:"+str(admissons.shape[0]))


# In[142]:


micu_patient = admissons[(admissons.icustay_id.isin(icustays[icustays.LAST_CAREUNIT == "MICU"].ICUSTAY_ID))]
print("MICU病例数:"+str(micu_patient.shape[0]))


# In[143]:


micu_patient_first = micu_patient[micu_patient.first_icu_stay == 1]
print("首次入MICU病例数:"+str(micu_patient_first.shape[0]))


# In[144]:


micu_patient_first_adult = micu_patient_first[micu_patient_first.admission_age >= 18]
print("大于18岁病例数:"+str(micu_patient_first_adult.shape[0]))


# In[145]:


micu_patient_first_adult_1day = micu_patient_first_adult[micu_patient_first_adult.los_icu >= 1]
print("入院大于24小时病例数:"+str(micu_patient_first_adult_1day.shape[0]))


# In[146]:


## 读入诊断库
d_dias = pd.read_csv(r".\zipfiles\D_ICD_DIAGNOSES.csv.gz")
dias = pd.read_csv(".\zipfiles\DIAGNOSES_ICD.csv.gz")


# In[147]:


res_ids = [str(i) for i in list(range(46,52))]

resp_dias = d_dias[d_dias.ICD9_CODE.apply(lambda x:x[:2]).isin(res_ids)]

resp_patient = dias[dias.ICD9_CODE.isin(resp_dias.ICD9_CODE)]

micu_resp = micu_patient_first_adult_1day[micu_patient_first_adult_1day.hadm_id.isin(resp_patient.HADM_ID)]

print("呼吸道疾病"+str(micu_resp.shape[0]))


# In[148]:


def explore_death(data):
    """
    死亡情况探索
    data:pandas dataframe,从admissions 提取的病人子集
    """
    survival_count = data[data.columns[data.columns.str.contains("survive")][1:]].apply(lambda x:x.value_counts())
    return survival_count

def diagonses_sum(data):
    """
    诊断统计汇总
    data:pandas dataframe,从admissions 提取的病人子集
    """
    global dias
    patients_dias = pd.merge(data["hadm_id"],dias[["hadm_id","seq_num","icd9_code"]],on="hadm_id",how="left")
        
    top30_sum = patients_dias.icd9_code.value_counts()[:30]
        
    diagnoses_sum = pd.merge(d_dias[d_dias.icd9_code.isin(top30_sum.index)],
                                    pd.DataFrame(top30_sum).reset_index().rename(columns={"index":"icd9_code","icd9_code":"sum"}),
                                    on="icd9_code").sort_values(by=["sum"],ascending=False)
        
    patients_first_dias = patients_dias[patients_dias.seq_num == 1]
        
    top30_first_dias_patients_sum = patients_first_dias.icd9_code.value_counts()[:30]

    first_diagnosis_sum = pd.merge(d_dias[d_dias.icd9_code.isin(top30_first_dias_patients_sum.index)],
                 pd.DataFrame(top30_first_dias_patients_sum).reset_index().rename(columns={"index":"icd9_code","icd9_code":"sum"}),
                 on="icd9_code").sort_values(by=["sum"],ascending=False)
    return diagnoses_sum,first_diagnosis_sum   


def explore_aki(data):
    """
    肾衰情况探索
    data:pandas dataframe,从admissions 提取的病人子集
    """
    aki_count = data[data.columns[data.columns.str.contains("aki")]].apply(lambda x:x.value_counts())
    return aki_count


# In[149]:


explore_death(micu_resp)


# In[150]:


## 读入化验单数据
lab_first_day = pd.read_csv(r"./Derived/tables/firstday/lab_first_day.csv")


# In[151]:


lab_first_day.columns


# In[152]:


micu_resp = pd.merge(micu_resp,lab_first_day.drop(columns=["subject_id","hadm_id"]),on="icustay_id",how="left")


# In[154]:


micu_resp = micu_resp[micu_resp.aniongap_min.notna()]
print("含有AG结果："+str(micu_resp.shape[0]))


# In[155]:


micu_resp.drop(columns=["subject_id_x","dod",'admittime',
       'dischtime','ethnicity','hospstay_seq',
       'first_hosp_stay', 'intime', 'outtime', 'icustay_seq',
       'first_icu_stay','subject_id_y', 'peptic_ulcer', 'aids', 'lymphoma',
       'metastatic_cancer','rheumatoid_arthritis',
       'coagulopathy', 'weight_loss', 'fluid_electrolyte',
       'blood_loss_anemia', 'deficiency_anemias', 'drug_abuse', 'psychoses', 'depression','cardiac_arrhythmias', 'valvular_disease', 'pulmonary_circulation',
       'peripheral_vascular', 'paralysis',
       'other_neurological', 'hypothyroidism','hemoglobin_min','bicarbonate_max','creatinine_min','chloride_min', 'chloride_max', 'glucose_min',
                       'hematocrit_max', 'hemoglobin_max','platelet_max','ptt_min','inr_min', 'pt_min','bun_min',  'wbc_min' ,"hypertension","renal_failure",
                        "obesity","alcohol_abuse"],axis = 1,inplace = True)


# In[156]:


na_sum = micu_resp.isna().sum()


# In[157]:


col_del = na_sum[na_sum.values>len(micu_resp)*0.2].index

col_del = list(col_del)

col_del.remove("survive_days")


# In[158]:


micu_resp.drop(columns=col_del,axis=1,inplace=True)


# In[159]:


micu_resp.loc[micu_resp.aniongap_min<12,"ag_group_min"] = 1

micu_resp.loc[(micu_resp.aniongap_min>=12) & (micu_resp.aniongap_min<=14),"ag_group_min"] = 2

micu_resp.loc[micu_resp.aniongap_min>14,"ag_group_min"] = 3

micu_resp.ag_group_min = micu_resp.ag_group_min.astype("object")


# In[160]:


micu_resp.loc[micu_resp.aniongap_max<12,"ag_group_max"] = 1

micu_resp.loc[(micu_resp.aniongap_max>=12) & (micu_resp.aniongap_max<=14),"ag_group_max"] = 2

micu_resp.loc[micu_resp.aniongap_max>14,"ag_group_max"] = 3

micu_resp.ag_group_max = micu_resp.ag_group_max.astype("object")


# In[161]:


micu_resp.ag_group_max.value_counts()


# In[162]:


micu_resp.ag_group_min.value_counts()


# In[113]:


def mkdir(path):
    # 引入模块
    import os
    # 去除首位空格
    path=path.strip()
    # 去除尾部 \ 符号
    path=path.rstrip("\\")
    # 判断路径是否存在
    # 存在     True
    # 不存在   False
    isExists=os.path.exists(path)
    # 判断结果
    if not isExists:
        # 如果不存在则创建目录
        # 创建目录操作函数
        os.makedirs(path) 
        print(path+' 创建成功')
        return True
    else:
        # 如果目录存在则不创建，并提示目录已存在
        print(path+' 目录已存在')
        return False


# In[163]:


# 定义要创建的目录
mkpath=r"e:\micu\data"
# 调用函数
mkdir(mkpath)


# In[164]:


micu_resp.to_csv(r"..\micu\data\rawdata.csv")


# In[214]:


micu_resp.info()


# In[133]:


columns = list(micu_resp.columns).remove(["hadm_id","icustay_id"])


# In[176]:


ks_result = pd.DataFrame(micu_resp.select_dtypes("float64").apply(lambda x:stats.kstest(x,"norm")),columns=["stats"])    ## 不能有空值


# In[194]:


tocategory = pd.Series(micu_resp.select_dtypes("int64").columns)


# In[210]:


tocategory = tocategory[~tocategory.isin(["hadm_id","icustay_id"])]


# In[213]:


micu_resp.loc[:,tocategory] = micu_resp.loc[:,tocategory].apply(lambda x:x.astype("object"))


# In[241]:


data = micu_resp
columns = list(data.columns)
categorical = list(micu_resp.select_dtypes("object").columns)
groupby = "ag_group_min"
nonnormal = []


# In[275]:


mytable = TableOne(data                       ## 数据集
                   ,columns=columns           ## 列名列表
                   ,categorical=categorical     ## 分类变量名列表
                   ,groupby=groupby              ## 分组变量名，str
                   #,nonnormal=nonnormal
                   ,pval=True
                   ,missing=False             ## 缺失值显示
                   ,display_all=True
                   ,overall=False
                   #,htest_name=True           ## 显示检验方式
                   ,rename = {'gender':'Gender', 'admission_age':'Age','ethnicity_grouped':"Ethnicity",
                              'wbc_max':'WBC'}
                   ) 


# In[276]:


## 非正态分布列名列表
print(mytable)     
"""
Display the table using the tabulate method. The tablefmt argument allows the table to be displayed in multiple formats, 
including “github”, “grid”, “fancy_grid”, “rst”, “html”, and “latex”.
"""


# In[261]:


mytable.to_csv('pythontable1.csv')


# In[262]:


help(TableOne)


# In[ ]:




