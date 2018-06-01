# -*- coding: utf-8 -*-
"""
Created on Tue May 15 15:34:10 2018

@author: Tiago
"""
import os
import pandas as pd
from sklearn.feature_selection import RFE
from sklearn.linear_model import LogisticRegression


  
def support(path):
    os.chdir(path)
    df=pd.read_excel('datasets\\normalizedDataset.xlsx')

    df_vars=df.columns.values.tolist()
    y=['isChurn']
    X=[i for i in df_vars if i not in y]



    model = LogisticRegression()

    rfe = RFE(model, 10)
    rfe = rfe.fit(df[X], df[y])
    print( rfe.support_)
