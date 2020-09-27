#!/usr/bin/env python
# coding: utf-8

# In[ ]:


# Notes
# Might be good to filter out when gender/age/location is None or NULL. 
# Definitely need to filter out by min sample size. 


# There are 4 final output tables:
# 1. Raw Country: all_aug_unioned_indexed
# 2. Smoothed Country: all_aug_unioned_smoothed_filtered
# 3. Raw Region: all_region_aug_unioned_indexed
# 4. Smoothed Region: all_region_aug_unioned_smoothed_filtered


# In[ ]:


# Basic importing to make things easier to query

import pandas as pd
import os
import glob
from datetime import datetime
import math
import numpy as np
import timeit

pd.set_option('display.max_rows', 100)
pd.set_option('display.max_columns', 10)
pd.set_option('display.width', 10)
pd.set_option('display.max_colwidth', 20)


# In[ ]:


# Initializing data

file_dict = {}
pre_aug_data = pd.DataFrame()
aug_data = pd.DataFrame()


# In[ ]:


# Downloading data to union. NOTE: THIS MIGHT CHANGE DEPENDING ON WHERE DATA IS

for root,dirs,files in os.walk('PATH_TO_FILES'):
    for file in files:
        data = pd.read_csv('PATH_TO_FILES' + file)
        file_dict[file] = data


# In[ ]:


# The data before August had fewer columns, so I basically separate August data and pre-August dataa. 
# This is to make it easier to calculate/trust the queries that I'm working on.

for key in file_dict:
    if '2020-08' in key:
        aug_data = pd.concat([aug_data, file_dict[key]])
    else:
        pre_aug_data = pd.concat([pre_aug_data, file_dict[key]])


# In[ ]:


# Defining different functions to help map things appropriately from survey -> calculations.

# This helps map the ages to buckets
def age_map(col):
    x = col.replace({1: '18-34',
                     2: '18-34',
                     3: '35-54',
                     4: '35-54',
                     5: '55+',
                     6: '55+',
                     7: '55+', 
                     -99: 'None'
                    })
    return x
    

# This helps map genders to buckets
def gender_map(col):
    x = col.replace({1:'male',
                    2:'female',
                    3: 'other',
                    4: 'other', 
                    -99: 'None'})
    return x
 

# This helps calculate CLI. Please check to make sure this is accurate! 
def is_cli(row):
    if row['B1_1'] == 1 and (row['B1_2'] == 1 or row['B1_7'] == 1):
        return 1
    elif math.isnan(row['B1_1']) or row['B1_1'] == -99:
        return float('NaN')
    else:
        return 0
    
# This helps calculate ILI. Please check to make sure this is accurate!
def is_ili(row):
    if row['B1_1'] == 1 and (row['B1_2'] == 1 or row['B1_3'] == 1):
        return 1
    elif math.isnan(row['B1_1']) or row['B1_1'] == -99:
        return None
    else:
        return 0
    

# This is a general cleaning function, that allows us to map different columsn to a true 0/1 binary variable.    
def clean_var(col, ls, real_val=1):
    x = col.replace(ls, 0)
    y = x.replace(real_val, 1)
    return y
  

# Storing this for easier access in the future, generally the numbers we want to map to 0 are 2 and -99
zero_map = [2, -99]


# In[ ]:


# Iterate over demographic data and mapping it to binaries. Also creating an overall column for easier aggregation. 
aug_data['date'] = pd.to_datetime(aug_data['RecordedDate']).dt.date
aug_data['age_bucket'] = age_map(aug_data['E4'])
aug_data['age_bucket_overall'] = 'overall'
aug_data['gender'] = gender_map(aug_data['E3'])
aug_data['gender_overall'] = 'overall'

# Iterate over signals and map to 0/1 binaries. The CLI/ILI calculations take the longest, because they use .apply.
aug_data['cli'] = aug_data.apply(is_cli, axis=1)
aug_data['ili'] = aug_data.apply(is_ili, axis=1)
aug_data['B1_1_logic'] = clean_var(aug_data['B1_1'], zero_map)
aug_data['B1_2_logic'] = clean_var(aug_data['B1_2'], zero_map)
aug_data['B1_3_logic'] = clean_var(aug_data['B1_3'], zero_map)
aug_data['B1_4_logic'] = clean_var(aug_data['B1_4'], zero_map)
aug_data['B1_5_logic'] = clean_var(aug_data['B1_5'], zero_map)
aug_data['B1_6_logic'] = clean_var(aug_data['B1_6'], zero_map)
aug_data['B1_7_logic'] = clean_var(aug_data['B1_7'], zero_map)
aug_data['B1_8_logic'] = clean_var(aug_data['B1_8'], zero_map)
aug_data['B1_9_logic'] = clean_var(aug_data['B1_9'], zero_map)
aug_data['B1_10_logic'] = clean_var(aug_data['B1_10'], zero_map)
aug_data['B1_11_logic'] = clean_var(aug_data['B1_11'], zero_map)
aug_data['B1_12_logic'] = clean_var(aug_data['B1_12'], zero_map)
aug_data['B1_13_logic'] = clean_var(aug_data['B1_13'], zero_map)
aug_data['B3_logic'] = clean_var(aug_data['B3'], zero_map)
aug_data['B6_logic'] = clean_var(aug_data['B6'], zero_map)
aug_data['B7_logic'] = clean_var(aug_data['B7'], zero_map)
aug_data['C0_1_logic'] = clean_var(aug_data['C0_1'], zero_map)
aug_data['C0_2_logic'] = clean_var(aug_data['C0_2'], zero_map)
aug_data['C0_3_logic'] = clean_var(aug_data['C0_3'], zero_map)
aug_data['C0_4_logic'] = clean_var(aug_data['C0_4'], zero_map)
aug_data['C0_5_logic'] = clean_var(aug_data['C0_5'], zero_map)
aug_data['C0_6_logic'] = clean_var(aug_data['C0_6'], zero_map)
aug_data['C1_m_logic'] = clean_var(aug_data['C1_m'], zero_map)
aug_data['C5_1_logic'] = clean_var(aug_data['C5'], [2, 3, 4, 5, 6, -99], 1)
aug_data['C5_2_logic'] = clean_var(aug_data['C5'], [1, 3, 4, 5, 6, -99], 2)
aug_data['C5_3_logic'] = clean_var(aug_data['C5'], [1, 2, 4, 5, 6, -99], 3)
aug_data['C5_4_logic'] = clean_var(aug_data['C5'], [1, 2, 3, 5, 6, -99], 4)
aug_data['C5_5_logic'] = clean_var(aug_data['C5'], [1, 2, 3, 4, 6, -99], 5)
aug_data['C5_6_logic'] = clean_var(aug_data['C5'], [1, 2, 3, 4, 5, -99], 6)

# Additional aggs D1-D5
# D1
aug_data['D1_1_logic'] = clean_var(aug_data['D1'], [2, 3, 4, 5, -99], 1)
aug_data['D1_2_logic'] = clean_var(aug_data['D1'], [1, 3, 4, 5, -99], 2)
aug_data['D1_3_logic'] = clean_var(aug_data['D1'], [1, 2, 4, 5, -99], 3)
aug_data['D1_4_logic'] = clean_var(aug_data['D1'], [1, 2, 3, 5, -99], 4)
aug_data['D1_5_logic'] = clean_var(aug_data['D1'], [1, 2, 3, 4, -99], 5)

# D2
aug_data['D2_1_logic'] = clean_var(aug_data['D2'], [2, 3, 4, 5, -99], 1)
aug_data['D2_2_logic'] = clean_var(aug_data['D2'], [1, 3, 4, 5, -99], 2)
aug_data['D2_3_logic'] = clean_var(aug_data['D2'], [1, 2, 4, 5, -99], 3)
aug_data['D2_4_logic'] = clean_var(aug_data['D2'], [1, 2, 3, 5, -99], 4)
aug_data['D2_5_logic'] = clean_var(aug_data['D2'], [1, 2, 3, 4, -99], 5)

# D3
aug_data['D3_1_logic'] = clean_var(aug_data['D3'], [2, 3, 4, 5, -99], 1)
aug_data['D3_2_logic'] = clean_var(aug_data['D3'], [1, 3, 4, 5, -99], 2)
aug_data['D3_3_logic'] = clean_var(aug_data['D3'], [1, 2, 4, 5, -99], 3)
aug_data['D3_4_logic'] = clean_var(aug_data['D3'], [1, 2, 3, 5, -99], 4)

# D4
aug_data['D4_1_logic'] = clean_var(aug_data['D4'], [2, 3, 4, 5, -99], 1)
aug_data['D4_2_logic'] = clean_var(aug_data['D4'], [1, 3, 4, 5, -99], 2)
aug_data['D4_3_logic'] = clean_var(aug_data['D4'], [1, 2, 4, 5, -99], 3)
aug_data['D4_4_logic'] = clean_var(aug_data['D4'], [1, 2, 3, 5, -99], 4)

# D5
aug_data['D5_1_logic'] = clean_var(aug_data['D5'], [2, 3, 4, 5, -99], 1)
aug_data['D5_2_logic'] = clean_var(aug_data['D5'], [1, 3, 4, 5, -99], 2)
aug_data['D5_3_logic'] = clean_var(aug_data['D5'], [1, 2, 4, 5, -99], 3)
aug_data['D5_4_logic'] = clean_var(aug_data['D5'], [1, 2, 3, 5, -99], 4)


# In[ ]:


# I calculated the weighted sum variables here, which will help me in the future. 
# This array below helps me iterate over just the signals, and ignore all the other data. 
agg_var =  ['cli', 'ili', 'B1_1_logic', 'B1_2_logic', 'B1_3_logic', 'B1_4_logic', 'B1_5_logic', 'B1_6_logic', 'B1_7_logic', 'B1_8_logic',
'B1_9_logic', 'B1_10_logic', 'B1_11_logic', 'B1_12_logic', 'B1_13_logic', 'B3_logic', 'B6_logic', 'B7_logic', 'C0_1_logic', 'C0_2_logic',
'C0_3_logic', 'C0_4_logic', 'C0_5_logic', 'C0_6_logic', 'C1_m_logic', 'C5_1_logic', 'C5_2_logic', 'C5_3_logic', 'C5_4_logic', 'C5_5_logic', 
'C5_6_logic', 'D1_1_logic', 'D1_2_logic', 'D1_3_logic', 'D1_4_logic', 'D1_5_logic',
'D2_1_logic', 'D2_2_logic', 'D2_3_logic', 'D2_4_logic', 'D2_5_logic',
'D3_1_logic', 'D3_2_logic', 'D3_3_logic', 'D3_4_logic',
'D4_1_logic', 'D4_2_logic', 'D4_3_logic', 'D4_4_logic',
'D5_1_logic', 'D5_2_logic', 'D5_3_logic', 'D5_4_logic']

for var in agg_var:
    new_var = var + '_weighted_sums'
    aug_data[new_var] = aug_data[var] * aug_data['weight']


# In[ ]:


#  COUNTRY AGGREGATION
agg_aug_overall = aug_data.groupby(['date', 'country_agg', 'GID_0',
                                 'gender_overall', 'age_bucket_overall']).agg(
                                        {'intro1': 'count',
                                         'weight': 'sum',
                                         'cli': 'mean',
                                         'ili': 'mean',
                                         'B1_1_logic': 'mean',
                                         'B1_2_logic': 'mean',
                                         'B1_3_logic': 'mean',
                                         'B1_4_logic': 'mean',
                                         'B1_5_logic': 'mean',
                                         'B1_6_logic': 'mean',
                                         'B1_7_logic': 'mean',
                                         'B1_8_logic': 'mean',
                                         'B1_9_logic': 'mean',
                                         'B1_10_logic': 'mean',
                                         'B1_11_logic': 'mean',
                                         'B1_12_logic': 'mean',
                                         'B1_13_logic': 'mean',
                                         'B3_logic': 'mean',
                                         'B6_logic': 'mean',
                                         'B7_logic': 'mean',
                                         'C0_1_logic': 'mean',
                                         'C0_2_logic': 'mean',
                                         'C0_3_logic': 'mean',
                                         'C0_4_logic': 'mean',
                                         'C0_5_logic': 'mean',
                                         'C0_6_logic': 'mean',
                                         'C1_m_logic': 'mean',
                                         'C5_1_logic': 'mean',
                                         'C5_2_logic': 'mean',
                                         'C5_3_logic': 'mean',
                                         'C5_4_logic': 'mean',
                                         'C5_5_logic': 'mean',
                                         'C5_6_logic': 'mean',
                                         'D1_1_logic': 'mean',
                                         'D1_2_logic': 'mean',
                                         'D1_3_logic': 'mean',
                                         'D1_4_logic': 'mean',
                                         'D1_5_logic': 'mean',
                                         'D2_1_logic': 'mean',
                                         'D2_2_logic': 'mean',
                                         'D2_3_logic': 'mean',
                                         'D2_4_logic': 'mean',
                                         'D2_5_logic': 'mean',
                                         'D3_1_logic': 'mean',
                                         'D3_2_logic': 'mean',
                                         'D3_3_logic': 'mean',
                                         'D3_4_logic': 'mean',
                                         'D4_1_logic': 'mean',
                                         'D4_2_logic': 'mean',
                                         'D4_3_logic': 'mean',
                                         'D4_4_logic': 'mean',
                                         'D5_1_logic': 'mean',
                                         'D5_2_logic': 'mean',
                                         'D5_3_logic': 'mean',
                                         'D5_4_logic': 'mean',                              
                                         'cli_weighted_sums': 'sum',
                                         'ili_weighted_sums': 'sum',
                                         'B1_1_logic_weighted_sums': 'sum',
                                         'B1_2_logic_weighted_sums': 'sum',
                                         'B1_3_logic_weighted_sums': 'sum',
                                         'B1_4_logic_weighted_sums': 'sum',
                                         'B1_5_logic_weighted_sums': 'sum',
                                         'B1_6_logic_weighted_sums': 'sum',
                                         'B1_7_logic_weighted_sums': 'sum',
                                         'B1_8_logic_weighted_sums': 'sum',
                                         'B1_9_logic_weighted_sums': 'sum',
                                         'B1_10_logic_weighted_sums': 'sum',
                                         'B1_11_logic_weighted_sums': 'sum',
                                         'B1_12_logic_weighted_sums': 'sum',
                                         'B1_13_logic_weighted_sums': 'sum',
                                         'B3_logic_weighted_sums': 'sum',
                                         'B6_logic_weighted_sums': 'sum',
                                         'B7_logic_weighted_sums': 'sum',
                                         'C0_1_logic_weighted_sums': 'sum',
                                         'C0_2_logic_weighted_sums': 'sum',
                                         'C0_3_logic_weighted_sums': 'sum',
                                         'C0_4_logic_weighted_sums': 'sum',
                                         'C0_5_logic_weighted_sums': 'sum',
                                         'C0_6_logic_weighted_sums': 'sum',
                                         'C1_m_logic_weighted_sums': 'sum',
                                         'C5_1_logic_weighted_sums': 'sum',
                                         'C5_2_logic_weighted_sums': 'sum',
                                         'C5_3_logic_weighted_sums': 'sum',
                                         'C5_4_logic_weighted_sums': 'sum',
                                         'C5_5_logic_weighted_sums': 'sum',
                                         'C5_6_logic_weighted_sums': 'sum',
                                         'D1_1_logic_weighted_sums': 'sum',
                                         'D1_2_logic_weighted_sums': 'sum',
                                         'D1_3_logic_weighted_sums': 'sum',
                                         'D1_4_logic_weighted_sums': 'sum',
                                         'D1_5_logic_weighted_sums': 'sum',
                                         'D2_1_logic_weighted_sums': 'sum',
                                         'D2_2_logic_weighted_sums': 'sum',
                                         'D2_3_logic_weighted_sums': 'sum',
                                         'D2_4_logic_weighted_sums': 'sum',
                                         'D2_5_logic_weighted_sums': 'sum',
                                         'D3_1_logic_weighted_sums': 'sum',
                                         'D3_2_logic_weighted_sums': 'sum',
                                         'D3_3_logic_weighted_sums': 'sum',
                                         'D3_4_logic_weighted_sums': 'sum',
                                         'D4_1_logic_weighted_sums': 'sum',
                                         'D4_2_logic_weighted_sums': 'sum',
                                         'D4_3_logic_weighted_sums': 'sum',
                                         'D4_4_logic_weighted_sums': 'sum',
                                         'D5_1_logic_weighted_sums': 'sum',
                                         'D5_2_logic_weighted_sums': 'sum',
                                         'D5_3_logic_weighted_sums': 'sum',
                                         'D5_4_logic_weighted_sums': 'sum'
                                        }
)
agg_aug_overall = agg_aug_overall.rename(columns={'gender_overall': 'gender', 'age_bucket_overall': 'age_bucket'})


# In[ ]:


# COUNTRY, GENDER AGGREGATION
agg_aug_gender = aug_data.groupby(['date', 'country_agg','GID_0', 
                                 'gender', 'age_bucket_overall']).agg(
                                        {'intro1': 'count',
                                         'weight': 'sum',
                                         'cli': 'mean',
                                         'ili': 'mean',
                                         'B1_1_logic': 'mean',
                                         'B1_2_logic': 'mean',
                                         'B1_3_logic': 'mean',
                                         'B1_4_logic': 'mean',
                                         'B1_5_logic': 'mean',
                                         'B1_6_logic': 'mean',
                                         'B1_7_logic': 'mean',
                                         'B1_8_logic': 'mean',
                                         'B1_9_logic': 'mean',
                                         'B1_10_logic': 'mean',
                                         'B1_11_logic': 'mean',
                                         'B1_12_logic': 'mean',
                                         'B1_13_logic': 'mean',
                                         'B3_logic': 'mean',
                                         'B6_logic': 'mean',
                                         'B7_logic': 'mean',
                                         'C0_1_logic': 'mean',
                                         'C0_2_logic': 'mean',
                                         'C0_3_logic': 'mean',
                                         'C0_4_logic': 'mean',
                                         'C0_5_logic': 'mean',
                                         'C0_6_logic': 'mean',
                                         'C1_m_logic': 'mean',
                                         'C5_1_logic': 'mean',
                                         'C5_2_logic': 'mean',
                                         'C5_3_logic': 'mean',
                                         'C5_4_logic': 'mean',
                                         'C5_5_logic': 'mean',
                                         'C5_6_logic': 'mean', 
                                         'D1_1_logic': 'mean',
                                         'D1_2_logic': 'mean',
                                         'D1_3_logic': 'mean',
                                         'D1_4_logic': 'mean',
                                         'D1_5_logic': 'mean',
                                         'D2_1_logic': 'mean',
                                         'D2_2_logic': 'mean',
                                         'D2_3_logic': 'mean',
                                         'D2_4_logic': 'mean',
                                         'D2_5_logic': 'mean',
                                         'D3_1_logic': 'mean',
                                         'D3_2_logic': 'mean',
                                         'D3_3_logic': 'mean',
                                         'D3_4_logic': 'mean',
                                         'D4_1_logic': 'mean',
                                         'D4_2_logic': 'mean',
                                         'D4_3_logic': 'mean',
                                         'D4_4_logic': 'mean',
                                         'D5_1_logic': 'mean',
                                         'D5_2_logic': 'mean',
                                         'D5_3_logic': 'mean',
                                         'D5_4_logic': 'mean',
                                         'cli_weighted_sums': 'sum',
                                         'ili_weighted_sums': 'sum',
                                         'B1_1_logic_weighted_sums': 'sum',
                                         'B1_2_logic_weighted_sums': 'sum',
                                         'B1_3_logic_weighted_sums': 'sum',
                                         'B1_4_logic_weighted_sums': 'sum',
                                         'B1_5_logic_weighted_sums': 'sum',
                                         'B1_6_logic_weighted_sums': 'sum',
                                         'B1_7_logic_weighted_sums': 'sum',
                                         'B1_8_logic_weighted_sums': 'sum',
                                         'B1_9_logic_weighted_sums': 'sum',
                                         'B1_10_logic_weighted_sums': 'sum',
                                         'B1_11_logic_weighted_sums': 'sum',
                                         'B1_12_logic_weighted_sums': 'sum',
                                         'B1_13_logic_weighted_sums': 'sum',
                                         'B3_logic_weighted_sums': 'sum',
                                         'B6_logic_weighted_sums': 'sum',
                                         'B7_logic_weighted_sums': 'sum',
                                         'C0_1_logic_weighted_sums': 'sum',
                                         'C0_2_logic_weighted_sums': 'sum',
                                         'C0_3_logic_weighted_sums': 'sum',
                                         'C0_4_logic_weighted_sums': 'sum',
                                         'C0_5_logic_weighted_sums': 'sum',
                                         'C0_6_logic_weighted_sums': 'sum',
                                         'C1_m_logic_weighted_sums': 'sum',
                                         'C5_1_logic_weighted_sums': 'sum',
                                         'C5_2_logic_weighted_sums': 'sum',
                                         'C5_3_logic_weighted_sums': 'sum',
                                         'C5_4_logic_weighted_sums': 'sum',
                                         'C5_5_logic_weighted_sums': 'sum',
                                         'C5_6_logic_weighted_sums': 'sum',
                                         'D1_1_logic_weighted_sums': 'sum',
                                         'D1_2_logic_weighted_sums': 'sum',
                                         'D1_3_logic_weighted_sums': 'sum',
                                         'D1_4_logic_weighted_sums': 'sum',
                                         'D1_5_logic_weighted_sums': 'sum',
                                         'D2_1_logic_weighted_sums': 'sum',
                                         'D2_2_logic_weighted_sums': 'sum',
                                         'D2_3_logic_weighted_sums': 'sum',
                                         'D2_4_logic_weighted_sums': 'sum',
                                         'D2_5_logic_weighted_sums': 'sum',
                                         'D3_1_logic_weighted_sums': 'sum',
                                         'D3_2_logic_weighted_sums': 'sum',
                                         'D3_3_logic_weighted_sums': 'sum',
                                         'D3_4_logic_weighted_sums': 'sum',
                                         'D4_1_logic_weighted_sums': 'sum',
                                         'D4_2_logic_weighted_sums': 'sum',
                                         'D4_3_logic_weighted_sums': 'sum',
                                         'D4_4_logic_weighted_sums': 'sum',
                                         'D5_1_logic_weighted_sums': 'sum',
                                         'D5_2_logic_weighted_sums': 'sum',
                                         'D5_3_logic_weighted_sums': 'sum',
                                         'D5_4_logic_weighted_sums': 'sum'
                                        }
)
agg_aug_gender = agg_aug_gender.rename(columns={'age_bucket_overall': 'age_bucket'})


# In[ ]:


# COUNTRY, AGE AGGREGATION
agg_aug_age = aug_data.groupby(['date', 'country_agg', 'GID_0',
                                 'gender_overall', 'age_bucket']).agg(
                                        {'intro1': 'count',
                                         'weight': 'sum',
                                         'cli': 'mean',
                                         'ili': 'mean',
                                         'B1_1_logic': 'mean',
                                         'B1_2_logic': 'mean',
                                         'B1_3_logic': 'mean',
                                         'B1_4_logic': 'mean',
                                         'B1_5_logic': 'mean',
                                         'B1_6_logic': 'mean',
                                         'B1_7_logic': 'mean',
                                         'B1_8_logic': 'mean',
                                         'B1_9_logic': 'mean',
                                         'B1_10_logic': 'mean',
                                         'B1_11_logic': 'mean',
                                         'B1_12_logic': 'mean',
                                         'B1_13_logic': 'mean',
                                         'B3_logic': 'mean',
                                         'B6_logic': 'mean',
                                         'B7_logic': 'mean',
                                         'C0_1_logic': 'mean',
                                         'C0_2_logic': 'mean',
                                         'C0_3_logic': 'mean',
                                         'C0_4_logic': 'mean',
                                         'C0_5_logic': 'mean',
                                         'C0_6_logic': 'mean',
                                         'C1_m_logic': 'mean',
                                         'C5_1_logic': 'mean',
                                         'C5_2_logic': 'mean',
                                         'C5_3_logic': 'mean',
                                         'C5_4_logic': 'mean',
                                         'C5_5_logic': 'mean',
                                         'C5_6_logic': 'mean',
                                         'D1_1_logic': 'mean',
                                         'D1_2_logic': 'mean',
                                         'D1_3_logic': 'mean',
                                         'D1_4_logic': 'mean',
                                         'D1_5_logic': 'mean',
                                         'D2_1_logic': 'mean',
                                         'D2_2_logic': 'mean',
                                         'D2_3_logic': 'mean',
                                         'D2_4_logic': 'mean',
                                         'D2_5_logic': 'mean',
                                         'D3_1_logic': 'mean',
                                         'D3_2_logic': 'mean',
                                         'D3_3_logic': 'mean',
                                         'D3_4_logic': 'mean',
                                         'D4_1_logic': 'mean',
                                         'D4_2_logic': 'mean',
                                         'D4_3_logic': 'mean',
                                         'D4_4_logic': 'mean',
                                         'D5_1_logic': 'mean',
                                         'D5_2_logic': 'mean',
                                         'D5_3_logic': 'mean',
                                         'D5_4_logic': 'mean',
                                         'cli_weighted_sums': 'sum',
                                         'ili_weighted_sums': 'sum',
                                         'B1_1_logic_weighted_sums': 'sum',
                                         'B1_2_logic_weighted_sums': 'sum',
                                         'B1_3_logic_weighted_sums': 'sum',
                                         'B1_4_logic_weighted_sums': 'sum',
                                         'B1_5_logic_weighted_sums': 'sum',
                                         'B1_6_logic_weighted_sums': 'sum',
                                         'B1_7_logic_weighted_sums': 'sum',
                                         'B1_8_logic_weighted_sums': 'sum',
                                         'B1_9_logic_weighted_sums': 'sum',
                                         'B1_10_logic_weighted_sums': 'sum',
                                         'B1_11_logic_weighted_sums': 'sum',
                                         'B1_12_logic_weighted_sums': 'sum',
                                         'B1_13_logic_weighted_sums': 'sum',
                                         'B3_logic_weighted_sums': 'sum',
                                         'B6_logic_weighted_sums': 'sum',
                                         'B7_logic_weighted_sums': 'sum',
                                         'C0_1_logic_weighted_sums': 'sum',
                                         'C0_2_logic_weighted_sums': 'sum',
                                         'C0_3_logic_weighted_sums': 'sum',
                                         'C0_4_logic_weighted_sums': 'sum',
                                         'C0_5_logic_weighted_sums': 'sum',
                                         'C0_6_logic_weighted_sums': 'sum',
                                         'C1_m_logic_weighted_sums': 'sum',
                                         'C5_1_logic_weighted_sums': 'sum',
                                         'C5_2_logic_weighted_sums': 'sum',
                                         'C5_3_logic_weighted_sums': 'sum',
                                         'C5_4_logic_weighted_sums': 'sum',
                                         'C5_5_logic_weighted_sums': 'sum',
                                         'C5_6_logic_weighted_sums': 'sum',
                                         'D1_1_logic_weighted_sums': 'sum',
                                         'D1_2_logic_weighted_sums': 'sum',
                                         'D1_3_logic_weighted_sums': 'sum',
                                         'D1_4_logic_weighted_sums': 'sum',
                                         'D1_5_logic_weighted_sums': 'sum',
                                         'D2_1_logic_weighted_sums': 'sum',
                                         'D2_2_logic_weighted_sums': 'sum',
                                         'D2_3_logic_weighted_sums': 'sum',
                                         'D2_4_logic_weighted_sums': 'sum',
                                         'D2_5_logic_weighted_sums': 'sum',
                                         'D3_1_logic_weighted_sums': 'sum',
                                         'D3_2_logic_weighted_sums': 'sum',
                                         'D3_3_logic_weighted_sums': 'sum',
                                         'D3_4_logic_weighted_sums': 'sum',
                                         'D4_1_logic_weighted_sums': 'sum',
                                         'D4_2_logic_weighted_sums': 'sum',
                                         'D4_3_logic_weighted_sums': 'sum',
                                         'D4_4_logic_weighted_sums': 'sum',
                                         'D5_1_logic_weighted_sums': 'sum',
                                         'D5_2_logic_weighted_sums': 'sum',
                                         'D5_3_logic_weighted_sums': 'sum',
                                         'D5_4_logic_weighted_sums': 'sum'
                                        }
)
agg_aug_age = agg_aug_age.rename(columns={'gender_overall': 'gender'})


# In[ ]:


# COUNTRY, AGE, GENDER AGGREGATION
agg_aug_age_gender = aug_data.groupby(['date', 'country_agg', 'GID_0',
                                 'gender', 'age_bucket']).agg(
                                        {'intro1': 'count',
                                         'weight': 'sum',
                                         'cli': 'mean',
                                         'ili': 'mean',
                                         'B1_1_logic': 'mean',
                                         'B1_2_logic': 'mean',
                                         'B1_3_logic': 'mean',
                                         'B1_4_logic': 'mean',
                                         'B1_5_logic': 'mean',
                                         'B1_6_logic': 'mean',
                                         'B1_7_logic': 'mean',
                                         'B1_8_logic': 'mean',
                                         'B1_9_logic': 'mean',
                                         'B1_10_logic': 'mean',
                                         'B1_11_logic': 'mean',
                                         'B1_12_logic': 'mean',
                                         'B1_13_logic': 'mean',
                                         'B3_logic': 'mean',
                                         'B6_logic': 'mean',
                                         'B7_logic': 'mean',
                                         'C0_1_logic': 'mean',
                                         'C0_2_logic': 'mean',
                                         'C0_3_logic': 'mean',
                                         'C0_4_logic': 'mean',
                                         'C0_5_logic': 'mean',
                                         'C0_6_logic': 'mean',
                                         'C1_m_logic': 'mean',
                                         'C5_1_logic': 'mean',
                                         'C5_2_logic': 'mean',
                                         'C5_3_logic': 'mean',
                                         'C5_4_logic': 'mean',
                                         'C5_5_logic': 'mean',
                                         'C5_6_logic': 'mean', 
                                         'D1_1_logic': 'mean',
                                         'D1_2_logic': 'mean',
                                         'D1_3_logic': 'mean',
                                         'D1_4_logic': 'mean',
                                         'D1_5_logic': 'mean',
                                         'D2_1_logic': 'mean',
                                         'D2_2_logic': 'mean',
                                         'D2_3_logic': 'mean',
                                         'D2_4_logic': 'mean',
                                         'D2_5_logic': 'mean',
                                         'D3_1_logic': 'mean',
                                         'D3_2_logic': 'mean',
                                         'D3_3_logic': 'mean',
                                         'D3_4_logic': 'mean',
                                         'D4_1_logic': 'mean',
                                         'D4_2_logic': 'mean',
                                         'D4_3_logic': 'mean',
                                         'D4_4_logic': 'mean',
                                         'D5_1_logic': 'mean',
                                         'D5_2_logic': 'mean',
                                         'D5_3_logic': 'mean',
                                         'D5_4_logic': 'mean',
                                         'cli_weighted_sums': 'sum',
                                         'ili_weighted_sums': 'sum',
                                         'B1_1_logic_weighted_sums': 'sum',
                                         'B1_2_logic_weighted_sums': 'sum',
                                         'B1_3_logic_weighted_sums': 'sum',
                                         'B1_4_logic_weighted_sums': 'sum',
                                         'B1_5_logic_weighted_sums': 'sum',
                                         'B1_6_logic_weighted_sums': 'sum',
                                         'B1_7_logic_weighted_sums': 'sum',
                                         'B1_8_logic_weighted_sums': 'sum',
                                         'B1_9_logic_weighted_sums': 'sum',
                                         'B1_10_logic_weighted_sums': 'sum',
                                         'B1_11_logic_weighted_sums': 'sum',
                                         'B1_12_logic_weighted_sums': 'sum',
                                         'B1_13_logic_weighted_sums': 'sum',
                                         'B3_logic_weighted_sums': 'sum',
                                         'B6_logic_weighted_sums': 'sum',
                                         'B7_logic_weighted_sums': 'sum',
                                         'C0_1_logic_weighted_sums': 'sum',
                                         'C0_2_logic_weighted_sums': 'sum',
                                         'C0_3_logic_weighted_sums': 'sum',
                                         'C0_4_logic_weighted_sums': 'sum',
                                         'C0_5_logic_weighted_sums': 'sum',
                                         'C0_6_logic_weighted_sums': 'sum',
                                         'C1_m_logic_weighted_sums': 'sum',
                                         'C5_1_logic_weighted_sums': 'sum',
                                         'C5_2_logic_weighted_sums': 'sum',
                                         'C5_3_logic_weighted_sums': 'sum',
                                         'C5_4_logic_weighted_sums': 'sum',
                                         'C5_5_logic_weighted_sums': 'sum',
                                         'C5_6_logic_weighted_sums': 'sum',
                                         'D1_1_logic_weighted_sums': 'sum',
                                         'D1_2_logic_weighted_sums': 'sum',
                                         'D1_3_logic_weighted_sums': 'sum',
                                         'D1_4_logic_weighted_sums': 'sum',
                                         'D1_5_logic_weighted_sums': 'sum',
                                         'D2_1_logic_weighted_sums': 'sum',
                                         'D2_2_logic_weighted_sums': 'sum',
                                         'D2_3_logic_weighted_sums': 'sum',
                                         'D2_4_logic_weighted_sums': 'sum',
                                         'D2_5_logic_weighted_sums': 'sum',
                                         'D3_1_logic_weighted_sums': 'sum',
                                         'D3_2_logic_weighted_sums': 'sum',
                                         'D3_3_logic_weighted_sums': 'sum',
                                         'D3_4_logic_weighted_sums': 'sum',
                                         'D4_1_logic_weighted_sums': 'sum',
                                         'D4_2_logic_weighted_sums': 'sum',
                                         'D4_3_logic_weighted_sums': 'sum',
                                         'D4_4_logic_weighted_sums': 'sum',
                                         'D5_1_logic_weighted_sums': 'sum',
                                         'D5_2_logic_weighted_sums': 'sum',
                                         'D5_3_logic_weighted_sums': 'sum',
                                         'D5_4_logic_weighted_sums': 'sum'
                                        }
)


# In[ ]:


# Union all the aggregations together

all_aug_unioned = pd.concat([agg_aug_age_gender, agg_aug_overall, agg_aug_gender, agg_aug_age])


# In[ ]:


# Check the number of rows we should have
check1 = len(agg_aug_overall) + len(agg_aug_gender) + len(agg_aug_age) + len(agg_aug_age_gender)
print(check1)


# In[ ]:


# Validate that the unioned dataset has all of those rows
len(all_aug_unioned)


# In[ ]:


# Convert all of the weighted_sums to actual weighted %s by dividing by the overall sum of weights

for var in agg_var:
    weighted_sums_var = var + '_weighted_sums'
    true_var = var + '_weighted'
    all_aug_unioned[true_var] = all_aug_unioned[weighted_sums_var]/all_aug_unioned['weight']


# In[ ]:


# Creating a dictionary to rename variables into a more human readable form

rename_dict = {
    'country_agg': 'country',
    'region_agg': 'region',
    'country_region_numeric': 'numeric_region',
    'intro1': 'total_responses',
    'weight': 'weight_sums', 
    'cli': 'pct_cli',
    'ili': 'pct_ili',
    'B1_1_logic': 'pct_fever', 
    'B1_2_logic': 'pct_cough',
    'B1_3_logic': 'pct_difficulty_breathing',
    'B1_4_logic': 'pct_fatigue',
    'B1_5_logic': 'pct_stuffy_runny_nose',
    'B1_6_logic': 'pct_aches_muscle_pain',
    'B1_7_logic': 'pct_sore_throat',
    'B1_8_logic': 'pct_chest_pain',
    'B1_9_logic': 'pct_nausea',
    'B1_10_logic': 'pct_anosmia_ageusia',
    'B1_11_logic': 'pct_eye_pain',
    'B1_12_logic': 'pct_headache',
    'B1_13_logic': 'pct_chills',
    'B3_logic': 'pct_cmnty_sick',
    'B6_logic': 'pct_ever_tested',
    'B7_logic': 'pct_tested_recently',
    'C0_1_logic': 'pct_worked_outside_home',
    'C0_2_logic': 'pct_grocery_outside_home',
    'C0_3_logic': 'pct_ate_outside_home',
    'C0_4_logic': 'pct_spent_time_with_non_hh',
    'C0_5_logic': 'pct_attended_public_event',
    'C0_6_logic': 'pct_used_public_transit',
    'C1_m_logic': 'pct_direct_contact_with_non_hh',
    'C5_1_logic': 'pct__all_time',
    'C5_2_logic': 'pct_wear_mask_most_time',
    'C5_3_logic': 'pct_wear_mask_half_time',
    'C5_4_logic': 'pct_wear_mask_some_time',
    'C5_5_logic': 'pct_wear_mask_none_time',
    'C5_6_logic': 'pct_no_public',
    'D1_1_logic': 'pct_feel_nervous_all_time',
    'D1_2_logic': 'pct_feel_nervous_most_time',
    'D1_3_logic': 'pct_feel_nervous_some_time',
    'D1_4_logic': 'pct_feel_nervous_little_time',
    'D1_5_logic': 'pct_feel_nervous_none_time',
    'D2_1_logic': 'pct_feel_depressed_all_time',
    'D2_2_logic': 'pct_feel_depressed_most_time',
    'D2_3_logic': 'pct_feel_depressed_some_time',
    'D2_4_logic': 'pct_feel_depressed_little_time',
    'D2_5_logic': 'pct_feel_depressed_none_time',
    'D3_1_logic': 'pct_worried_ill_covid19_very',
    'D3_2_logic': 'pct_worried_ill_covid19_somewhat',
    'D3_3_logic': 'pct_worried_ill_covid19_notTooWorried',
    'D3_4_logic': 'pct_worried_ill_covid19_notWorried',
    'D4_1_logic': 'pct_enough_toEat_very_worried',
    'D4_2_logic': 'pct_enough_toEat_somewhat_worried',
    'D4_3_logic': 'pct_enough_toEat_notToo_worried',
    'D4_4_logic': 'pct_enough_toEat_not_worried',
    'D5_1_logic': 'pct_finances_very_worried',
    'D5_2_logic': 'pct_finances_somewhat_worried',
    'D5_3_logic': 'pct_finances_notToo_worried',
    'D5_4_logic': 'pct_finances_not_worried',
}


# In[ ]:


# The for loop to rename each column in the dataset 

for col in list(all_aug_unioned):
    base_name = col.replace('_weighted', '').replace('_sums', '')
    if base_name in rename_dict.keys():
        new_name = rename_dict[base_name] + col.split(base_name)[1]
        all_aug_unioned = all_aug_unioned.rename(columns={col: new_name})


# In[ ]:


# Resetting and setting the index to make sure I know exactly how it looks
all_aug_unioned_indexed = all_aug_unioned.reset_index().set_index(['country_agg', 'GID_0', 'gender', 'age_bucket', 'date'])


# In[ ]:


# Sort the data so that we can do rolling sums
all_aug_unioned_indexed = all_aug_unioned_indexed.sort_index(level =['country_agg', 'GID_0', 'gender', 'age_bucket', 'date'], ascending = [True, True, True, True, True])

all_aug_unioned_indexed


# In[ ]:


###

# NOTE: THIS ESSENTIALLY CONCLUDES ALL OF THE WORK NECESSARY TO GENERATE THE RAW/WEIGHTED SIGNALS AT THE COUNTRY LEVEL. 
# A simple function to write the pandas table to CSV would produce the right estimated aggregates (hopefully). 
# Below is how we'll do smoothed calculations

###


# In[ ]:


# Creating rolling total responses, which we will reference a ton later

all_aug_unioned_smoothed = all_aug_unioned_indexed
all_aug_unioned_smoothed['rolling_total_responses'] = all_aug_unioned_smoothed['total_responses'].groupby(level=[0, 1, 2, 3], group_keys=False).rolling(7).sum()


# In[ ]:


# Iterate through all of the variables that we've calculated above and calculate the rolling averages

for col in list(all_aug_unioned_smoothed):
    if 'pct_' in col and 'smoothed' not in col:
        smoothed_var = 'smoothed_' + col
        ct_var = 'count_' + col.replace('pct_', '')
        all_aug_unioned_smoothed[ct_var] = all_aug_unioned_smoothed[col]*all_aug_unioned_smoothed['total_responses'] 
        all_aug_unioned_smoothed['rolling_' + ct_var] = all_aug_unioned_smoothed[ct_var].groupby(level=[0, 1, 2, 3], group_keys=False).rolling(7).sum()
        all_aug_unioned_smoothed[smoothed_var] = all_aug_unioned_smoothed['rolling_' + ct_var]/all_aug_unioned_smoothed['rolling_total_responses']

        


# In[ ]:


base_variables = ['rolling_total_responses', 'weight_sums']
smoothed_colnames_stg = list(all_aug_unioned_smoothed)
smoothed_colnames = base_variables + [s for s in smoothed_colnames_stg if "smoothed_" in s and "sums" not in s]
all_aug_unioned_smoothed_filterd = all_aug_unioned_smoothed[smoothed_colnames]


## all_aug_unioned_smoothed_filtered can be written out as a CSV


# In[ ]:


############################################################
### RECALCULATE EVERYTHING ON THE REGION LEVEL
############################################################
############################################################


# In[ ]:


#  REGION AGGREGATION
agg_region_aug_overall = aug_data.groupby(['date', 'country_agg', 'GID_0', 'region_agg', 'GID_1', 'country_region_numeric',
                                 'gender_overall', 'age_bucket_overall']).agg(
                                        {'intro1': 'count',
                                         'weight': 'sum',
                                         'cli': 'mean',
                                         'ili': 'mean',
                                         'B1_1_logic': 'mean',
                                         'B1_2_logic': 'mean',
                                         'B1_3_logic': 'mean',
                                         'B1_4_logic': 'mean',
                                         'B1_5_logic': 'mean',
                                         'B1_6_logic': 'mean',
                                         'B1_7_logic': 'mean',
                                         'B1_8_logic': 'mean',
                                         'B1_9_logic': 'mean',
                                         'B1_10_logic': 'mean',
                                         'B1_11_logic': 'mean',
                                         'B1_12_logic': 'mean',
                                         'B1_13_logic': 'mean',
                                         'B3_logic': 'mean',
                                         'B6_logic': 'mean',
                                         'B7_logic': 'mean',
                                         'C0_1_logic': 'mean',
                                         'C0_2_logic': 'mean',
                                         'C0_3_logic': 'mean',
                                         'C0_4_logic': 'mean',
                                         'C0_5_logic': 'mean',
                                         'C0_6_logic': 'mean',
                                         'C1_m_logic': 'mean',
                                         'C5_1_logic': 'mean',
                                         'C5_2_logic': 'mean',
                                         'C5_3_logic': 'mean',
                                         'C5_4_logic': 'mean',
                                         'C5_5_logic': 'mean',
                                         'C5_6_logic': 'mean', 
                                         'D1_1_logic': 'mean',
                                         'D1_2_logic': 'mean',
                                         'D1_3_logic': 'mean',
                                         'D1_4_logic': 'mean',
                                         'D1_5_logic': 'mean',
                                         'D2_1_logic': 'mean',
                                         'D2_2_logic': 'mean',
                                         'D2_3_logic': 'mean',
                                         'D2_4_logic': 'mean',
                                         'D2_5_logic': 'mean',
                                         'D3_1_logic': 'mean',
                                         'D3_2_logic': 'mean',
                                         'D3_3_logic': 'mean',
                                         'D3_4_logic': 'mean',
                                         'D4_1_logic': 'mean',
                                         'D4_2_logic': 'mean',
                                         'D4_3_logic': 'mean',
                                         'D4_4_logic': 'mean',
                                         'D5_1_logic': 'mean',
                                         'D5_2_logic': 'mean',
                                         'D5_3_logic': 'mean',
                                         'D5_4_logic': 'mean',
                                         'cli_weighted_sums': 'sum',
                                         'ili_weighted_sums': 'sum',
                                         'B1_1_logic_weighted_sums': 'sum',
                                         'B1_2_logic_weighted_sums': 'sum',
                                         'B1_3_logic_weighted_sums': 'sum',
                                         'B1_4_logic_weighted_sums': 'sum',
                                         'B1_5_logic_weighted_sums': 'sum',
                                         'B1_6_logic_weighted_sums': 'sum',
                                         'B1_7_logic_weighted_sums': 'sum',
                                         'B1_8_logic_weighted_sums': 'sum',
                                         'B1_9_logic_weighted_sums': 'sum',
                                         'B1_10_logic_weighted_sums': 'sum',
                                         'B1_11_logic_weighted_sums': 'sum',
                                         'B1_12_logic_weighted_sums': 'sum',
                                         'B1_13_logic_weighted_sums': 'sum',
                                         'B3_logic_weighted_sums': 'sum',
                                         'B6_logic_weighted_sums': 'sum',
                                         'B7_logic_weighted_sums': 'sum',
                                         'C0_1_logic_weighted_sums': 'sum',
                                         'C0_2_logic_weighted_sums': 'sum',
                                         'C0_3_logic_weighted_sums': 'sum',
                                         'C0_4_logic_weighted_sums': 'sum',
                                         'C0_5_logic_weighted_sums': 'sum',
                                         'C0_6_logic_weighted_sums': 'sum',
                                         'C1_m_logic_weighted_sums': 'sum',
                                         'C5_1_logic_weighted_sums': 'sum',
                                         'C5_2_logic_weighted_sums': 'sum',
                                         'C5_3_logic_weighted_sums': 'sum',
                                         'C5_4_logic_weighted_sums': 'sum',
                                         'C5_5_logic_weighted_sums': 'sum',
                                         'C5_6_logic_weighted_sums': 'sum',
                                         'D1_1_logic_weighted_sums': 'sum',
                                         'D1_2_logic_weighted_sums': 'sum',
                                         'D1_3_logic_weighted_sums': 'sum',
                                         'D1_4_logic_weighted_sums': 'sum',
                                         'D1_5_logic_weighted_sums': 'sum',
                                         'D2_1_logic_weighted_sums': 'sum',
                                         'D2_2_logic_weighted_sums': 'sum',
                                         'D2_3_logic_weighted_sums': 'sum',
                                         'D2_4_logic_weighted_sums': 'sum',
                                         'D2_5_logic_weighted_sums': 'sum',
                                         'D3_1_logic_weighted_sums': 'sum',
                                         'D3_2_logic_weighted_sums': 'sum',
                                         'D3_3_logic_weighted_sums': 'sum',
                                         'D3_4_logic_weighted_sums': 'sum',
                                         'D4_1_logic_weighted_sums': 'sum',
                                         'D4_2_logic_weighted_sums': 'sum',
                                         'D4_3_logic_weighted_sums': 'sum',
                                         'D4_4_logic_weighted_sums': 'sum',
                                         'D5_1_logic_weighted_sums': 'sum',
                                         'D5_2_logic_weighted_sums': 'sum',
                                         'D5_3_logic_weighted_sums': 'sum',
                                         'D5_4_logic_weighted_sums': 'sum'
                                        }
)
agg_aug_overall = agg_aug_overall.rename(columns={'gender_overall': 'gender', 'age_bucket_overall': 'age_bucket'})


# In[ ]:


# REGION, GENDER AGGREGATION
agg_region_aug_gender = aug_data.groupby(['date', 'country_agg','GID_0', 'region_agg', 'GID_1', 'country_region_numeric', 
                                 'gender', 'age_bucket_overall']).agg(
                                        {'intro1': 'count',
                                         'weight': 'sum',
                                         'cli': 'mean',
                                         'ili': 'mean',
                                         'B1_1_logic': 'mean',
                                         'B1_2_logic': 'mean',
                                         'B1_3_logic': 'mean',
                                         'B1_4_logic': 'mean',
                                         'B1_5_logic': 'mean',
                                         'B1_6_logic': 'mean',
                                         'B1_7_logic': 'mean',
                                         'B1_8_logic': 'mean',
                                         'B1_9_logic': 'mean',
                                         'B1_10_logic': 'mean',
                                         'B1_11_logic': 'mean',
                                         'B1_12_logic': 'mean',
                                         'B1_13_logic': 'mean',
                                         'B3_logic': 'mean',
                                         'B6_logic': 'mean',
                                         'B7_logic': 'mean',
                                         'C0_1_logic': 'mean',
                                         'C0_2_logic': 'mean',
                                         'C0_3_logic': 'mean',
                                         'C0_4_logic': 'mean',
                                         'C0_5_logic': 'mean',
                                         'C0_6_logic': 'mean',
                                         'C1_m_logic': 'mean',
                                         'C5_1_logic': 'mean',
                                         'C5_2_logic': 'mean',
                                         'C5_3_logic': 'mean',
                                         'C5_4_logic': 'mean',
                                         'C5_5_logic': 'mean',
                                         'C5_6_logic': 'mean',
                                         'D1_1_logic': 'mean',
                                         'D1_2_logic': 'mean',
                                         'D1_3_logic': 'mean',
                                         'D1_4_logic': 'mean',
                                         'D1_5_logic': 'mean',
                                         'D2_1_logic': 'mean',
                                         'D2_2_logic': 'mean',
                                         'D2_3_logic': 'mean',
                                         'D2_4_logic': 'mean',
                                         'D2_5_logic': 'mean',
                                         'D3_1_logic': 'mean',
                                         'D3_2_logic': 'mean',
                                         'D3_3_logic': 'mean',
                                         'D3_4_logic': 'mean',
                                         'D4_1_logic': 'mean',
                                         'D4_2_logic': 'mean',
                                         'D4_3_logic': 'mean',
                                         'D4_4_logic': 'mean',
                                         'D5_1_logic': 'mean',
                                         'D5_2_logic': 'mean',
                                         'D5_3_logic': 'mean',
                                         'D5_4_logic': 'mean',
                                         'cli_weighted_sums': 'sum',
                                         'ili_weighted_sums': 'sum',
                                         'B1_1_logic_weighted_sums': 'sum',
                                         'B1_2_logic_weighted_sums': 'sum',
                                         'B1_3_logic_weighted_sums': 'sum',
                                         'B1_4_logic_weighted_sums': 'sum',
                                         'B1_5_logic_weighted_sums': 'sum',
                                         'B1_6_logic_weighted_sums': 'sum',
                                         'B1_7_logic_weighted_sums': 'sum',
                                         'B1_8_logic_weighted_sums': 'sum',
                                         'B1_9_logic_weighted_sums': 'sum',
                                         'B1_10_logic_weighted_sums': 'sum',
                                         'B1_11_logic_weighted_sums': 'sum',
                                         'B1_12_logic_weighted_sums': 'sum',
                                         'B1_13_logic_weighted_sums': 'sum',
                                         'B3_logic_weighted_sums': 'sum',
                                         'B6_logic_weighted_sums': 'sum',
                                         'B7_logic_weighted_sums': 'sum',
                                         'C0_1_logic_weighted_sums': 'sum',
                                         'C0_2_logic_weighted_sums': 'sum',
                                         'C0_3_logic_weighted_sums': 'sum',
                                         'C0_4_logic_weighted_sums': 'sum',
                                         'C0_5_logic_weighted_sums': 'sum',
                                         'C0_6_logic_weighted_sums': 'sum',
                                         'C1_m_logic_weighted_sums': 'sum',
                                         'C5_1_logic_weighted_sums': 'sum',
                                         'C5_2_logic_weighted_sums': 'sum',
                                         'C5_3_logic_weighted_sums': 'sum',
                                         'C5_4_logic_weighted_sums': 'sum',
                                         'C5_5_logic_weighted_sums': 'sum',
                                         'C5_6_logic_weighted_sums': 'sum',
                                         'D1_1_logic_weighted_sums': 'sum',
                                         'D1_2_logic_weighted_sums': 'sum',
                                         'D1_3_logic_weighted_sums': 'sum',
                                         'D1_4_logic_weighted_sums': 'sum',
                                         'D1_5_logic_weighted_sums': 'sum',
                                         'D2_1_logic_weighted_sums': 'sum',
                                         'D2_2_logic_weighted_sums': 'sum',
                                         'D2_3_logic_weighted_sums': 'sum',
                                         'D2_4_logic_weighted_sums': 'sum',
                                         'D2_5_logic_weighted_sums': 'sum',
                                         'D3_1_logic_weighted_sums': 'sum',
                                         'D3_2_logic_weighted_sums': 'sum',
                                         'D3_3_logic_weighted_sums': 'sum',
                                         'D3_4_logic_weighted_sums': 'sum',
                                         'D4_1_logic_weighted_sums': 'sum',
                                         'D4_2_logic_weighted_sums': 'sum',
                                         'D4_3_logic_weighted_sums': 'sum',
                                         'D4_4_logic_weighted_sums': 'sum',
                                         'D5_1_logic_weighted_sums': 'sum',
                                         'D5_2_logic_weighted_sums': 'sum',
                                         'D5_3_logic_weighted_sums': 'sum',
                                         'D5_4_logic_weighted_sums': 'sum'
                                        }
)
agg_aug_gender = agg_aug_gender.rename(columns={'age_bucket_overall': 'age_bucket'})


# In[ ]:


# COUNTRY, AGE AGGREGATION
agg_region_aug_age = aug_data.groupby(['date', 'country_agg', 'GID_0', 'region_agg', 'GID_1', 'country_region_numeric', 
                                 'gender_overall', 'age_bucket']).agg(
                                        {'intro1': 'count',
                                         'weight': 'sum',
                                         'cli': 'mean',
                                         'ili': 'mean',
                                         'B1_1_logic': 'mean',
                                         'B1_2_logic': 'mean',
                                         'B1_3_logic': 'mean',
                                         'B1_4_logic': 'mean',
                                         'B1_5_logic': 'mean',
                                         'B1_6_logic': 'mean',
                                         'B1_7_logic': 'mean',
                                         'B1_8_logic': 'mean',
                                         'B1_9_logic': 'mean',
                                         'B1_10_logic': 'mean',
                                         'B1_11_logic': 'mean',
                                         'B1_12_logic': 'mean',
                                         'B1_13_logic': 'mean',
                                         'B3_logic': 'mean',
                                         'B6_logic': 'mean',
                                         'B7_logic': 'mean',
                                         'C0_1_logic': 'mean',
                                         'C0_2_logic': 'mean',
                                         'C0_3_logic': 'mean',
                                         'C0_4_logic': 'mean',
                                         'C0_5_logic': 'mean',
                                         'C0_6_logic': 'mean',
                                         'C1_m_logic': 'mean',
                                         'C5_1_logic': 'mean',
                                         'C5_2_logic': 'mean',
                                         'C5_3_logic': 'mean',
                                         'C5_4_logic': 'mean',
                                         'C5_5_logic': 'mean',
                                         'C5_6_logic': 'mean',
                                         'D1_1_logic': 'mean',
                                         'D1_2_logic': 'mean',
                                         'D1_3_logic': 'mean',
                                         'D1_4_logic': 'mean',
                                         'D1_5_logic': 'mean',
                                         'D2_1_logic': 'mean',
                                         'D2_2_logic': 'mean',
                                         'D2_3_logic': 'mean',
                                         'D2_4_logic': 'mean',
                                         'D2_5_logic': 'mean',
                                         'D3_1_logic': 'mean',
                                         'D3_2_logic': 'mean',
                                         'D3_3_logic': 'mean',
                                         'D3_4_logic': 'mean',
                                         'D4_1_logic': 'mean',
                                         'D4_2_logic': 'mean',
                                         'D4_3_logic': 'mean',
                                         'D4_4_logic': 'mean',
                                         'D5_1_logic': 'mean',
                                         'D5_2_logic': 'mean',
                                         'D5_3_logic': 'mean',
                                         'D5_4_logic': 'mean',
                                         'cli_weighted_sums': 'sum',
                                         'ili_weighted_sums': 'sum',
                                         'B1_1_logic_weighted_sums': 'sum',
                                         'B1_2_logic_weighted_sums': 'sum',
                                         'B1_3_logic_weighted_sums': 'sum',
                                         'B1_4_logic_weighted_sums': 'sum',
                                         'B1_5_logic_weighted_sums': 'sum',
                                         'B1_6_logic_weighted_sums': 'sum',
                                         'B1_7_logic_weighted_sums': 'sum',
                                         'B1_8_logic_weighted_sums': 'sum',
                                         'B1_9_logic_weighted_sums': 'sum',
                                         'B1_10_logic_weighted_sums': 'sum',
                                         'B1_11_logic_weighted_sums': 'sum',
                                         'B1_12_logic_weighted_sums': 'sum',
                                         'B1_13_logic_weighted_sums': 'sum',
                                         'B3_logic_weighted_sums': 'sum',
                                         'B6_logic_weighted_sums': 'sum',
                                         'B7_logic_weighted_sums': 'sum',
                                         'C0_1_logic_weighted_sums': 'sum',
                                         'C0_2_logic_weighted_sums': 'sum',
                                         'C0_3_logic_weighted_sums': 'sum',
                                         'C0_4_logic_weighted_sums': 'sum',
                                         'C0_5_logic_weighted_sums': 'sum',
                                         'C0_6_logic_weighted_sums': 'sum',
                                         'C1_m_logic_weighted_sums': 'sum',
                                         'C5_1_logic_weighted_sums': 'sum',
                                         'C5_2_logic_weighted_sums': 'sum',
                                         'C5_3_logic_weighted_sums': 'sum',
                                         'C5_4_logic_weighted_sums': 'sum',
                                         'C5_5_logic_weighted_sums': 'sum',
                                         'C5_6_logic_weighted_sums': 'sum',
                                         'D1_1_logic_weighted_sums': 'sum',
                                         'D1_2_logic_weighted_sums': 'sum',
                                         'D1_3_logic_weighted_sums': 'sum',
                                         'D1_4_logic_weighted_sums': 'sum',
                                         'D1_5_logic_weighted_sums': 'sum',
                                         'D2_1_logic_weighted_sums': 'sum',
                                         'D2_2_logic_weighted_sums': 'sum',
                                         'D2_3_logic_weighted_sums': 'sum',
                                         'D2_4_logic_weighted_sums': 'sum',
                                         'D2_5_logic_weighted_sums': 'sum',
                                         'D3_1_logic_weighted_sums': 'sum',
                                         'D3_2_logic_weighted_sums': 'sum',
                                         'D3_3_logic_weighted_sums': 'sum',
                                         'D3_4_logic_weighted_sums': 'sum',
                                         'D4_1_logic_weighted_sums': 'sum',
                                         'D4_2_logic_weighted_sums': 'sum',
                                         'D4_3_logic_weighted_sums': 'sum',
                                         'D4_4_logic_weighted_sums': 'sum',
                                         'D5_1_logic_weighted_sums': 'sum',
                                         'D5_2_logic_weighted_sums': 'sum',
                                         'D5_3_logic_weighted_sums': 'sum',
                                         'D5_4_logic_weighted_sums': 'sum'
                                        }
)
agg_aug_age = agg_aug_age.rename(columns={'gender_overall': 'gender'})


# In[ ]:


# COUNTRY, AGE, GENDER AGGREGATION
agg_region_aug_age_gender = aug_data.groupby(['date', 'country_agg', 'GID_0', 'region_agg', 'GID_1', 'country_region_numeric', 
                                 'gender', 'age_bucket']).agg(
                                        {'intro1': 'count',
                                         'weight': 'sum',
                                         'cli': 'mean',
                                         'ili': 'mean',
                                         'B1_1_logic': 'mean',
                                         'B1_2_logic': 'mean',
                                         'B1_3_logic': 'mean',
                                         'B1_4_logic': 'mean',
                                         'B1_5_logic': 'mean',
                                         'B1_6_logic': 'mean',
                                         'B1_7_logic': 'mean',
                                         'B1_8_logic': 'mean',
                                         'B1_9_logic': 'mean',
                                         'B1_10_logic': 'mean',
                                         'B1_11_logic': 'mean',
                                         'B1_12_logic': 'mean',
                                         'B1_13_logic': 'mean',
                                         'B3_logic': 'mean',
                                         'B6_logic': 'mean',
                                         'B7_logic': 'mean',
                                         'C0_1_logic': 'mean',
                                         'C0_2_logic': 'mean',
                                         'C0_3_logic': 'mean',
                                         'C0_4_logic': 'mean',
                                         'C0_5_logic': 'mean',
                                         'C0_6_logic': 'mean',
                                         'C1_m_logic': 'mean',
                                         'C5_1_logic': 'mean',
                                         'C5_2_logic': 'mean',
                                         'C5_3_logic': 'mean',
                                         'C5_4_logic': 'mean',
                                         'C5_5_logic': 'mean',
                                         'C5_6_logic': 'mean',
                                         'D1_1_logic': 'mean',
                                         'D1_2_logic': 'mean',
                                         'D1_3_logic': 'mean',
                                         'D1_4_logic': 'mean',
                                         'D1_5_logic': 'mean',
                                         'D2_1_logic': 'mean',
                                         'D2_2_logic': 'mean',
                                         'D2_3_logic': 'mean',
                                         'D2_4_logic': 'mean',
                                         'D2_5_logic': 'mean',
                                         'D3_1_logic': 'mean',
                                         'D3_2_logic': 'mean',
                                         'D3_3_logic': 'mean',
                                         'D3_4_logic': 'mean',
                                         'D4_1_logic': 'mean',
                                         'D4_2_logic': 'mean',
                                         'D4_3_logic': 'mean',
                                         'D4_4_logic': 'mean',
                                         'D5_1_logic': 'mean',
                                         'D5_2_logic': 'mean',
                                         'D5_3_logic': 'mean',
                                         'D5_4_logic': 'mean',
                                         'cli_weighted_sums': 'sum',
                                         'ili_weighted_sums': 'sum',
                                         'B1_1_logic_weighted_sums': 'sum',
                                         'B1_2_logic_weighted_sums': 'sum',
                                         'B1_3_logic_weighted_sums': 'sum',
                                         'B1_4_logic_weighted_sums': 'sum',
                                         'B1_5_logic_weighted_sums': 'sum',
                                         'B1_6_logic_weighted_sums': 'sum',
                                         'B1_7_logic_weighted_sums': 'sum',
                                         'B1_8_logic_weighted_sums': 'sum',
                                         'B1_9_logic_weighted_sums': 'sum',
                                         'B1_10_logic_weighted_sums': 'sum',
                                         'B1_11_logic_weighted_sums': 'sum',
                                         'B1_12_logic_weighted_sums': 'sum',
                                         'B1_13_logic_weighted_sums': 'sum',
                                         'B3_logic_weighted_sums': 'sum',
                                         'B6_logic_weighted_sums': 'sum',
                                         'B7_logic_weighted_sums': 'sum',
                                         'C0_1_logic_weighted_sums': 'sum',
                                         'C0_2_logic_weighted_sums': 'sum',
                                         'C0_3_logic_weighted_sums': 'sum',
                                         'C0_4_logic_weighted_sums': 'sum',
                                         'C0_5_logic_weighted_sums': 'sum',
                                         'C0_6_logic_weighted_sums': 'sum',
                                         'C1_m_logic_weighted_sums': 'sum',
                                         'C5_1_logic_weighted_sums': 'sum',
                                         'C5_2_logic_weighted_sums': 'sum',
                                         'C5_3_logic_weighted_sums': 'sum',
                                         'C5_4_logic_weighted_sums': 'sum',
                                         'C5_5_logic_weighted_sums': 'sum',
                                         'C5_6_logic_weighted_sums': 'sum',
                                         'D1_1_logic_weighted_sums': 'sum',
                                         'D1_2_logic_weighted_sums': 'sum',
                                         'D1_3_logic_weighted_sums': 'sum',
                                         'D1_4_logic_weighted_sums': 'sum',
                                         'D1_5_logic_weighted_sums': 'sum',
                                         'D2_1_logic_weighted_sums': 'sum',
                                         'D2_2_logic_weighted_sums': 'sum',
                                         'D2_3_logic_weighted_sums': 'sum',
                                         'D2_4_logic_weighted_sums': 'sum',
                                         'D2_5_logic_weighted_sums': 'sum',
                                         'D3_1_logic_weighted_sums': 'sum',
                                         'D3_2_logic_weighted_sums': 'sum',
                                         'D3_3_logic_weighted_sums': 'sum',
                                         'D3_4_logic_weighted_sums': 'sum',
                                         'D4_1_logic_weighted_sums': 'sum',
                                         'D4_2_logic_weighted_sums': 'sum',
                                         'D4_3_logic_weighted_sums': 'sum',
                                         'D4_4_logic_weighted_sums': 'sum',
                                         'D5_1_logic_weighted_sums': 'sum',
                                         'D5_2_logic_weighted_sums': 'sum',
                                         'D5_3_logic_weighted_sums': 'sum',
                                         'D5_4_logic_weighted_sums': 'sum'
                                        }
)


# In[ ]:


# Union all the aggregations together

all_region_aug_unioned = pd.concat([agg_region_aug_age_gender, agg_region_aug_overall, agg_region_aug_gender, agg_region_aug_age])


# In[ ]:


len(all_region_aug_unioned)


# In[ ]:


len(agg_region_aug_age_gender) + len(agg_region_aug_overall) + len(agg_region_aug_gender) + len(agg_region_aug_age)


# In[ ]:


# Convert all of the weighted_sums to actual weighted %s by dividing by the overall sum of weights
for var in agg_var:
    weighted_sums_var = var + '_weighted_sums'
    true_var = var + '_weighted'
    all_region_aug_unioned[true_var] = all_region_aug_unioned[weighted_sums_var]/all_region_aug_unioned['weight']


# In[ ]:


# The for loop to rename each column in the dataset 

for col in list(all_region_aug_unioned):
    base_name = col.replace('_weighted', '').replace('_sums', '')
    if base_name in rename_dict.keys():
        new_name = rename_dict[base_name] + col.split(base_name)[1]
        all_region_aug_unioned = all_region_aug_unioned.rename(columns={col: new_name})


# In[ ]:


all_region_aug_unioned


# In[ ]:


# Resetting and setting the index to make sure I know exactly how it looks
all_region_aug_unioned_indexed = all_region_aug_unioned.reset_index().set_index(['country_agg', 'GID_0', 'region_agg', 'GID_1', 'country_region_numeric', 'gender', 'age_bucket', 'date'])


# In[ ]:


# Sort the data so that we can do rolling sums
all_region_aug_unioned_indexed = all_region_aug_unioned_indexed.sort_index(level =['country_agg', 'GID_0', 'region_agg', 'GID_1', 'country_region_numeric', 'gender', 'age_bucket', 'date'], ascending = [True, True, True, True, True, True, True, True])

all_region_aug_unioned_indexed


# In[ ]:


###

# NOTE: THIS ESSENTIALLY CONCLUDES ALL OF THE WORK NECESSARY TO GENERATE THE RAW/WEIGHTED SIGNALS AT THE REGION LEVEL. 
# A simple function to write the pandas table to CSV would produce the right estimated aggregates (hopefully). 
# Below is how we'll do smoothed calculations

###


# In[ ]:


# Creating rolling total responses, which we will reference a ton later

all_region_aug_unioned_smoothed = all_region_aug_unioned_indexed
all_region_aug_unioned_smoothed['rolling_total_responses'] = all_region_aug_unioned_smoothed['total_responses'].groupby(level=[0, 1, 2, 3, 4, 5, 6], group_keys=False).rolling(7).sum()


# In[ ]:


# Iterate through all of the variables that we've calculated above and calculate the rolling averages

for col in list(all_region_aug_unioned_smoothed):
    if 'pct_' in col and 'smoothed' not in col:
        smoothed_var = 'smoothed_' + col
        ct_var = 'count_' + col.replace('pct_', '')
        all_region_aug_unioned_smoothed[ct_var] = all_region_aug_unioned_smoothed[col]*all_region_aug_unioned_smoothed['total_responses'] 
        all_region_aug_unioned_smoothed['rolling_' + ct_var] = all_region_aug_unioned_smoothed[ct_var].groupby(level=[0, 1, 2, 3, 4, 5, 6], group_keys=False).rolling(7).sum()
        all_region_aug_unioned_smoothed[smoothed_var] = all_region_aug_unioned_smoothed['rolling_' + ct_var]/all_region_aug_unioned_smoothed['rolling_total_responses']

        


# In[ ]:


base_variables = ['rolling_total_responses', 'weight_sums']
smoothed_region_colnames_stg = list(all_region_aug_unioned_smoothed)
smoothed_region_colnames = base_variables + [s for s in smoothed_region_colnames_stg if "smoothed_" in s and "sums" not in s]
all_region_aug_unioned_smoothed_filterd = all_region_aug_unioned_smoothed[smoothed_region_colnames]


## all_region_aug_unioned_smoothed_filtered can be written out as a CSV


# In[ ]:


all_region_aug_unioned_smoothed_filterd

