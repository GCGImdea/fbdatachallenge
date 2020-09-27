##
# Additional changes for  Aggregates
# Multiply aggregates by 100. .03 becomes 3 meaning 3%
# Roundr values to 2 decimal places 
# Filter out 'None' in 'age' and 'gender' columns
# Min sample size is 50
##

import pandas as pd

df = pd.read_csv('PATH_TO_FILE.csv')

# Use 7 for Country Aggregates
# Use 10 for Regional Aggregates
df.iloc[:, 7:] = df.iloc[:, 7:].multiply(100)
df = df.round(2)
df = df[(df.gender != 'None') & (df.age_bucket != 'None')]

# For smoothed use rolling_total_reponses
df = df[df.total_responses >= 50]

df.to_csv('PATH_TO_FILE.csv')