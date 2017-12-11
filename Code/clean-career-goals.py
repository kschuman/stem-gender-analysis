# Libraries
import pandas as pd

# Full data file
data = pd.read_csv('lsay.csv')

# Select columns
high_school_cols = ['CASENUM', 'GENDER', 'COHORT', 'STRATA', 'WEIGHT7', 'WEIGHT8', 'WEIGHT9', 'WEIGHT10',
                    'WEIGHT11', 'WEIGHT12', 'ASTEMMX1', 'BSTEMMX1', 'CSTEMMX1', 'DSTEMMX1', 'ESTEMMX1',
                    'FSTEMMX1', 'GSTEMMX1', 'HSTEMMX1', 'ISTEMMX1', 'JSTEMMX1',
                    'KSTEMMX1', 'LSTEMMX1']
#df = data[high_school_cols].dropna()
df = data[high_school_cols]
#df = df.reset_index()



# Set column names
set_cols = ['id', 'cohort', 'gender', 'strata', 'weight7', 'weight8', 'weight9', 'weight10', 'weight11', 'weight12', 'F7', 'S7', 'F8', 'S8', 'F9', 'S9', 'F10', 'S10', 'F11', 'S11', 'F12', 'S12']

df.columns = set_cols
set_cols.remove('id')
set_cols.remove('cohort')
set_cols.remove('gender')
set_cols.remove('strata')
set_cols.remove('weight7')
set_cols.remove('weight8')
set_cols.remove('weight9')
set_cols.remove('weight10')
set_cols.remove('weight11')
set_cols.remove('weight12')



# Long format
weight_vars = ['weight7', 'weight8', 'weight9', 'weight10', 'weight11', 'weight12']
df_weights = df.melt(id_vars=['id', 'gender', 'cohort', 'strata'], value_vars= weight_vars)
df = df.melt(id_vars=['id', 'gender', 'cohort', 'strata'], value_vars=set_cols)

df['value'] = df['value'].astype('category')

# Add STEMM flag
careers = pd.read_csv('careers.csv')
df = df.merge(careers, how='left', left_on='value', right_on='career')
df = df.drop(['Unnamed: 0', 'career'], axis=1)
df.columns = ['id', 'gender', 'cohort', 'strata', 'yr', 'career', 'stem', 'medicine','social science']

# Only Fall responses
df = df[df['yr'].isin(['F7', 'F8', 'F9', 'F10', 'F11', 'F12'])]

# Set time interval
#dict = {'F7':1, 'S7':2, 'F8':3, 'S8':4, 'F9':5, 'S9':6, 'F10':7, 'S10':8, 'F11':9, 'S11':10, 'F12':11, 'S12':12}
dict = {'F7':0, 'F8':1, 'F9':2, 'F10':3, 'F11':4, 'F12':5}
weight_dict = {'weight7':0, 'weight8':1, 'weight9':2, 'weight10':3, 'weight11':4, 'weight12':5}
df['time'] = df['yr'].map(dict)
df_weights['time'] = df_weights['variable'].map(weight_dict)

df = df.merge(df_weights, how='left', on=['id', 'time', 'gender', 'cohort', 'strata'])

df = df.drop(['variable'], axis=1)
df.columns = ['CASENUM', 'COHORT', 'GENDER', 'STRATA', 'yr', 'career', 'stem', 'medicine',
       'social science', 'time', 'weight']


# Save csv
df.to_csv('stemm_career_goal.csv')
