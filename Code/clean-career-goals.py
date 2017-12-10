# Libraries
import pandas as pd

# Full data file
data = pd.read_csv('lsay.csv')

# Select columns
high_school_cols = ['CASENUM', 'GENDER', 'COHORT', 'ASTEMMX1', 'BSTEMMX1', 'CSTEMMX1', 'DSTEMMX1', 'ESTEMMX1',
                    'FSTEMMX1', 'GSTEMMX1', 'HSTEMMX1', 'ISTEMMX1', 'JSTEMMX1',
                    'KSTEMMX1', 'LSTEMMX1']
#df = data[high_school_cols].dropna()
df = data[high_school_cols]
df = df.reset_index()

# Set column names
set_cols = ['id', 'cohort', 'gender', 'F7', 'S7', 'F8', 'S8', 'F9', 'S9', 'F10', 'S10', 'F11', 'S11', 'F12', 'S12']

df.columns = set_cols
set_cols.remove('id')
set_cols.remove('gender')
set_cols.remove('cohort')

# Long format
df = df.melt(id_vars=['id', 'gender', 'cohort'], value_vars=set_cols)
df['value'] = df['value'].astype('category')

# Add STEMM flag
careers = pd.read_csv('careers.csv')
df = df.merge(careers, how='left', left_on='value', right_on='career')
df = df.drop(['Unnamed: 0', 'career'], axis=1)
df.columns = ['id', 'gender', 'cohort', 'yr', 'career', 'stem', 'medicine','social science']

# Only Fall responses
df = df[df['yr'].isin(['F7', 'F8', 'F9', 'F10', 'F11', 'F12'])]

# Set time interval
#dict = {'F7':1, 'S7':2, 'F8':3, 'S8':4, 'F9':5, 'S9':6, 'F10':7, 'S10':8, 'F11':9, 'S11':10, 'F12':11, 'S12':12}
dict = {'F7':0, 'F8':1, 'F9':2, 'F10':3, 'F11':4, 'F12':5}
df['time'] = df['yr'].map(dict)


# Save csv
df.to_csv('stemm_career_goal.csv')
