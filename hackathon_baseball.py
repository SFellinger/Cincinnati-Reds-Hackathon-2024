# -*- coding: utf-8 -*-
"""hackathon_baseball.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1WnPAF7_B0naerHXJMaJSkHaOLq7NO6kr
"""

#import BaseBallSavant table pt1

from google.colab import files

uploaded = files.upload()

#import BaseBallSavant table pt2

import pandas as pd
file_path = next(iter(uploaded))

pitch_data = pd.read_csv(file_path)
print(pitch_data.head())

#group by + average pitch_data
pitch_data_groups = pitch_data.groupby(["player_name","game_date","pitcher_at_bat_number", "pitch_type"])["effective_speed",
 "pfx_x", "pfx_z", "release_spin_rate"].mean().reset_index()

#import dropoff table pt2

from google.colab import files

uploaded = files.upload()

#import dropoff table pt2

import pandas as pd
file_path = next(iter(uploaded))
new_drop = pd.read_csv(file_path)
print(new_drop.head())

# Extract the file path from the uploaded dictionary

file_path = next(iter(uploaded))
new_list = pd.read_csv(file_path)
print(new_list.head())

#pivot dropoff table

ptype = ['FF', 'SL', 'CU', 'SI', 'CH', 'FS', 'KC', 'FC', 'SV', 'ST', 'CS', 'SC']
dummy = pd.DataFrame()
for pt in ptype:
  drop_filter = new_drop[new_drop['pitch_type'] == pt ]
  drop_pivot_filter = drop_filter.pivot_table(index=['player_name', 'game_date', 'role_key'], columns='pitcher_at_bat_number', values='dropoff', aggfunc='first').reset_index()
  drop_pivot_filter['pitch_type'] = pt
  dummy = pd.concat([dummy, drop_pivot_filter], ignore_index=True, sort=False)
print(dummy.head())

#cleaning dummy pt1

cols = list(dummy.columns)
print(cols)
cols.remove('pitch_type')
cols.insert(3, 'pitch_type')

dummy = dummy[cols]

print(dummy.head())

#cleaing dummy pt2

columns_to_drop = ['1', 'pitcher_at_bat_number']

for col in columns_to_drop:
    if col in dummy.columns:
        dummy.drop(col, axis=1, inplace=True)
    else:
        print(f"Column '{col}' not found in DataFrame.")
print(dummy.head())

#upload drops

from google.colab import drive
#drive.mount('/content/drive')
# Save the CSV file to Google Drive
dummy.to_csv("/content/drive/My Drive/drop_dummy_4.csv")

#finding average atbat1 number

from google.colab import files
import pandas as pd
uploaded = files.upload()
file_path = next(iter(uploaded))
pitch_data = pd.read_csv(file_path)
print(pitch_data.head())

#filter pitch_data by atbat 1

pitch_data = pitch_data[pitch_data["pitcher_at_bat_number"] == 1]
print(pitch_data)

#filter pitch_data by atbat 1 pt2

pitch_data_firsts = pitch_data.groupby(["pitch_type", "role_key"])["effective_speed",
 "pfx_x", "pfx_z", "release_spin_rate"].mean().reset_index()
print(pitch_data_firsts.head())

#finding average atbat1 number pt2

#ptype = ['FF', 'SL', 'CU', 'SI', 'CH', 'FS', 'KC', 'FC', 'SV', 'ST', 'CS', 'SC']

def select_data(row):
    if row['pitch_type'] == 'FC' or row['pitch_type'] == 'SV' or row['pitch_type'] == 'ST':
        return row['release_spin_rate']
    elif row['pitch_type'] == 'CU' or row['pitch_type'] == 'KC' or row['pitch_type'] == 'SC':
        return row['pfx_z']
    elif row['pitch_type'] == 'SL' or row['pitch_type'] == 'CS' or row['pitch_type'] == 'FS':
        return row['pfx_x']
    elif row['pitch_type'] == 'FF' or row['pitch_type'] == 'SI' or row['pitch_type'] == 'CH':
        return row['effective_speed']
    else:
        return None  # Handle other pitch types if needed

# Apply the custom function to create a new 'selected_data' column
pitch_data_firsts['selected'] = pitch_data_firsts.apply(select_data, axis=1)
print(pitch_data_firsts)

#getting average role comparisons for first 4 columns
from google.colab import files
import pandas as pd

uploaded = files.upload()
file_path = next(iter(uploaded))
result = pd.read_csv(file_path)
print(result.head())

#get euclidian distance for 2-4 atbats

import numpy as np
result["dist"] = np.sqrt(result[['Result_Value_2nd_at_Bat', 'Result_Value_3rd_at_Bat', 'Result_Value_4th_at_Bat']].apply(lambda x: x.dropna().pow(2).sum(), axis=1))
print(result.head())

#split ID column to appriopriate titles
result[["player_name", "pitch_type", "role_key"]] = result['ID'].str.rsplit('-', expand=True, n=2)
print(result.head())

#filter results to just have dist and keys
result = result[["player_name", "pitch_type", "role_key", "dist"]]
print(result.head())

#split dist results into starters and relievers, remove the NAN=0 scores

result_SP = result[result["role_key"] == " SP"]
result_RP = result[result["role_key"] == " RP"]

#result_SP = result_SP.drop("role_key", axis=1)
#result_RP = result_RP.drop("role_key", axis=1)

result_SP =  result_SP[ result_SP["dist"] != 0]
result_RP = result_RP[result_RP["dist"] != 0]

print(result_SP.head())
print(result_RP.head())

#checking to make sure ranks, sort, groups, and roles worked properly

result_SP['score'] = result_SP.groupby('pitch_type')['dist'].rank()
print(result_SP[result_SP['score'] < 3].sort_values(by='pitch_type'))

result_RP['score'] = result_RP.groupby('pitch_type')['dist'].rank()
print(result_RP[result_RP['score'] < 3].sort_values(by='pitch_type'))

#take the average of the scores, produce top candidates for a role swithc for both roles.

final_SP = result_SP.groupby('player_name')['score'].mean().reset_index()
final_RP = result_RP.groupby('player_name')['score'].mean().reset_index()

print(final_SP.sort_values(by='score').iloc[0:20])
print("\n")
print(final_RP.sort_values(by='score').iloc[0:40])