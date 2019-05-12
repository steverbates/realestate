import pandas as pd
import numpy as np
import os
from datetime import datetime


data_categories = ['Median List Price ($)','Median List Price Per Sq Ft ($)','Median Sale Price - Seasonally Adjusted ($)','Monthly For-Sale Inventory (Raw)','Monthly For-Sale Inventory (Smooth, Seasonally Adjusted)','Monthly Home Sales (Number, Raw)','Monthly Home Sales (Number, Seasonally Adjusted)','New Monthly For-Sale Inventory (Raw)','New Monthly For-Sale Inventory (Smooth, Seasonally Adjusted)','Sale-to-list ratio']

states = ["Alabama","Alaska","Arkansas","Arizona","California","Colorado","Connecticut","Delaware","District of Columbia","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"]


populations = [4779736,710231,6392017,2915918,37253956,5029196,3574097,897934,601723,18801310,9687653,1360301,1567582,12830632,6483802,3046355,2853118,4339367,4533372,1328361,5773552,6547629,9883640,5303925,2967297,5988927,989415,1826341,2700551,1316470,8791894,2059179,19378102,9535483,672591,11536504,3751351,3831074,12702379,1052567,4625364,814180,6346105,25145561,2763885,625741,8001024,6724540,1852994,5686986,563626]

population_lookup = dict(zip(states,populations))

def month_convert(string):
	s = str(string)
	if s[4] == '-' and (s[:4]).isnumeric and (s[5:]).isnumeric:
		return datetime(int(s[:4]),int(s[5:]),1)
	else:
		return s

for category in data_categories:
	path = 'Zillow data/'+category
	for filename in os.listdir(path):
		if category in ['Median List Price ($)','Median List Price Per Sq Ft ($)']:
			geography = filename.split(sep='_')[0] #different filename parsing rules
		else:
			geography = (filename.split(sep='_')[-1]).split(sep='.')[0]
		if geography == 'State': #selecting out only state-level data for now
   			df=pd.read_csv(path+'/'+filename,encoding='iso-8859-1',index_col='RegionName',parse_dates=True) #index values will be one of the states (or DC)
   			df.rename(mapper=month_convert,axis=1,inplace=True)
   			for col in df.columns:
   				if type(col) is not datetime:
   					df.drop(labels=col,axis=1,inplace=True) #drop all non-index columns that aren't part of time series data
   			for state in states:
   				if state not in df.index:
   					df = pd.concat([df,pd.DataFrame(np.reshape([float('nan') for i in range(len(df.columns))],(1,len(df.columns))),index=[state],columns=df.columns)],axis=0) #fill in missing data
   			df = df.transpose() #better for R processing to have each column be a time series, for a given state;
   			df.insert(0,'DataCategory',pd.Series([category for i in range(len(df.index))],index=df.index))
   			df.set_index('DataCategory',append=True,inplace=True) #index now two-level; data category, and series of time (month) values
   			try:
   				state_frame = state_frame.append(df)
   			except NameError:
   				state_frame = df


#now need to create and append derivative dataframes for certain categorys
cat_remap = {'Monthly For-Sale Inventory (Raw)':'Monthly For-Sale Inventory (Normalized)','Monthly For-Sale Inventory (Smooth, Seasonally Adjusted)':'Monthly For-Sale Inventory (Normalized) - Seasonally Adjusted','New Monthly For-Sale Inventory (Raw)':'New Monthly For-Sale Inventory (Normalized)','New Monthly For-Sale Inventory (Smooth, Seasonally Adjusted)':'New Monthly For-Sale Inventory (Normalized) - Seasonally Adjusted','Monthly Home Sales (Number, Raw)':'Monthly Home Sales (Normalized)','Monthly Home Sales (Number, Seasonally Adjusted)':'Monthly Home Sales (Normalized) - Seasonally Adjusted'}
#Dictionary mapping source data columns for data columns  to be derived

cat_norm = {'Monthly Home Sales (Number, Raw)':'Monthly For-Sale Inventory (Raw)','Monthly Home Sales (Number, Seasonally Adjusted)':'Monthly For-Sale Inventory (Smooth, Seasonally Adjusted)','New Monthly For-Sale Inventory (Raw)':'Monthly For-Sale Inventory (Raw)','New Monthly For-Sale Inventory (Smooth, Seasonally Adjusted)':'Monthly For-Sale Inventory (Smooth, Seasonally Adjusted)'}
#Dictionary mapping data columns to columns by which they should be normalized

def normalize(data_frame,category):
	data_frame = data_frame.loc[(slice(None),category),:].reset_index(level='DataCategory',drop=True)
	normalized = data_frame.apply(lambda x: x/population_lookup[x.name])
	normalized['DataCategory']=pd.Series([cat_remap[category] for i in range(len(normalized.index))],index=normalized.index)
	return normalized.set_index('DataCategory',append=True)
#for columns that are to be normalized by state populations

def normalize2(data_frame,category1,category2):
	data_frame_1 = data_frame.loc[(slice(None),category1),:].reset_index(level='DataCategory',drop=True)
	data_frame_2 = data_frame.loc[(slice(None),category2),:].reset_index(level='DataCategory',drop=True)
	normalized = data_frame_1.div(data_frame_2,level=0)
	normalized['DataCategory']=pd.Series([cat_remap[category] for i in range(len(normalized.index))],index=normalized.index)
	return normalized.set_index('DataCategory',append=True)
#for columns that are to be normalized by other columns

for category in ['Monthly For-Sale Inventory (Raw)','Monthly For-Sale Inventory (Smooth, Seasonally Adjusted)']:
	state_frame = state_frame.append(normalize(state_frame,category))
#actual execution of normalization by state populations

for category in ['Monthly Home Sales (Number, Raw)','Monthly Home Sales (Number, Seasonally Adjusted)','New Monthly For-Sale Inventory (Raw)','New Monthly For-Sale Inventory (Smooth, Seasonally Adjusted)']:
	state_frame = state_frame.append(normalize2(state_frame,category,cat_norm[category]))
#actual execution of normalization by other columns

state_frame.index.rename(['time','DataCategory'],inplace=True)
state_frame.reorder_levels(['DataCategory','time'])
state_frame.rename(lambda x: x.replace(' ','_'),axis=1,inplace=True) #R doesn't like spaces in column names
state_frame.to_csv('Zillow data/All_Data_State.csv')
