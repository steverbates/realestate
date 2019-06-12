import pandas as pd
import numpy as np
import os
from datetime import datetime


data_categories = ['Median List Price ($)','Median List Price Per Sq Ft ($)','Median Sale Price - Seasonally Adjusted ($)','Monthly For-Sale Inventory (Raw)','Monthly For-Sale Inventory (Smooth, Seasonally Adjusted)','Monthly Home Sales (Number, Raw)','Monthly Home Sales (Number, Seasonally Adjusted)','New Monthly For-Sale Inventory (Raw)','New Monthly For-Sale Inventory (Smooth, Seasonally Adjusted)','Sale-to-list ratio']

states = ['Alabama','Alaska','Arkansas','Arizona','California','Colorado','Connecticut','Delaware','District of Columbia','Florida','Georgia','Hawaii','Idaho','Illinois','Indiana','Iowa','Kansas','Kentucky','Louisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota','Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey','New Mexico','New York','North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island','South Carolina','South Dakota','Tennessee','Texas','Utah','Vermont','Virginia','Washington','West Virginia','Wisconsin','Wyoming']

abbrevs = ['AL','AK','AR','AZ','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY']

abbrev_lookup = dict(zip(states,abbrevs))




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
		if geography == 'County': #this time, county level data
   			df=pd.read_csv(path+'/'+filename,encoding='iso-8859-1',index_col='RegionName',parse_dates=True) #index values will be county names
   			if category in ['Median List Price ($)','Median List Price Per Sq Ft ($)']: #this time it's an irregularity in column naming that needs to be dealt with
   				df.rename(mapper={'State':'StateName'},axis=1,inplace=True)
   			df = df[df['StateName'].map(lambda x: x not in ['PR','VI'])] #filtering out rows outside of focus on 50 states and DC
   			df.set_index('StateName',append=True,inplace=True) #need to retain this information for duplicate county names in different states (Name is actually standard abbreviation)
   			df.rename(mapper=month_convert,axis=1,inplace=True)
   			df.rename(mapper=abbrev_lookup,level=1,inplace=True) #State names format also not consistent between full names and abbreviations.
   			for col in df.columns:
   				if type(col) is not datetime:
   					df.drop(labels=col,axis=1,inplace=True) #drop all non-index columns that aren't part of time series data
   			df = df.transpose() #better for R processing to have each column be a time series, for a given county;
   			df.insert(0,'DataCategory',pd.Series([category for i in range(len(df.index))],index=df.index))
   			df.set_index('DataCategory',append=True,inplace=True) #index now two-level; data category, and series of time (month) values
   			try:
   				county_frame = county_frame.append(df)
   			except NameError:
   				county_frame = df




def alaska(string): #smoothing out irregularities particular to Alaska Borough name formatting
	phrase = string.split()
	if phrase[-1] == 'Municipality':
		phrase[-1] = 'Borough'
	if phrase[-3:] == ['City','and','Borough']:
		del phrase[-3]
		del phrase[-2]
	if phrase[-2:] == ['Census','Area']:
		del phrase[-2]
		phrase[-1] = 'Borough'
	return ' '.join(phrase)

def proper_noun(string):
	exceptions = ['and','the','of','du']
	for sep in [' ','-']:
		phrase = string.split(sep=sep)
		phrase = ['Saint'if s == 'St.' else 'Sainte' if s == 'Ste.' else s for s in phrase]
		if phrase[0][:2] == 'Mc':
			phrase[0] = phrase[0][:2] + phrase[0][2].upper() + phrase[0][3:]
		string = sep.join([s[0].upper() + s[1:] if s not in exceptions else s for s in phrase]) #Can't use capitlize method as would be ideal; since it has side effect of removing internal capitalization and undoing work of previous loops
	return alaska(string)

standardize = {'Valdez Cordova Borough':'Valdez-Cordova Borough','Matanuska Susitna Borough':'Matanuska-Susitna Borough','Yukon Koyukuk Borough':'Yukon-Koyukuk Borough','Queen Annes County':'Queen Anne\'s County','Saint Marys County':'Saint Mary\'s County','Prince Georges County':'Prince George\'s County','Dona Ana County':'Do\xF1a Ana County','O Brien County':'O\'Brien County','De Kalb County':'DeKalb County','Dekalb County':'DeKalb County','Lagrange County':'LaGrange County','De Soto County':'DeSoto County','De Witt County':'DeWitt County','Dewitt County':'DeWitt County','La Porte County':'LaPorte County','LaSalle County':'La Salle County','Lamoure County':'LaMoure County','LeFlore County':'Le Flore County'} #dictionary of remaining needed formatting changes that can't be simply grouped with rules.  #Technically LaSalle County, IL and La Salle County Texas should be formatted differently, but I'm leaving aside working through that complication for the moment.
population_lookup = pd.read_csv('Zillow data/co-est2018-alldata.csv',usecols=['CTYNAME','STNAME','CENSUS2010POP'],encoding='latin-1',parse_dates=True).rename(mapper={'CTYNAME':'RegionName','STNAME':'StateName','CENSUS2010POP':'pop'},axis=1).set_index(['RegionName','StateName']).rename(mapper=abbrev_lookup,level=1).rename(mapper=proper_noun,level=0).rename(mapper=standardize,level=0) #getting data here for normalization by county population in 2010 (analogous to that used by states).  Encoding choice needed to avoid raising error: 'utf-8' codec can't decode byte 0xf1 in position 2: invalid continuation byte
#last rename method call necessary because capitalization of full names not standardized
county_frame.rename(mapper=standardize,level=0,axis=1,inplace=True) #some of name format standardization needs to be done on county_frame side


'''
orphans=[]
for i,x in enumerate(county_frame.columns):
	try:
		a = population_lookup.loc[x,:]
	except:
		orphans.append(x)

print(orphans)

This commented out code was used for exploratory analysis to determine how county name formats needed to be standardized between the data frames.
'''


#now need to create and append derivative dataframes for certain categories
cat_remap = {'Monthly For-Sale Inventory (Raw)':'Monthly For-Sale Inventory (Normalized)','Monthly For-Sale Inventory (Smooth, Seasonally Adjusted)':'Monthly For-Sale Inventory (Normalized) - Seasonally Adjusted','New Monthly For-Sale Inventory (Raw)':'New Monthly For-Sale Inventory (Normalized)','New Monthly For-Sale Inventory (Smooth, Seasonally Adjusted)':'New Monthly For-Sale Inventory (Normalized) - Seasonally Adjusted','Monthly Home Sales (Number, Raw)':'Monthly Home Sales (Normalized)','Monthly Home Sales (Number, Seasonally Adjusted)':'Monthly Home Sales (Normalized) - Seasonally Adjusted'}
#Dictionary mapping source data columns for data columns  to be derived

cat_norm = {'Monthly Home Sales (Number, Raw)':'Monthly For-Sale Inventory (Raw)','Monthly Home Sales (Number, Seasonally Adjusted)':'Monthly For-Sale Inventory (Smooth, Seasonally Adjusted)','New Monthly For-Sale Inventory (Raw)':'Monthly For-Sale Inventory (Raw)','New Monthly For-Sale Inventory (Smooth, Seasonally Adjusted)':'Monthly For-Sale Inventory (Smooth, Seasonally Adjusted)'}
#Dictionary mapping data columns to columns by which they should be normalized

def normalize(data_frame,category):
	data_frame = data_frame.loc[(slice(None),category),:].reset_index(level='DataCategory',drop=True)
	normalized = data_frame.apply(lambda x: x/(population_lookup.loc[x.name,:].values[0][0]))
	normalized['DataCategory']=pd.Series([cat_remap[category] for i in range(len(normalized.index))],index=normalized.index)
	return normalized.set_index('DataCategory',append=True)
#for columns that are to be normalized by 2010 county populations

def normalize2(data_frame,category1,category2):
	data_frame_1 = data_frame.loc[(slice(None),category1),:].reset_index(level='DataCategory',drop=True)
	data_frame_2 = data_frame.loc[(slice(None),category2),:].reset_index(level='DataCategory',drop=True)
	normalized = data_frame_1.div(data_frame_2,level=0)
	normalized['DataCategory']=pd.Series([cat_remap[category] for i in range(len(normalized.index))],index=normalized.index)
	return normalized.set_index('DataCategory',append=True)
#for columns that are to be normalized by other columns

for category in ['Monthly For-Sale Inventory (Raw)','Monthly For-Sale Inventory (Smooth, Seasonally Adjusted)']:
	county_frame = county_frame.append(normalize(county_frame,category))
#actual execution of normalization by county populations

for category in ['Monthly Home Sales (Number, Raw)','Monthly Home Sales (Number, Seasonally Adjusted)','New Monthly For-Sale Inventory (Raw)','New Monthly For-Sale Inventory (Smooth, Seasonally Adjusted)']:
	county_frame = county_frame.append(normalize2(county_frame,category,cat_norm[category]))
#actual execution of normalization by other columns

county_frame.index.rename(['time','DataCategory'],inplace=True)
county_frame.reorder_levels(['DataCategory','time'])
county_frame.rename(lambda x: x.replace(' ','_'),axis=1,inplace=True) #R doesn't like spaces in column names
county_frame[~county_frame.index.get_level_values('DataCategory').isin(cat_remap)].transpose().set_index(county_frame.columns.map(lambda x: '_'.join(x))).transpose().to_csv('Zillow data/All_Data_County.csv')
#Lots of data, so dropping data categories that were remapped and won't be needed anymore
#reindexing of columns (index reset sandwiched by transpose operations) needed to combine county and state names since R doesn't like dataframe multiindexes, columns or otherwise.l


