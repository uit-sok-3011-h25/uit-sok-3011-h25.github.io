import nbformat
import numpy as np
import os

import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.api as sm

MINVAL_Y = -0.1
MAXVAL_Y = 0.3
MINVAL_X = 0
MAXVAL_X = 2


def plot_sec_mkt_lines():
	df, names = get_data()
	df_weekly = weeklyfy(df)
	df_sml = get_sec_mkt_line_data(df_weekly, names)

	fig, ax = plt.subplots(nrows=2, ncols=2)

	df_sml = df_sml[df_sml['mean']>MINVAL_Y]
	
	
	plot_sec_mkt_line(df_sml, ax[0,0], 0.05, True)
	plot_sec_mkt_line(df_sml, ax[1,0], 0.03)
	plot_sec_mkt_line(df_sml, ax[0,1], 0.01)
	plot_sec_mkt_line(df_sml, ax[1,1], 0.0)

	# Add a single legend for the whole figure
	handles, labels = ax[0, 0].get_legend_handles_labels()
	fig.legend(handles, labels,loc='upper center', frameon=False, ncol= 2)
	fig.subplots_adjust(hspace=0.7)

	return fig, ax





def get_data():
	# Read the data saved in lecture 3, and pick relevant columns and making sure data is unique
	path = os.path.dirname(__file__)+'/data/stocks.df'
	df = pd.read_pickle(path)
	#df = pd.read_pickle("E:\\OneDrive - UiT Office 365\\Docs\\Undervisning\\SOK-3011\\uit-sok-3011-h24.github.io\\finans\\output\\stocks.df")
	names = df.drop_duplicates(subset=['ISIN', 'Symbol']).set_index('ISIN')['Symbol'].to_dict()

	# We will only look at OSEBX stocks
	df = df[df['IN_OSEBX']>0]

	df['rf'] = df['NOWA_DayLnrate']
	df['lnDeltaOSEBX_rf'] = df['lnDeltaOSEBX']- df['rf']
	df['lnDeltaP_rf'] = df['lnDeltaP']- df['rf']


	df['Date'] = pd.to_datetime(df['Date'])
	df = df.set_index(['ISIN', 'Date'])



	# Keeping only the 20% biggest companies in the period
	df = df[['lnDeltaP_rf', 'lnDeltaOSEBX_rf', 'rf', 'OSEBXmktshare_prevmnth']]

	return df, names


def weeklyfy(df):
	"Creates weekly data"
	# Group by 'ISIN' and resample on 'Date' level, ensuring 'ISIN' stays in the index
	weekly_df = (
		df
		.groupby(level='ISIN')       # Group by ISIN
		.resample('W', level='Date') # Resample weekly on the Date level
		.sum()                       # Aggregate with sum
	)

	weekly_df_mean = (
		df
		.groupby(level='ISIN')       # Group by ISIN
		.resample('W', level='Date') # Resample weekly on the Date level
		.mean()                       # Aggregate with sum
	)

	weekly_df['OSEBXmktshare_prevmnth'] = weekly_df_mean['OSEBXmktshare_prevmnth']

	# first define a function that calculates the beta

	return weekly_df


def beta_calc(group):
	group = group.dropna()
	if len(group) <10:
		return None
	X = group['lnDeltaOSEBX_rf']
	y = group['lnDeltaP_rf']
	X = sm.add_constant(X)  # Adds intercept
	model = sm.OLS(y, X).fit()
	return model.params['lnDeltaOSEBX_rf']  # Return the beta coefficient

def get_sec_mkt_line_data(df, names):
	# calculating beta for all isins
	df_sml = []
	MULT = 52


	for isin, gr in df.groupby('ISIN'):
		mean = gr['lnDeltaP_rf'].mean()*MULT
		rf = gr['rf'].mean()*MULT
		beta = beta_calc(gr)
		index_rf = gr['lnDeltaOSEBX_rf'].mean()*MULT
		mkt_shr = gr['OSEBXmktshare_prevmnth'].mean()
		df_sml.append({'ISIN':isin, 'mean':(mean + rf), 'beta':beta, 'rf':rf, 
				'index_rf':index_rf, 'symbol': names[isin], 
				'mkt_shr':mkt_shr})

	df_sml = pd.DataFrame(df_sml)

	return df_sml


def plot_sec_mkt_line(df_sml, ax, mktsh, setlabel = False):

	ax.set_title(f'Minimun {int(mktsh*100)} % markedsandel')
	df_sml = df_sml[df_sml['mkt_shr']>mktsh]


	rf = np.mean(df_sml['rf'])
	index_rf = np.mean(df_sml['index_rf'])

	# Create security market line plot


	ax.scatter(df_sml['beta'], df_sml['mean'], alpha=0.7)

	#adding names
	for symbol, x, y in zip(df_sml['symbol'], df_sml['beta'], df_sml['mean']):
		if not np.any(np.isnan([x,y])):
			ax.text(x, y, symbol, fontsize=7, ha='right', va='bottom')

	# Set plot labels and title
	ax.set_xlabel('Beta')
	ax.set_ylabel('Avkastning')
	ax.set_xlim(MINVAL_X, MAXVAL_X)
	ax.set_ylim(MINVAL_Y, MAXVAL_Y)


	# creating the line:
	df_sml = df_sml.dropna()
	X = df_sml['beta']
	y = df_sml['mean']
	X = sm.add_constant(X)  # Adds intercept
	p = sm.OLS(y, X).fit().params
	xvals = np.linspace(MINVAL_X, MAXVAL_X, 100)
	pred_ret = p['const'] + xvals*p['beta']
	capm = rf + xvals*index_rf
	if setlabel:
		ax.plot(xvals, pred_ret, label = 'Predikert avkastning')
		ax.plot(xvals, capm, label = 'Verdipapirmarkedslinjen', color='red')
	else:
		ax.plot(xvals, pred_ret)
		ax.plot(xvals, capm, color='red')
	ax.spines['right'].set_visible(False)
	ax.spines['top'].set_visible(False)

	ax.scatter(1, index_rf+rf, color='red')
	ax.text(1, index_rf+rf, 'INDEKS', fontsize=8, ha='left', va='top', fontweight='bold')
	ax.text(0, rf, 'rf', fontsize=8, ha='right', va='bottom', fontweight='bold')



