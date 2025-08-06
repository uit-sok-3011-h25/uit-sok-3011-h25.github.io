
from scipy.stats import norm
import numpy as np
import os

os.chdir(os.path.dirname(__file__))

def generate_backtest(f, df, name, estimation_win_size):
	# Appending values each time we estimate the volatility, makes it easier to 
	# keep track. Defining the lists we need
	datelist = []
	sigmalist = []
	d95list = []
	d99list = []
	ret = []

	# Iterating over returns to add to lists and estimate volatility
	for t in range(estimation_win_size, len(df)):
			
			# Append date and present return
			datelist.append(df.index[t].date())
			ret.append(df[name].iloc[t])

			# We pick data from t-estimation_win_size to "yesterday"
			x = df[name].iloc[t-estimation_win_size:t-1]

			# The supplied funciton for estimating the VaR is now applied on 
			# the data and previously estimated volatility
			d95, d99, sigma = f(x, sigmalist)

			# appending the estimates
			sigmalist.append(sigma)
			d95list.append(d95)
			d99list.append(d99)
			
	# returning everything
	return (np.array(d95list),
					np.array(d99list),
					np.array(sigmalist),
					np.array(datelist),
					np.array(ret)
					)

def evaluate(plt, d95, d99, ret, dates, heading):
	plt.cla()

	plt.plot(dates, d95)
	plt.plot(dates, d99)
	plt.plot(dates, ret)
	maxret = max(ret)
	bars = [maxret if d > r else 0 for d, r in zip(d95, ret)]
	plt.bar(dates, bars, color='gray', alpha=0.5, width=0.5, label='d95 > ret')
	
	plt.ylabel('VaR')
	plt.xlabel('Date')
	plt.legend(['95% Confidence Level', 
			 '99% Confidence Level', 
			 'actual return', 
			 'breaches 95%'], loc="lower right")
	plt.title(heading)
	plt.xticks(rotation=90)
	plt.subplots_adjust(bottom=0.15)
	plt.show()
	
	backtest = [np.round(sum(d>ret)/len(ret)*100,1) for d in [d95,d99]]

	for i in [0,1]:
		print(f"{heading} with {['95','99'][i]}% conficence interval: \n"
			f"breaches: {[sum(d95>ret), sum(d99>ret)][i]}\n"
			f"Backtesting (Realized VaR - % breaches): : {backtest[i]}%\n"
			)
		a=0


import pandas as pd
df = pd.read_pickle('output/X.df')


from datetime import  datetime
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


ESTIMATION_WINSIZE = 52
PVALS = [0.05, 0.01]




def normal_est(x, sigmalist):
	z = norm.ppf(PVALS)
	sigma = np.std(x, ddof=1)
	return z[0]*sigma, z[1]*sigma, sigma

def historcal_est(x, sigmalist):
	#using signal
	q95 = abs(np.quantile(x, PVALS[0]))
	q99 = abs(np.quantile(x, PVALS[1]))
	return -q95, -q99, None

def last_volat(x, sigmalist):
	x = np.array(x)
	z = norm.ppf(PVALS)
	if len(sigmalist) ==0:# If sigmalist is empty, use abs x
		sigma = np.std(x,ddof=1)
	else: # use sqrt of avg of last sigma**2 and x**2
		sigma = (0.1*x[0]**2 +0.9*sigmalist[-1]**2)**0.5
	return z[0]*sigma, z[1]*sigma, sigma

	
NAME = 'EQNR'

(normal95, normal99, 
 sigma, dates, ret )= generate_backtest(normal_est, 
		df, NAME, ESTIMATION_WINSIZE)
evaluate(plt, normal95, normal99, ret, dates,
		 'VaR Estimation Using the Normal Distribution Method')


(hist95, hist99, 
 sigma, dates, ret )= generate_backtest(historcal_est, 
		df, NAME, ESTIMATION_WINSIZE)
evaluate(plt, hist95, hist99, ret, dates,
		 'VaR Estimation Using Empirical Distribution')


(last95, last99, 
 sigma, dates, ret )= generate_backtest(last_volat, 
		df, NAME, ESTIMATION_WINSIZE)
evaluate(plt, last95, last99, ret, dates,
		 'VaR Estimation Using Past Volatility')
a=0