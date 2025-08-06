import functions
import decomposition
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import datetime

datetime

df = pd.read_pickle('notes/output/output.df')
# Defining risk free, cov matrix and mean vector
rf = np.exp(0.05/12)-1#df['NOWA_DayLnrate'].mean()

cov_matrix, means = functions.calc_moments(df, False)
print(cov_matrix[0,:5])
R = decomposition.get_independent_portfolios(cov_matrix, 0.00001)
cov_matrix = R.T @ cov_matrix @ R
means = R.T @ means
# Create a vector of ones with the same length as the number of columns in the covariance matrix
ones = np.ones(cov_matrix.shape[0])

# Some useful scalar values
A = np.dot(ones.T, np.dot(np.linalg.inv(cov_matrix), ones))
B = np.dot(ones.T, np.dot(np.linalg.inv(cov_matrix), means-rf))
C = np.dot(means.T-rf, np.dot(np.linalg.inv(cov_matrix), means-rf))

#Creating plot
fig, ax = plt.subplots(figsize=(10, 6))

# Setting the range of rp values and sigma values
rp_values = np.linspace(0, 0.05, 100)


# Calculate and plot sigma values for each rp
sigma_values = 1/A + ((rp_values - abs(B)/A)**2) / (C - B**2/A)
ax.plot(sigma_values**0.5, rp_values+rf, label='Efficient Frontier')

# Calculate the tangency point and tangent
tangency_rp = C/abs(B)
tangency_sigma =  1/A + ((tangency_rp - abs(B)/A)**2) / (C - B**2/A)

ax.plot(tangency_sigma**0.5, tangency_rp + rf, 'ro')
sigma_range = np.linspace(0, np.max(sigma_values**0.5), 100)
ax.plot(sigma_range, rf + sigma_range*tangency_rp/tangency_sigma**0.5, color='r', linestyle='--', label='Tangency Point')
ax.set_xlim([0, np.max(sigma_values**0.5)])
ax.set_ylim([0, np.max(rp_values)])
ax.set_xlabel('Sigma (Risk)')
ax.set_ylabel('Rp (Return)')
ax.set_title('Efficient Frontier')
ax.grid(True)
ax.legend()