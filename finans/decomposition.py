import numpy as np


def calculate_eigen(cov_matrix, threshold):
	# Compute eigenvalues (L) and eigenvectors (P)
	L, P = np.linalg.eig(cov_matrix)
	
	# Identify imaginary eigenvalues (if any)
	real_indices = np.where(np.isreal(L))[0]  # Only real eigenvalues
	L = np.real(L[real_indices])  # Filter eigenvalues to keep only real parts
	P = np.real(P[:, real_indices])  # Keep corresponding real eigenvectors
	
	positive_indices = np.where(L > threshold*max(L))[0]
	L = L[positive_indices]
	P = P[:, positive_indices]

	vp = proportion_of_variance(P, L)
	mc, removeindxs = identify_multicollinear_variables(L, vp,0.5, 30)
	valid_idx = np.setdiff1d(np.arange(len(L)), removeindxs)

	L = L[valid_idx]
	P = P[:, valid_idx]
	
	return L, P

def normalize_eigenvectors(P):
	# Normalize the eigenvector matrix so that each row sums to 1 (creating pseudo-portfolio weights)
	R = P / np.sum(P, axis=0, keepdims=True)
	v = np.var(R,0)
	keep = v<np.sort(v)[int(0.5*len(v))]
	R = R[:,keep]
	keep = (np.var(R,0)-np.min(R,0))<2
	R = R[:,keep]
	return R

def get_independent_portfolios(cov_matrix, threshold):
	L, P = calculate_eigen(cov_matrix, threshold)
	R = normalize_eigenvectors(P)
	return R


def proportion_of_variance(P, L):
	# P is the eigenvector matrix, L is the vector of eigenvalues
	# P.shape[0] is the number of variables (n)
	# P.shape[1] is the number of eigenvalues/eigenvectors (k)

	# Initialize the matrix to store proportions
	variance_proportion = np.zeros(P.shape)

	# Calculate the proportion of variance for each variable and eigenvalue
	for i in range(P.shape[0]):  # Iterate over each variable
		for j in range(P.shape[1]):  # Iterate over each eigenvalue
			# Numerator: squared eigenvector element multiplied by the eigenvalue
			numerator = (P[i, j] ** 2) / L[j]
			# Denominator: sum of squared eigenvector elements times their corresponding eigenvalues
			denominator = np.sum((P[i, :] ** 2) / L)
			# Proportion of variance
			variance_proportion[i, j] = numerator / denominator

	return variance_proportion

def identify_multicollinear_variables(L, variance_prop, threshold=0.5, eigenvalue_threshold=0.01):
 
	# Step 2: Initialize a list to store the results
	multicollinear_pairs = []
	variable_pairs = []
	
	# Step 3: Loop over eigenvalues to check if they are less than eigenvalue_threshold
	maxL = np.max(L)**0.5
	for j in range(len(L)):
		if maxL/abs(L[j])**0.5 > eigenvalue_threshold:  # Check if eigenvalue is small enough
			# Step 4: Identify variables where the proportion of variance is greater than the threshold
			indices = np.where(variance_prop[:, j] > threshold)[0]
			print(max(variance_prop[:, j]))
			# Step 5: If two or more variables meet the condition, store their indices
			if len(indices) >= 2:
				multicollinear_pairs.append((j, indices))  # Store the eigenvalue index and variable indices
				variable_pairs.extend(indices)
	return multicollinear_pairs, variable_pairs




