import numpy as np

#class NoiseMechanism:
#@staticmethod
def laplaceMechanism(x, epsilon):
	noisy_val = x + np.random.laplace(0, 1/epsilon, 1)[0]
	return noisy_val
    
#@staticmethod
def gaussianMechanism(x, epsilon, delta):
	sigma = np.sqrt(2 * np.log(1.25 / delta)) * 1 / epsilon
	noisy_val = x + np.random.normal(0, sigma, 1)[0]
	return noisy_val