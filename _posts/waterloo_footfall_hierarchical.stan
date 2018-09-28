// waterloo_footfall_hierarchical.stan
data {
	int K; // Number of groups (two here)
	real y[K]; // footfall count
	
}
parameters {
	real theta[K];
	real mu;
	real<lower=0> tau;
}
model {
	real<lower=0> sigma[K];
	
	sigma ~ cauchy(0, 5)
	theta ~ normal(mu, tau);
	y ~ normal(theta, sigma);
}
