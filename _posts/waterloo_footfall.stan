// waterloo_footfall.stan
data {
	int N; // observations
	vector[N] time; // time of day measured in minutes from 00:00
	vector[N] y; // footfall count - integer as count data
}
parameters {
	real mu_1; // mean of first normal
	real mu_2; // mean of second normal
	real<lower=0> sigma_1; // variance of first normal;
	real<lower=0> sigma_2; // variance of second normal
	real<lower=0,upper=1> lambda; // amount of mixing
}
model {
	mu_1 ~ normal(0, 5);
	mu_2 ~ normal(0, 5);
	sigma_1 ~ normal(0, 20);
	sigma_2 ~ normal(0, 20);
	target += log_mix(lambda,
					  normal_lpdf(y | mu_1, sigma_1),
					  normal_lpdf(y | mu_2, sigma_2));
}