data {
	int<lower=1> N; // number of obs
	int L; // number of groups
	int<lower=1, upper = L> id[N]; // group membership (pre/post lunch)
	real y[N]; // footfall
}
parameters {
	real mu[L]; // population mean
	real<lower=0> sigma[L]; // population variance
	
}
model {
	mu ~ normal(0, 10);
	sigma ~ cauchy(0, 5);
	{	for (n in 1:N)
			y ~ normal(mu[id[n]], sigma[id[n]]);
	}
}
