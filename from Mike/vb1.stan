//using uniform dist for uncertainty

//do the linf and lt equation thingy
data {
  int <lower=0> N;
  //length
  real<lower=0> l[N];
  //age
  real<lower=0> t[N]; 
} 

transformed data{
  //expected length at time/age t
  real l_tt[N];
  for (i in 1:N){
    l_tt[i] <- log(l[i]);
  }
}

parameters {
  real<lower=0> l_inf; 
  real <lower=0,upper=3> k; 
  real<lower=0> sigmasq;
  real <upper=0>t0;
} 


transformed parameters {
  real<lower=0> sigma; 
  real E_ll[N];
  sigma <- sqrt(sigmasq);
  for (i in 1:N){
    E_ll[i] <- log(l_inf)+log(1-exp(-k*(t[i]-t0)));
  }
} 

model {
  //sigmasq~inv_gamma(0.001,0.001);
  l_tt ~ normal(E_ll,sigma);
}
