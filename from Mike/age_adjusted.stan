//using uniform dist for uncertainty

//do the linf and lt equation thingy
data {
  int <lower=0> N;
  //length
  real<lower=0> l[N];
  //age
  vector <lower=0> [N] t; 
  // quarter
  vector<lower=0,upper=1>[N] q;
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
  real <lower=0,upper=1>c_d;
} 


transformed parameters {
  real<lower=0> sigma; 
  real E_ll[N];
  vector <lower=0>[N] age;
  sigma <- sqrt(sigmasq); 
  age<-t+c_d*q;
  for (i in 1:N){
    E_ll[i] <- log(l_inf)+log(1-exp(-k*(age[i]-t0)));
  }
} 

model {
  //sigmasq~inv_gamma(0.001,0.001);
  l_tt ~ normal(E_ll,sigma);
}
