//using uniform dist for uncertainty

//do the linf and lt equation thingy

functions{
   vector grow(vector toy, real c)
   {
	  vector [num_elements(toy)] ret;
	  for (i in 1:num_elements(toy))
	  {
         ret[i]<-((toy[i]<=0.75)*1.0/(0.75)*c*toy[i]+(toy[i]>0.75)*(4.0*(1-c)*(toy[i]-1.0)+1.0));
      }
      return ret;
   }
}

data {
  int <lower=0> N1; // number of fish older than 1
  int <lower=0> N4; // number of aged 0 fish in quarter 4
  //length
  real l[N1+N4];
  //age
  vector<lower=0> [N1+N4] t; 
  // month
  vector<lower=0,upper=1> [N1+N4] q;
  real<lower=-pi(),upper=pi()> mu_a;
  real<lower=0> sigma_a;
} 

transformed data{
  //expected length at time/age t
  real b[N1+N4];
  for (i in 1:(N1+N4)){
    b[i]<-log(l[i]);
  }
}

parameters {
  real<lower=0> l_inf; 
  real <lower=0,upper=1> k; 
  real<lower=0> sigmasq;
  vector<lower=-pi(),upper=pi()> [N1] toy1;
  vector<lower=-pi(),upper=pi()/2> [N4] toy4;
  real <upper=0>t0;
  real <lower=0,upper=1> c;
} 

transformed parameters {
  real<lower=0> sigma; 
  real E_ll[N1+N4];
  vector <upper=0> [N1+N4] x;
  vector <lower=0>[N1+N4] age;
  vector<lower=0,upper=1> [N1+N4] toy;
  toy<-(append_row(toy1,toy4)+pi())/(2*pi());
  sigma <- sqrt(sigmasq);
  age<- t-grow(toy,c)+c*q;
  x<- -k*(age-t0);
  for (i in 1:(N1+N4)){
    E_ll[i]<-log(l_inf)+log(1-exp(x[i]));
  }
} 

model {
  sigmasq~inv_gamma(0.001,0.001);
  toy1 ~ von_mises(mu_a, sigma_a);
  toy4 ~ von_mises(mu_a, sigma_a);
  b ~ normal(E_ll,sigma);
}