library(rstan)
## Load data
load("inst/extdata/Spence_turtle_data.Rdata")
# Herring

h_qua<-c()
h_age<-c()
h_len<-c()
for (i in 1:nrow(her_on))
{
	h_qua<-c(h_qua,rep(her_on$Quarter[i],her_on$CANoAtLngt[i]))
	h_age<-c(h_age,rep(her_on$Age[i],her_on$CANoAtLngt[i]))
	h_len<-c(h_len,rep(her_on$LngtClass[i],her_on$CANoAtLngt[i]))
}

h_qs<-(h_qua==4)*0.75

temp<-which(h_qua==1)

#### Quarter 1 only

h1_data <- list(t= h_age[temp], l= h_len[temp], N=length(temp))
fit1 <- stan(file = "vb.stan", data = h1_data, iter = 2000, chains = 1)

h1_out<-extract(fit1)

#### Quarter 4 only

h3_data <- list(t= h_age[-temp], l= h_len[-temp], N=length(h_len[-temp]))
fit3 <- stan(file = "vb.stan", data = h3_data, iter = 2000, chains = 1)

h3_out<-extract(fit3)

#### model i

h_complete_dat<-list(t= h_age, l= h_len, N=length(h_age),q= h_qs,mu_a=0.8576*2*pi-pi,sigma_a=0.40493)
fit_c <- stan(file = "vb.stan", data = h_complete_dat, iter = 2000, chains = 1,control=list(adapt_delta=0.85,max_treedepth=10))

h_comp_out<-extract(fit_c)

#### model ii

h_complete_dat1<-list(t= h_age+h_qs, l= h_len, N=length(h_age),q= h_qs,mu_a=0.8576*2*pi-pi,sigma_a=0.40493)
fit_c <- stan(file = "vb1.stan", data = h_complete_dat1, iter = 2000, chains = 1,control=list(adapt_delta=0.85,max_treedepth=10))

h_comp_out1<-extract(fit_c)

#### model iii

h_complete_dat2<-list(t= h_age, l= h_len, N=length(h_age),q= h_qs*(1/0.75),mu_a=0.8576*2*pi-pi,sigma_a=0.40493)

h1_fitsvm <- stan(file = "age_adjusted.stan", data = h_complete_dat2, iter = 2000, chains = 1,control=list(adapt_delta=0.9,max_treedepth=10))

h_fitv<-extract(h1_fitsvm)

#### model iv
temp<-which(h_age==0)
h_a_t<-c(h_age[-temp],h_age[temp])
h_a_l<-c(h_len[-temp],h_len[temp])
h_q_t<-c(h_qs[-temp],h_qs[temp])

h_complete_dat_test<-list(t= h_a_t, l= h_a_l, N1=length(h_a_t)-length(temp),N4=length(temp),q= h_q_t,mu_a=0.8576*2*pi-pi,sigma_a=0.40493)
fitvm <- stan(file = "von_moses1.stan", data = h_complete_dat_test, iter = 2000, chains = 1,control=list(adapt_delta=0.97,max_treedepth=10))

hiv_fitvm<-extract(fitvm)

#### model v

h1_complete_dat_test<-h_complete_dat_test
h1_complete_dat_test$q<-h1_complete_dat_test$q*(1/0.75)

h1_fitsvm <- stan(file = "seasonal_vm1.stan", data = h1_complete_dat_test, iter = 2000, chains = 1,control=list(adapt_delta=0.97,max_treedepth=10))

h1_fitsvm.c<-extract(h1_fitsvm)

######### Cod

## cod
c_qua<-c()
c_age<-c()
c_len<-c()
for (i in 1:nrow(cod_on))
{
	c_qua<-c(c_qua,rep(cod_on$Quarter[i],cod_on$CANoAtLngt[i]))
	c_age<-c(c_age,rep(cod_on$Age[i],cod_on$CANoAtLngt[i]))
	c_len<-c(c_len,rep(cod_on$LngtClass[i],cod_on$CANoAtLngt[i]))
}

c_qs<-(c_qua==4)*0.75

### Quarter 1 only
temp<-which(c_qua==1)

q1_data <- list(t= c_age[temp], l= c_len[temp], N=length(temp))
fit1 <- stan(file = "vb.stan", data = q1_data, iter = 2000, chains = 1,control=list(adapt_delta=0.85,max_treedepth=10))
q1_out<-extract(fit1)

#### Quarter 2 only
q3_data <- list(t= c_age[-temp], l= c_len[-temp], N=length(c_len[-temp]))
fit3 <- stan(file = "vb.stan", data = q3_data, iter = 2000, chains = 1)

q3_out<-extract(fit3)

#### Model i

complete_dat<-list(t= c_age, l= c_len, N=length(c_age),q= c_qs,mu_a=0.3123*2*pi-pi,sigma_a=5.4732)
fit_c <- stan(file = "vb.stan", data = complete_dat, iter = 2000, chains = 1,control=list(adapt_delta=0.9,max_treedepth=10))

comp_out<-extract(fit_c)

#### Model ii

complete_dat1<-list(t= c_age+ c_qs, l= c_len, N=length(c_age),q= c_qs,mu_a=0.3123*2*pi-pi,sigma_a=5.4732)

fit_c <- stan(file = "vb1.stan", data = complete_dat1, iter = 2000, chains = 1,control=list(adapt_delta=0.9,max_treedepth=10))

comp_out1<-extract(fit_c)

#### Model iii
complete_dat2<-list(t= c_age, l= c_len, N=length(c_age),q= c_qs*(1/0.75),mu_a=0.3123*2*pi-pi,sigma_a=5.4732)

c1_fitsvm <- stan(file = "age_adjusted.stan", data = complete_dat2, iter = 2000, chains = 1,control=list(adapt_delta=0.9,max_treedepth=10))

cv_fit<-extract(c1_fitsvm)

#### Model iv
temp<-which(c_age==0)
c_a_t<-c(c_age[-temp],c_age[temp])
c_a_l<-c(c_len[-temp],c_len[temp])
c_q_t<-c(c_qs[-temp],c_qs[temp])

complete_dat_test<-list(t= c_a_t, l= c_a_l, N1=length(c_a_t)-length(temp),N4=length(temp),q= c_q_t,mu_a=0.3123*2*pi-pi,sigma_a=5.4732)

fitvm <- stan(file = "von_moses1.stan", data = complete_dat_test, iter = 2000, chains = 1,control=list(adapt_delta=0.95,max_treedepth=10))

cod_vm1<-extract(fitvm)

#### Model v

complete_dat_test1<-complete_dat_test
complete_dat_test1$q<-complete_dat_test$q*(1/0.75)

c1_fitsvm <- stan(file = "seasonal_vm1.stan", data = complete_dat_test1, iter = 2000, chains = 1,control=list(adapt_delta=0.95,max_treedepth=10))

c1_fitsvm.c<-extract(c1_fitsvm)

#############
#############
#############
## YPR analysis

single_species_model<-function(param,a,b,age.range,sel.at.age,Mort)
{
	require(fishmethods)
	k<-param[1]
	Linf<-param[2]
	t0<-param[3]
	sigmasq<-param[4]
	uage<-c(age.range[-1],max(age.range)+1)
	lts<-Linf*exp(sigmasq/2)*(1-1/k*exp(k*t0)*(exp(-k* age.range)-exp(-k* uage)))
	weighs01 <- a*(lts)^b
	return( ypr(age = age.range, wgt = weighs01, partial = sel.at.age, M = Mort,
              plus = FALSE, maxF = 4,incrF = 0.01,graph=F)$Reference_Points[, 2])
}


age.range <- 0:19

#### Run in parallel

library(parallel)
cl <- makeCluster(getOption("cl.cores", 8))
clusterExport(cl, as.list(ls()))

#### Herring

h.a <- 0.00646
h.b <- 3.05
h.sel.at.age <- c(rep(0, 2), rep(1, 18))
h.Mort<- c(0.767,0.385,0.356,0.339,0.319,0.314,rep(0.307,14)) ## From page 297 of ICES Hawg report 2016

hm_i<-cbind(h_comp_out$k,h_comp_out$l_inf/1000,h_comp_out$t0)
hm_ii<-cbind(h_comp_out1$k,h_comp_out1$l_inf/1000,h_comp_out1$t0)
hm_iii<-cbind(h_fitv$k,h_fitv$l_inf/1000,h_fitv$t0)
hm_iv<-cbind(hiv_fitvm$k,hiv_fitvm$l_inf/1000,hiv_fitvm$t0)
hm_v<-cbind(h1_fitsvm.h$k,h1_fitsvm.h$l_inf/1000,h1_fitsvm.h$t0)

hsm_i<-matrix(parRapply(cl,hm_i, single_species_model,a=h.a,b=h.b,sel.at.age=h.sel.at.age,Mort=h.Mort,age.range=age.range),ncol=2)
hsm_ii<-matrix(parRapply(cl,hm_ii, single_species_model,a=h.a,b=h.b,sel.at.age=h.sel.at.age,Mort=h.Mort,age.range=age.range),ncol=2)
hsm_iii<-matrix(parRapply(cl,hm_iii, single_species_model,a=h.a,b=h.b,sel.at.age=h.sel.at.age,Mort=h.Mort,age.range=age.range),ncol=2)
hsm_iv<-matrix(parRapply(cl,hm_iv, single_species_model,a=h.a,b=h.b,sel.at.age=h.sel.at.age,Mort=h.Mort,age.range=age.range),ncol=2)
hsm_v<-matrix(parRapply(cl,hm_v, single_species_model,a=h.a,b=h.b,sel.at.age=h.sel.at.age,Mort=h.Mort,age.range=age.range),ncol=2)

#### Cod

c.a<-0.00759
c.b<-3.06
c.sel.at.age <- c(rep(0, 2), rep(1, 18))
c.Mort<-c(0.537,0.386,0.306,0.262,0.237,0.223,rep(0.211,14)) #From ICES WGCSE report 2016

m_i<-cbind(comp_out$k,comp_out$l_inf/1000,comp_out$t0)
m_ii<-cbind(comp_out1$k,comp_out1$l_inf/1000,comp_out1$t0)
m_iii<-cbind(cv_fit$k, cv_fit$l_inf/1000,cv_fit$t0)
m_iv<-cbind(cod_vm1$k,cod_vm1$l_inf/1000,cod_vm1$t0)
m_v<-cbind(c1_fitsvm.c$k,c1_fitsvm.c$l_inf/1000,c1_fitsvm.c$t0)

sm_i<-matrix(parRapply(cl,m_i, single_species_model,a=c.a,b=c.b,sel.at.age=c.sel.at.age,Mort=c.Mort,age.range=age.range),ncol=2)
sm_ii<-matrix(parRapply(cl,m_ii, single_species_model,a=c.a,b=c.b,sel.at.age=c.sel.at.age,Mort=c.Mort,age.range=age.range),ncol=2)
sm_iii<-matrix(parRapply(cl,m_iii, single_species_model,a=c.a,b=c.b,sel.at.age=c.sel.at.age,Mort=c.Mort,age.range=age.range),ncol=2)
sm_iv<-matrix(parRapply(cl,m_iv, single_species_model,a=c.a,b=c.b,sel.at.age=c.sel.at.age,Mort=c.Mort,age.range=age.range),ncol=2)
sm_v<-matrix(parRapply(cl,m_v, single_species_model,a=c.a,b=c.b,sel.at.age=c.sel.at.age,Mort=c.Mort,age.range=age.range),ncol=2)
