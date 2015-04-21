#Define parameter values

Xbars=NULL # a collection of means obtained from many samples

R=1000 # number of simulations

n=30 # sample size

df=5 # degrees of freedom

# The following loop will generate many samples of size n  from the t distribution with degrees of freedom, df.

#for(i in 1:R) Xbars[i]=mean(rt(n,df))

for(i in 1:R) Xbars[i]=mean(rgamma(n,shape=3,rate=0.8))

#create a histogram of a "population"

#hist(rt(n=100000,df=2))

hist(rgamma(100000,shape=3,rate=0.8))

#Create a histogram of X bars:

hist(Xbars)