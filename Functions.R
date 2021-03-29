#The DTO model When X is Unifromlly distributed
n = 5
min = 0
maxx = 1

ff<- function(x)
{
  x*dunif(x, min, maxx) 
}
a<- rep(NA,n)
a

a[n] <- pdf2(0,1)*(1/(1+1))
a
for(k in (n-1):1)
{
  a[k] = (1/(1+1))*(a[[k+1]]*(1-punif(a[[k+1]], min = 0,max = 1)))+integrate(ff, lower = 0, upper = a[[k+1]])$value

}
a
round(a,5)
plot(a)
#=====================================================
#The DTO model When X is normally distributed
mu = 50
sd = 10
pdfNormal <- function(x)
{
  x*(1/sqrt(2*pi*sd^2))*exp(-((x-mu)^2)/(2*sd^2))
}
pdfNormal2 <- function(x)
{
  (1/sqrt(2*pi*sd^2))*exp(-((x-mu)^2)/(2*sd^2))
}
n=20
a<- rep(NA,n)
a  
x = rnorm(1, mu, sd)
#The value of a at n time is the mean of the total delay X
a[n] <-  mu*(1/(1+1))
a

for(k in (n-1):1)
{
  a[k] =(1/(1+1))*(integrate(pdfNormal, lower = -Inf, upper = a[[k+1]])$value)+ (a[[k+1]]*integrate(pdfNormal2, lower = a[[k+1]],
                                                                                               upper = 2000)$value)
}
a
plot(a)
#=====================================================
#The DTO model When X is exponentially distributed
n = 20
pdfExp<- function(x)
{
  dexp(x, rate=1/50) 
}

pdfExp1<- function(x)
{
  x*dexp(x, rate=1/50) 
}
a<- rep(NA,n)
a 
a[n] =(1/(1+1))*50 
a
for(k in (n-1):1)
{
  a[k] = (1/(1+1))*integrate(pdfExp1, lower = 0, upper = a[[k+1]])$value + (a[[k+1]]*integrate(pdfExp, lower =a[[k+1]], upper = 2000)$value)
  #a[k] =1/(1+0)* a[[k+1]]*(1-pdfExp(a[[k+1]]))+integrate(pdfExp, lower = 0, upper = a[[k+1]])$value
}
a
#==================================================================================
#The COT model When X is uniformlly distributed distributed
VSTAR <- NULL
COST <- NULL
#For c<=1/2
for(c in seq(from=0, to=0.5, by=0.1))
{
  v = 1 - (2*c)^(1/2)
  VSTAR = rbind(VSTAR,data.frame(v))
  COST = rbind(COST,data.frame(c))
}
#For c>1/2
for(c in seq(from=0.5, to=1, by=0.1))
{
  v = -c +(1/2)
  VSTAR = rbind(VSTAR,data.frame(v))
  COST = rbind(COST,data.frame(c))
}
#==================================================================================
#The COT model When X is normally distributed distributed
epsilon = 0.01
VSTAR <- NULL
COST <- NULL
MMAX = 100
MMIN = 1

for(c in seq(from=MMIN, to=50, by=1))
  
{
  
  for(v in seq(from=MMIN, to=MMAX, by=0.01))
    
  {
    
    f1 <- function(x)
      
    {
      
      x*(1/sqrt(2*pi*sigma^2))*exp(-((x-mu)^2)/(2*sigma^2))
      
    }
    
    first <- integrate(f1, lower = v, upper = MMAX)$value
    
    f2 <- function(x)
      
    {
      
      (1/sqrt(2*pi*sigma^2))*exp(-((x-mu)^2)/(2*sigma^2))
      
    }
    
    second <- integrate(f2, lower = v, upper = MMAX)$value
    
    first
    
    second
    
    value = (first-v*second-c)
    
    value
    
    v
    
    if(abs(value)<epsilon)
      
    {
      
      VSTAR = rbind(VSTAR,data.frame(v))
      
      print(v)
      
      COST = rbind(COST,data.frame(c))
      
      break
      
    }
    
  }
  
}
#==================================================================================
#The COT model When X is exponentially distributed
mu=50
sigma = 10
expPDf1 <- function(x)
{
  x*dexp(x, rate=1/50) 
}
expPDf1 <- function(x)
{
  v*dexp(x, rate=1/50) 
}
VSTAR <- NULL
COST <- NULL
for(c in seq(from=1, to=50, by=1))
{
  v = -log(c/50)/(1/50)
  VSTAR = rbind(VSTAR,data.frame(v))
  COST = rbind(COST,data.frame(c))
}
