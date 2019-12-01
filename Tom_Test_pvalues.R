

```{r}
n<-10000

rsq<-vector(n, mode = 'numeric')
pval<-vector(n, mode = 'numeric')
for(i in 1:n){
  
  x<-rnorm(100)
  y<-rnorm(100)
  
  fm<-lm(y~x)
  
  fm_sum<-summary(fm)
  rsq[[i]]<-fm_sum$r.squared
  pval[[i]]<-fm_sum$coefficients[2,4]
}

hist(rsq, breaks = 100)
hist(pval, breaks = 100)

(sum(pval<0.05) / length(pval))*100


```