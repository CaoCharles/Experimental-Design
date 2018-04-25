#睹计(Mid-Square Method)
#ヴ匡せ计计璸衡ㄤキよ计(い丁计キよ)
options(scipen = 999 , digits= 1)
options("digits") 
x<-123456;
y=NULL
x^2
x^2/1000
1234/1000
(trunc(x^2/1000)/1000000-trunc(x^2/1000000000))
x2<-(trunc(x^2/1000)/1000000-trunc(x^2/1000000000))*1000000;x2
for(i in 1:10){
  x<-(trunc(x^2/1000)/1000000-trunc(x^2/1000000000))*1000000
    y=c(y,x)
}
x
y
y=as.factor(y)
y
nchar(y[1])
for(i in 1:10){
 while (nchar(y[i]) != 6){{y[i]=paste0(0,y[i])}}
}
y 


#纒肕's method
ms <- function(seed, len) {
  randvector <- NULL
  for(i in 1:len) {
    value <- seed * seed 
    randvector <- c(randvector, value)
    seed <- (value %/% 1000) %% 1000000
  }   
  return(randvector)
}
123456*123456
signif(123456*123456,12)
x<-ms(123456,10000)
zz<-(x %/% 1000) %% 1000000

View(y)
hist(y)


