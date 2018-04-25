#date:2018/03/08
#exercise2.5
#charles's method
x<-c(15.3,-31.8,-35.6,-14.5,3.1,-24.5)
y=NULL
for(i in 1:10000){
   num=sample(0:6,1,T,prob = c(1,6,15,20,15,6,1)/64);num
if(num==0){XX<-x}
else{sign=sample(1:6,num,F);sign
XX<-c(x[sign]*-1,x[-sign])}

y=rbind(y,XX)
}
#模擬一萬筆64種組合值的數據，並計算樣本的位置
simulation_vector<-apply(y,1,sum)
hist(simulation_vector,breaks=200)
sum(x)
sum(sum(x)>simulation_vector)/10000
#x的64種排列組合
X_combination<-matrix(data=unique(sort(simulation_vector)),ncol = 8 , byrow = T )
sum(sum(x)>X_combination)/64

#method2(need to remove values)
x<-c(15.3,-31.8,-35.6,-14.5,3.1,-24.5)
g=NULL
for(i in 1:10000){
  y=sample(c(1,-1),6,T,prob = c(0.5,0.5))
  Y=c(x*y)
  sum(Y)
  g=c(g,Y)
}
g<-matrix(data = g,ncol = 6,byrow = T);g
x_100000<-apply(g,1,sum);x_100000
hist(x_100000,breaks=10000)
sum(sum(x)>x_100000)/10000
#倒數題
#problem2.1
x<-c(0.950,0.978,0.762,0.733,0.823,1.011)
y=NULL
for(i in 1:10000){
  num=sample(0:6,1,T,prob = c(1,6,15,20,15,6,1)/64);num
  if(num==0){XX<-x}
  else{inverse=sample(1:6,num,F);inverse
  XX<-c(1/x[inverse],x[-inverse])}
  
  y=rbind(y,XX)
}
#simulation and calculate estamation of P
simulation_x<-apply(y,1,sum)
hist(simulation_x,breaks=200)
sum(x)
sum(sum(x)>simulation_x)/10000
#combination of x
combination_x<-matrix(unique(sort(simulation_x)),ncol = 8,byrow = T)
sum(sum(x)>combination_x)/64
