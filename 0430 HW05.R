#實驗設計作業
#7.1
g = 6                     # 分組
n = 2                     # 每組個數
N = n*g                   # 總個數
F = qf(0.95,g-1,N-g)      # a=0.05 虛無假設下臨界點之F值
power=1-pf(F,g-1,N-g,4*n) # 檢定力power

while(power<0.7){
  n = n+1
  N = n*g
  F = qf(0.95,g-1,N-g)
  power = 1-pf(F,g-1,N-g,4*n)
}
seq <- seq(0,0.995,by=0.005)
q <- qf(seq,g-1,N-g) 
dp <- df(q,g-1,N-g) 
nq <- qf(seq,g-1,N-g,4*n)
ndp <- df(nq,g-1,N-g,4*n)
plot(dp~q,type="l",main="F 機率密度圖",xlab="x",ylab = "pdf",xlim=c(0,15),ylim=c(0,0.8),col="#0000FF",lwd=3)
lines(ndp~nq,type="l",col="#00FF00",lwd=3)
abline(v=F,col ="#FFFF00",fill ="gray",lwd=3)
N;power

#ggplot
data <- cbind(q,dp,nq,ndp)
colnames(data) <- c("H0_x","H0_y","H1_x","H1_y")
data <- as.data.frame(data)
ggplot(data)+ labs(title = "F distribution (central & non-central)",size=40,x="x",y="pdf")+
  theme(axis.text=element_text(size=12),title=element_text(size=40))+
  geom_line(mapping = aes(x=H0_x,y=H0_y),col="#00EE00",lwd=1.5)+
  geom_line(mapping = aes(x=H1_x,y=H1_y),col="#00BBFF",lwd=1.5)+
  geom_vline(mapping = aes(xintercept=F),col="#FF0000",lwd=2,linetype=2)+
  scale_x_continuous(breaks = c(0:2,F,3:15),labels = c(0:2,"F",3:15))+
  theme(panel.background = element_rect(fill ="#FFFFF0", colour = "black",size = 2))


#7.3
g=4
N=16
n=N/g
var0=3
alphasq=6#給定的處理平方和
E1=0.01#顯著水準
afa=1-E1
zeta=(alphasq*n)/var0#非中心參數
z=qf(afa,g-1,N-g)#檢定臨界值
1-pf(z,g-1,N-g,zeta)#檢定力


#problem7.2
var0=35#給定的誤差變異數
ybar=c(45,32,60)#每組的估計值
alpha=ybar-mean(ybar)
afasq=sum(alpha^2)
zeta=0#非中心參數
n=2#每組的樣本數
g=3#組數
power=0
while(power<0.95){
  z=qf(0.99,g-1,n*3-g)
  zeta=(n*afasq)/var0
  power=1-pf(z,g-1,n*3-g,ncp=zeta)
  n=n+1
}
N=n*g;N#檢定力超過0.95的最低樣本數
power#最後的檢定力


# plot
