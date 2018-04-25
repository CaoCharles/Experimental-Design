# 生成訓練數據

set.seed(1999)
x1 <- rnorm(1000)
x2 <- rnorm(1000)
z <- 1+2*x1+3*x2
pr <- 1/(1+exp(-z))
y <- rbinom(1000,1,pr)


# 使用logloss作為訓練目標函數 -------------------------------------------------------

df <- data.frame(y=y,x1=x1,x2=x2)
glm.fit <- glm(y~x1+x2,data = df,family = "binomial")

# 下面使用AUC作為訓練目標函數 ---------------------------------------------------------

library(ROCR)

CalAUC <- function(real,pred){
  rocr.pred=prediction(pred,real)
  rocr.perf=performance(rocr.pred,'auc')
  as.numeric(rocr.perf@y.values)
}


# 初始值 ---------------------------------------------------------------------

beta0=c(1,1,1)

loss <- function(beta){
  z=beta[1]+beta[2]*x1+beta[3]*x2
  pred=1/(1+exp(-z))
  -CalAUC(y,pred)
}

res <- optim(beta0,loss,method = "Nelder-Mead",control = list(maxit = 100))
cat("直接用AUC訓練",-res$value)
cat("使用glm函數",CalAUC(y,glm.fit$fitted.values))

