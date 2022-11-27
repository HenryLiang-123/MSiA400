# Question 1 part d
m <- as.matrix(cbind(c(0.7,0.4,0.2), c(0.2,0.3,0.4), c(0.1,0.3,0.4)))
ini <- c(0,1,0)
ini %*% m %*% m

# Question 2 part b
library(e1071)
dat <- cbind(c(2,5,1), c(231,52,129), c(6,12,0), c("no churn", "churn", "no churn"))
dat <- as.data.frame(dat)
colnames(dat) <- c("f1", "f2", "f3", "is_churn")
dat$f1 <- as.numeric(dat$f1)
dat$f2 <- as.numeric(dat$f2)
dat$f3 <- as.numeric(dat$f3)
svm_dat <- svm(factor(is_churn) ~ f1 + f2 + f3, kernel = "linear", data = dat)
w <- t(svm_dat$coefs) %*% svm_dat$SV
b <- svm_dat$rho
w
b

# Question 2 part c
test <- as.data.frame(t(as.matrix(c(2,230,0))))
colnames(test) <- c("f1", "f2", "f3")
pred <- predict(svm_dat, newdata=test, type = 'response') 
pred

# Question 2 part d
pred_train <- predict(svm_dat, newdata=dat, type = 'response') 
pred_train

# Question 3 part b
x1 <- exp(seq(-10, 300))
x1new <- (x1 - min(x1)) / (max(x1) - min(x1))
x2 <- seq(-10, 300) * seq(-10, 300)
x2new <- (x2 - min(x2)) / (max(x2) - min(x2))
