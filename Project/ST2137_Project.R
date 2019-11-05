setwd("C:/Users/Tay/Downloads/NUS/ST/ST2137/Project/Data")

library(lmomco) # for estimator T4
library(robustbase) # for estimator T5
library(RColorBrewer) # for more colour palette for plots

disc_unif = read.table("disc_unif.txt", header=F)
disc_unif = disc_unif$V1

################################### Define the estimators ###################################

# the following are 6 estimators for standard deviation

# Sample Standard Deviation
T1 <- function(x){
  return( sqrt(sum((x-mean(x))^2/(length(x)-1))) )
}

# Median Absolute Difference
T2 <- function(x){
  return(1.4826 * mad(x))
}

# Interquartile Range
T3 <- function(x){
  return(IQR(x) / 1.34898)
}

# Gini Mean Difference
T4 <- function(x){
  return( sqrt(pi)/2 * gini.mean.diff(x)$gini )
}

T5 <- function(x){
  return( Sn(x, constant = 1.1926) )
}

T6 <- function(x){
  return( sqrt(pi/2) * sum(abs(x-mean(x))) / length(x) )
}

#################################### Auxiliary Functions ####################################
simulate.size <- function(ns=1000, n=100, dist='norm'){
  sigma = 0
  
  T1.est = numeric(ns)
  T2.est = numeric(ns)
  T3.est = numeric(ns)
  T4.est = numeric(ns)
  T5.est = numeric(ns)
  T6.est = numeric(ns)
  
  set.seed(12345)
  
  for (i in 1:ns){
    if (dist == 'norm'){
      x = rnorm(n, 0, 1) # sample from standard norm
      sigma = 1
    }
    else if (dist == 't'){
      x = rt(n, df=3) # sample from t(3)
      sigma = sqrt(3)
    }
    else if (dist == 'chisq'){
      x = rchisq(n, df=3) # sample from chisq(3)
      sigma = sqrt(3*2)
    }
    else if (dist == 'exp'){
      x = rexp(n, rate=1) # sample from Exponential(1)
      sigma = 1
    }
    else if (dist == 'beta'){
      x = rbeta(n, shape1=0.5, shape2=0.5) # sample from Beta(0.5,0.5)
      sigma = 0.353553
    }
    else if (dist == 'disc_unif'){
      x = sample(disc_unif, n, replace=T) # sample from discrete uniform distribution
    }
    
    T1.est[i] = T1(x)
    T2.est[i] = T2(x)
    T3.est[i] = T3(x)
    T4.est[i] = T4(x)
    T5.est[i] = T5(x)
    T6.est[i] = T6(x)
  }
  
  simumean = apply(cbind(T1.est,T2.est,T3.est,T4.est,T5.est,T6.est), 2, mean)
  
  #compute bias
  simubias = simumean - rep(sigma,6)
  
  # compute standard deviation
  simustd = apply(cbind(T1.est,T2.est,T3.est,T4.est,T5.est,T6.est), 2, sd)
  
  #compute MSE
  simumse = simubias^2 + simustd^2
  
  # column heading
  ests = c("T1", "T2", "T3", "T4", "T5", "T6")
  # row heading
  names = c("True value", "No. of simu", "MC Mean", "MC Std Deviation", "MC Bias", "MC MSE")
  sumdat = rbind(sigma, ns, simumean, simustd, simubias, simumse)
  
  dimnames(sumdat) <- list(names, ests)
  
  cat("Simulation results for Simulation size =", ns, ", Sample size =", n, 
      ", Distribution =", dist, "\n")
  
  round(sumdat, 4)
  
  return(round(sumdat, 4))
}

####################################### Simulation ##########################################

ns = 1000 # simulation size
n = c(5, 10, 20, 30, 50, 100, 200) # sample size

norm.data = list()
t.data = list()
chisq.data = list()
exp.data = list()
beta.data = list()
disc_unif.data = list()

# normal 
for (i in seq(1,length(n))){
  norm.data[[i]] = simulate.size(ns=ns, n=n[i], dist='norm')
  t.data[[i]] = simulate.size(ns=ns, n=n[i], dist='t')
  chisq.data[[i]] = simulate.size(ns=ns, n=n[i], dist='chisq')
  exp.data[[i]] = simulate.size(ns=ns, n=n[i], dist='exp')
  beta.data[[i]] = simulate.size(ns=ns, n=n[i], dist='beta')
  disc_unif.data[[i]] = simulate.size(ns=ns, n=n[i], dist='disc_unif')
}

########################### visualise estimators for normal dist ##########################

norm.mean = matrix(numeric(6),length(n),6)
norm.sd = matrix(numeric(6),length(n),6)
norm.bias = matrix(numeric(6),length(n),6)
norm.mse = matrix(numeric(6),length(n),6)

for(i in seq(1, length(n))){
 norm.mean[i,] = norm.data[[i]]["MC Mean",]
 norm.sd[i,] = norm.data[[i]]["MC Std Deviation",]
 norm.bias[i,] = norm.data[[i]]["MC Bias",]
 norm.mse[i,] = norm.data[[i]]["MC MSE",]
}

rownames = c("Sample Size = 5", "Sample Size = 10", "Sample Size = 20", "Sample Size = 30",
             "Sample Size = 50", "Sample Size = 100", "Sample Size = 200")
colnames = c("T1", "T2", "T3", "T4", "T5", "T6")
dimnames(norm.mean) = list(rownames, colnames)
dimnames(norm.sd) = list(rownames, colnames)
dimnames(norm.bias) = list(rownames, colnames)
dimnames(norm.mse) = list(rownames, colnames)

# plot for mean
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", norm.mean, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "Mean", main="Mean of Estimators for Standard Deviation of  t(3)")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

# plot for standard deviations
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", norm.sd, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "Standard Deviation", main="Standard Deviation of Estimators for Standard Deviation of t(3)")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

# plot for bias
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", norm.bias, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "Bias", main="Bias of Estimators for Standard Deviation of N(0,1)")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

# plot for mse
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", norm.mse, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "MSE", main="MSE of Estimators for Standard Deviation of N(0,1)")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

############################## visualise estimators for t dist #############################

t.mean = matrix(numeric(6),length(n),6)
t.sd = matrix(numeric(6),length(n),6)
t.bias = matrix(numeric(6),length(n),6)
t.mse = matrix(numeric(6),length(n),6)

for(i in seq(1, length(n))){
  t.mean[i,] = t.data[[i]]["MC Mean",]
  t.sd[i,] = t.data[[i]]["MC Std Deviation",]
  t.bias[i,] = t.data[[i]]["MC Bias",]
  t.mse[i,] = t.data[[i]]["MC MSE",]
}

rownames = c("Sample Size = 5", "Sample Size = 10", "Sample Size = 20", "Sample Size = 30",
             "Sample Size = 50", "Sample Size = 100", "Sample Size = 200")
colnames = c("T1", "T2", "T3", "T4", "T5", "T6")
dimnames(t.mean) = list(rownames, colnames)
dimnames(t.sd) = list(rownames, colnames)
dimnames(t.bias) = list(rownames, colnames)
dimnames(t.mse) = list(rownames, colnames)

# plot for mean
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", t.mean, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "Mean", main="Mean of Estimators for Standard Deviation of t(3)")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

# plot for standard deviations
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", t.sd, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "Standard Deviation", main="Standard Deviation of Estimators for Standard Deviation of t(3)")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

# plot for bias
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", t.bias, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "Bias", main="Bias of Estimators for Standard Deviation of t(3)")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

# plot for mse
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", t.mse, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "MSE", main="MSE of Estimators for Standard Deviation of t(3)")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

############################ visualise estimators for chisq dist ###########################

chisq.mean = matrix(numeric(6),length(n),6)
chisq.sd = matrix(numeric(6),length(n),6)
chisq.bias = matrix(numeric(6),length(n),6)
chisq.mse = matrix(numeric(6),length(n),6)

for(i in seq(1, length(n))){
  chisq.mean[i,] = chisq.data[[i]]["MC Mean",]
  chisq.sd[i,] = chisq.data[[i]]["MC Std Deviation",]
  chisq.bias[i,] = chisq.data[[i]]["MC Bias",]
  chisq.mse[i,] = chisq.data[[i]]["MC MSE",]
}

rownames = c("Sample Size = 5", "Sample Size = 10", "Sample Size = 20", "Sample Size = 30",
             "Sample Size = 50", "Sample Size = 100", "Sample Size = 200")
colnames = c("T1", "T2", "T3", "T4", "T5", "T6")
dimnames(chisq.mean) = list(rownames, colnames)
dimnames(chisq.sd) = list(rownames, colnames)
dimnames(chisq.bias) = list(rownames, colnames)
dimnames(chisq.mse) = list(rownames, colnames)

# plot for mean
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", chisq.mean, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "Mean", main="Mean of Estimators for Standard Deviation of chisq(3)")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

# plot for standard deviations
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", chisq.sd, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "Standard Deviation", main="Standard Deviation of Estimators for Standard Deviation of chisq(3)")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

# plot for bias
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", chisq.bias, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "Bias", main="Bias of Estimators for Standard Deviation of chisq(3)")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

# plot for mse
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", chisq.mse, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "MSE", main="MSE of Estimators for Standard Deviation of chisq(3)")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

############################ visualise estimators for Exp dist ###########################

exp.mean = matrix(numeric(6),length(n),6)
exp.sd = matrix(numeric(6),length(n),6)
exp.bias = matrix(numeric(6),length(n),6)
exp.mse = matrix(numeric(6),length(n),6)

for(i in seq(1, length(n))){
  exp.mean[i,] = exp.data[[i]]["MC Mean",]
  exp.sd[i,] = exp.data[[i]]["MC Std Deviation",]
  exp.bias[i,] = exp.data[[i]]["MC Bias",]
  exp.mse[i,] = exp.data[[i]]["MC MSE",]
}

rownames = c("Sample Size = 5", "Sample Size = 10", "Sample Size = 20", "Sample Size = 30",
             "Sample Size = 50", "Sample Size = 100", "Sample Size = 200")
colnames = c("T1", "T2", "T3", "T4", "T5", "T6")
dimnames(exp.mean) = list(rownames, colnames)
dimnames(exp.sd) = list(rownames, colnames)
dimnames(exp.bias) = list(rownames, colnames)
dimnames(exp.mse) = list(rownames, colnames)

# plot for mean
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", exp.mean, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "Mean", main="Mean of Estimators for Standard Deviation of Exponential(1)")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

# plot for standard deviations
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", exp.sd, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "Standard Deviation", main="Standard Deviation of Estimators for Standard Deviation of Exponential(1)")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

# plot for bias
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", exp.bias, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "Bias", main="Bias of Estimators for Standard Deviation of Exponential(1)")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

# plot for mse
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", exp.mse, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "MSE", main="MSE of Estimators for Standard Deviation of Exponential(1)")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

############################ visualise estimators for Beta dist ###########################

beta.mean = matrix(numeric(6),length(n),6)
beta.sd = matrix(numeric(6),length(n),6)
beta.bias = matrix(numeric(6),length(n),6)
beta.mse = matrix(numeric(6),length(n),6)

for(i in seq(1, length(n))){
  beta.mean[i,] = beta.data[[i]]["MC Mean",]
  beta.sd[i,] = beta.data[[i]]["MC Std Deviation",]
  beta.bias[i,] = beta.data[[i]]["MC Bias",]
  beta.mse[i,] = beta.data[[i]]["MC MSE",]
}

rownames = c("Sample Size = 5", "Sample Size = 10", "Sample Size = 20", "Sample Size = 30",
             "Sample Size = 50", "Sample Size = 100", "Sample Size = 200")
colnames = c("T1", "T2", "T3", "T4", "T5", "T6")
dimnames(beta.mean) = list(rownames, colnames)
dimnames(beta.sd) = list(rownames, colnames)
dimnames(beta.bias) = list(rownames, colnames)
dimnames(beta.mse) = list(rownames, colnames)

# plot for mean
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", beta.mean, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "Mean", main="Mean of Estimators for Standard Deviation of Beta(0.5,0.5)")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

# plot for standard deviations
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", beta.sd, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "Standard Deviation", main="Standard Deviation of Estimators for Standard Deviation of Beta(0.5,0.5)")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

# plot for bias
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", beta.bias, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "Bias", main="Bias of Estimators for Standard Deviation of Beta(0.5,0.5)")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

# plot for mse
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", beta.mse, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "MSE", main="MSE of Estimators for Standard Deviation of Beta(0.5,0.5)")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

########################## visualise estimators for Disc Unif dist #########################

disc_unif.mean = matrix(numeric(6),length(n),6)
disc_unif.sd = matrix(numeric(6),length(n),6)
disc_unif.bias = matrix(numeric(6),length(n),6)
disc_unif.mse = matrix(numeric(6),length(n),6)

for(i in seq(1, length(n))){
  disc_unif.mean[i,] = disc_unif.data[[i]]["MC Mean",]
  disc_unif.sd[i,] = disc_unif.data[[i]]["MC Std Deviation",]
  disc_unif.bias[i,] = disc_unif.data[[i]]["MC Bias",]
  disc_unif.mse[i,] = disc_unif.data[[i]]["MC MSE",]
}

rownames = c("Sample Size = 5", "Sample Size = 10", "Sample Size = 20", "Sample Size = 30",
             "Sample Size = 50", "Sample Size = 100", "Sample Size = 200")
colnames = c("T1", "T2", "T3", "T4", "T5", "T6")
dimnames(disc_unif.mean) = list(rownames, colnames)
dimnames(disc_unif.sd) = list(rownames, colnames)
dimnames(disc_unif.bias) = list(rownames, colnames)
dimnames(disc_unif.mse) = list(rownames, colnames)

# plot for mean
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", disc_unif.mean, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "Mean", main="Mean of Estimators for Standard Deviation \n of Discrete Uniform Distribution")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

# plot for standard deviations
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", disc_unif.sd, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "Standard Deviation", main="Standard Deviation of Estimators for Standard Deviation \n of Discrete Uniform Distribution")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

# plot for bias
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", disc_unif.bias, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "Bias", main="Bias of Estimators for Standard Deviation \n of Discrete Uniform Distribution")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

# plot for mse
par(xpd = T, mar = par()$mar + c(0,0,0,4))
matplot(1:7, xaxt = "n", disc_unif.mse, type="o", lty=1, pch=1, col=1:7, xlab = "Sample size",
        ylab = "MSE", main="MSE of Estimators for Standard Deviation \n of Discrete Uniform Distribution")
axis(1, at = 1:7, labels = as.character(n))
legend("topright", xpd = T, inset=c(-0.15,0), legend = colnames, col=1:7, pch=1)
par(mar=c(5, 4, 4, 2) + 0.1)

