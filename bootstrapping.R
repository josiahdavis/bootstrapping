# Setup
x1 <- c(20, 22, 18, 34, 45, 86, 13, 100, 89, 22)
n1 <- length(x1)
thetaHat <- mean(x1)
B <- 100
Tboot <- rep(NA, B)

# Bootstrapping
for(i in 1:B){
  xx1 <- sample(x1, size = n1, replace = TRUE)
  Tboot[i] <- mean(xx1)
}

# Estimated SE and confidence intervals
seHat <- sqrt(var(Tboot))
Normal <- c(thetaHat + qnorm(.025)*seHat, thetaHat + qnorm(.975)*seHat)
percentile <- c(quantile(Tboot,.025), quantile(Tboot,.975))
pivotal <- c(2*thetaHat-quantile(Tboot,.975), 2*thetaHat-quantile(Tboot, .025))

# Plotting
par(mfrow = c(3, 1))
hist(Tboot, xlim = c(0, 80), main = 'Method #1: Normal')
abline(v = Normal)
hist(Tboot, xlim = c(0, 80), main = 'Method #2: Percentile')
abline(v = percentile)
hist(Tboot, xlim = c(0, 80), main = 'Method #3: Pivotal')
abline(v = pivotal)