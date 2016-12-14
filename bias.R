# Setup
x <- c(20, 22, 18, 34, 45, 86, 13, 100, 89, 22)
n <- length(x)
thetaHat <- mean(x)
B <- 100
B2 <- 100
Tboot1 <- rep(NA, B)
Tboot2 <- rep(NA, B2)

# Bootstrapping
for(i in 1:B){
  
  # Create the first bootstrapped sample
  y <- sample(x, size = n, replace = TRUE)
  for(j in 1:B2){
    
    # Create the second bootstrapped sample from the first bootstrapped sample
    z <- sample(y, size = n, replace = TRUE)
    
    # Estimate the statistic from the second bootstrapped sample
    Tboot2[(i-1)*B2 + j] <- mean(z)
    
  }
  # Estimate the statistic from the first bootstrapped sample
  Tboot1[i] <- mean(y)
  
}

# Estimated the bias
tHat <- mean(Tboot1) - mean(Tboot2)

# The unbiased estimate
thetaTilde <- thetaHat + tHat