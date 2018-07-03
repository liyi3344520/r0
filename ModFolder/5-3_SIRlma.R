#Load package
library("EpiModel")
library(deSolve)

#Remember to change desired directory 

#####Arguments:
#days is a vector of the day numbers of the data points. This allows for data without regular intervals between points.
###Must be a vector of numbers. 
#init.pop is a vector of the initial size of each compartment. It needs to be a vector that has elements named "S", "I", and 
#inf.data is the number of people in I for each day in the data. Must be a vector of numbers, same length as days.
#sus.data is the number of people in S for each day in the data. Must be a vector of numbers, same length as days.
#k is the number of degress for the polynomial regression.

poly_deriv <- function(t, mod){
  deriv_coefs <- as.numeric(mod$coef[-1])
  deriv_coefs <- ifelse(is.na(deriv_coefs), 0, deriv_coefs)
  k <- length(deriv_coefs)
  if( k == 1 ) {return(deriv_coefs)}
  else if ( k < 1){ return(0)}
  poly_mat <- cbind(rep(1, length(t)), poly(t, k-1, raw = TRUE))
  deriv_val <- poly_mat %*% (deriv_coefs * 1:k)
  return(deriv_val)
}

SIRlma <- function(init.pop, days, inf.data, sus.data, k = 10) {
  #Get initial populations of each compartment S, I, R.
  init.sus <- init.pop["S"]
  init.inf <- init.pop["I"]
  init.rem <- init.pop["R"]
  inf.data <- c(init.inf, inf.data)
  sus.data <- c(init.sus, sus.data)
  days <- c(0, days)
  df <- data.frame(days = days, inf.data = inf.data, sus.data = sus.data)
  
  #Get total number of people in population
  #Assume population stays constant except for deaths by disease
  N <-sum(init.pop)
  n <- N

  y_mod <- lm(inf.data ~ poly(days, k, raw = TRUE), data = df)
  y_hat <- predict(y_mod, df)
  x_mod <- lm(sus.data ~ poly(days, k, raw = TRUE), data = df)
  x_hat <- predict(x_mod, df)
  dxt <- poly_deriv(days, x_mod)
  dyt <- poly_deriv(days, y_mod)
  xt <- x_hat
  r0_est <- (dxt[1] * N) / ((dyt[1] + dxt[1]) * xt[1])
  
  r01 <- numeric(length(days))
  for(ii in 1:length(r01)){
    y_mod <- lm(inf.data ~ poly(days, k, raw = TRUE), data = df[-ii,])
    y_hat <- predict(y_mod, df[-ii,])
    x_mod <- lm(sus.data ~ poly(days, k, raw = TRUE), data = df[-ii,])
    x_hat <- predict(x_mod, df[-ii,])
    dxt <- poly_deriv(days, x_mod)
    dyt <- poly_deriv(days, y_mod)
    xt <- x_hat
    r01[ii] <- (dxt[1] * N) / ((dyt[1] + dxt[1]) * xt[1])
  }
  r01var <- (n-1) / n * sum((r01 - mean(r01))^2)
  r0_sd <- sqrt(r01var)
  
  return(list(est = r0_est, sd = r0_sd))
}

SIRlmat <- function(init.pop, days, inf.data, sus.data, k = 10) {
  #Get initial populations of each compartment S, I, R.
  init.sus <- init.pop["S"]
  init.inf <- init.pop["I"]
  init.rem <- init.pop["R"]
  inf.data <- c(init.inf, inf.data)
  sus.data <- c(init.sus, sus.data)
  days <- c(0, days)
  df <- data.frame(days = days, inf.data = inf.data, sus.data = sus.data)
  
  #Get total number of people in population
  #Assume population stays constant except for deaths by disease
  N <-sum(init.pop)
  n <- N
  
  y_mod <- lm(inf.data ~ poly(days, k, raw = TRUE), data = df)
  y_hat <- predict(y_mod, df)
  x_mod <- lm(sus.data ~ poly(days, k, raw = TRUE), data = df)
  x_hat <- predict(x_mod, df)
  dxt <- poly_deriv(days, x_mod)
  dyt <- poly_deriv(days, y_mod)
  xt <- x_hat
  r0_est <- mean((dxt  / (dyt + dxt)) )* N / xt[1]
  r0_sd <- sd((dxt  / (dyt + dxt)) )* N / xt[1]
  
  return(list(est = r0_est, sd = r0_sd))
}

