#init.pop is a vector of the initial size of each compartment. It needs to be a vector that has elements named "S", "I", and "R"
#days is a vector of the day numbers of the data points. This allows for data without regular intervals between points.
###Must be a vector of numbers. 
#inf.data is the number of people in I for each day in the data. Must be a vector of numbers, same length as days.
#sus.data is the number of people in S for each day in the data. Must be a vector of numbers, same length as days.
#gamma is 1/infectivity period.

set.seed(1)
SIRipr <- function(init.pop, days, inf.data, sus.data, gam) {
  sus.data <- c(init.pop["S"], sus.data)
  inf.data <- c(init.pop["I"], inf.data)
  days <- c(0, days)
  n <- sum(init.pop)
  J <- -diff(sus.data, lag = 1) #incidence
  IPR <- J / inf.data[- length(days)]
  IPR <- ifelse(IPR < 0, 0, IPR)
  r0_est <- mean(IPR / gam)
  r0_sd <- sd(IPR / gam)
  return(list(est = r0_est, sd = r0_sd))
}

SIRsipr <- function(init.pop, days, inf.data, sus.data, gam) {
  sus.data <- c(init.pop["S"], sus.data)
  inf.data <- c(init.pop["I"], inf.data)
  days <- c(0, days)
  n <- sum(init.pop)
  modx <- smooth.spline(days, sus.data, df = 4)
  xhat <- predict(modx, seq(0, max(days), by = 1))
  mody <- smooth.spline(days, inf.data, df = 4)
  yhat <- predict(mody, seq(0, max(days), by = 1))
  J <- - diff(xhat$y, lag=1) #incidence
  IPR <- J / yhat$y[- length(days)]
  r0_est <- mean(IPR / gam)
  
  # Jackknife
  r0 <- numeric(length(days))
  for(ii in 1:length(r0)){
    modx <- smooth.spline(days[-ii], sus.data[-ii], df = 4)
    xhat <- predict(modx, seq(0, max(days), by = 1))
    mody <- smooth.spline(days[-ii], inf.data[-ii], df = 4)
    yhat <- predict(mody, seq(0, max(days), by = 1))
    J <- -diff(xhat$y, lag = 1) #incidence
    IPR <- J[1] / yhat$y[1]
    r0[ii] <- IPR / gam
  }
  r0_sd <- sqrt((n-1) / n * sum((r0 - mean(r0))^2))
  return(list(est = r0_est, sd = r0_sd))
}