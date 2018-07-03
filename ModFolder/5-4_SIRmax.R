#init.pop is a vector of the initial size of each compartment. It needs to be a vector that has elements named "S", "I", and "R"
#days is a vector of the day numbers of the data points. This allows for data without regular intervals between points.
###Must be a vector of numbers. 
#inf.data is the number of people in I for each day in the data. Must be a vector of numbers, same length as days.
#sus.data is the number of people in S for each day in the data. Must be a vector of numbers, same length as days.

SIRmax <- function(init.pop, days, inf.data, sus.data) {
  N <- sum(init.pop)
  tstar <- which.max(inf.data)
  r0_est <- N / sus.data[tstar]
  return(list(est = r0_est))
}

SIRsmax <- function(init.pop, days, inf.data, sus.data) {
  sus.data <- c(init.pop["S"], sus.data)
  inf.data <- c(init.pop["I"], inf.data)
  days <- c(0, days)
  N <- sum(init.pop)
  modx <- smooth.spline(days, sus.data, df = 4)
  xhat <- predict(modx, seq(0, max(days), by = .05))
  mody <- smooth.spline(days, inf.data, df = 4)
  yhat <- predict(mody, seq(0, max(days), by = .05))
  tstar <- which.max(yhat$y)
  r0_est <- N / xhat$y[tstar]
  
  # Jackknife
  r0 <- numeric(length(days))
  for(ii in 1:length(r0)){
    modx <- smooth.spline(days[-ii], sus.data[-ii], df = 4)
    xhat <- predict(modx, seq(0, max(days), by = .05))
    mody <- smooth.spline(days[-ii], inf.data[-ii], df = 4)
    yhat <- predict(mody, seq(0, max(days), by = .05))
    tstar <- which.max(yhat$y)
    r0[ii] <- N / xhat$y[tstar]
  }
  r0_sd <- sqrt((N-1) / N * sum((r0 - mean(r0))^2))
  return(list(est = r0_est, sd = r0_sd))
}