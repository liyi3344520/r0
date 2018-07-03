

SIRmc <- function(init.pop, inf.data, sus.data, k = 100) {
  N <- sum(init.pop)
  sus.data <- c(init.pop["S"], sus.data)
  inf.data <- c(init.pop["I"], inf.data)
  resp <- (log(sus.data[2:(length(sus.data))]) - log(sus.data[1:(length(sus.data) - 1)]))
  pred <- inf.data[1:(length(sus.data) - 1)] / N
  reg <- lm(resp ~ pred - 1)
  alpha <- exp(coef(summary(reg))[1, 1])
  r0_est <- log(1 / (1 - alpha))
  alpha_se <- (coef(summary(reg))[1, 2])
  
  r0_sd <- (1 / (1 - alpha)) ^ 2 * alpha_se
  return(list(est = r0_est, sd = r0_sd))
}