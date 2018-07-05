

SIRmc <- function(init.pop, inf.data, sus.data, k = 100) {
  N <- sum(init.pop)
  sus.data <- c(init.pop["S"], sus.data)
  inf.data <- c(init.pop["I"], inf.data)
  resp <- exp((log(sus.data[2:(length(sus.data))]) - log(sus.data[1:(length(sus.data) - 1)])) /
    inf.data[1:(length(sus.data) - 1)])
  reg <- glm(resp ~ 1)
  g <- coef(summary(reg))[1, 1]
  alpha <- exp(g) / (1-exp(g))
  r0_est <- -log((1 - exp(g)) / (1 - 2 * exp(g)))
  g_se <- (coef(summary(reg))[1, 2])
  alpha_se <- (exp(g) / (1 - exp(g)) ^ 2) ^ 2 * g_se
  r0_sd <- (exp(g) / (-3*exp(g) + 2 * exp(2 * g) + 1)) ^ 2 * g_se
  return(list(est = r0_est, sd = r0_sd, output = summary(reg)))
}