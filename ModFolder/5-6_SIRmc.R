
# 
# SIRmc <- function(init.pop, inf.data, sus.data, k = 100) {
#   N <- sum(init.pop)
#   sus.data <- c(init.pop["S"], sus.data)
#   inf.data <- c(init.pop["I"], inf.data)
#   resp <- exp((log(pmax(sus.data[2:(length(sus.data))], 0.001)) - log(pmax(sus.data[1:(length(sus.data) - 1)]), 0.001)) /
#     inf.data[1:(length(sus.data) - 1)])
#   reg <- glm(resp ~ 1)
#   g <- coef(summary(reg))[1, 1]
#   alpha <- exp(g) / (1-exp(g))
#   r0_est <- -log((1 - exp(g)) / (1 - 2 * exp(g)))
#   g_se <- (coef(summary(reg))[1, 2])
#   alpha_se <- (exp(g) / (1 - exp(g)) ^ 2) ^ 2 * g_se
#   r0_sd <- (exp(g) / (-3*exp(g) + 2 * exp(2 * g) + 1)) ^ 2 * g_se
#   return(list(est = r0_est, sd = r0_sd, output = summary(reg)))
# }


SIRmc <- function(init.pop, inf.data, sus.data, k = 100) {
  N <- sum(init.pop)
  sus.data <- pmax(c(init.pop["S"], sus.data), 0.001)
  inf.data <- pmax(c(init.pop["I"], inf.data), 0.001)
  
  opt_func <- function(alpha, ss, ii){
    ll <- numeric(length(ss) - 1)
    diff_S <- pmin(-0.1, diff(ss))
    new_I <- 0.1
    for(jj in 1:length(ll)){
          p_avoid <- (1 - alpha)^(new_I)
          p_inf <- 1 - p_avoid
          ll[jj] <- -diff_S[jj] * log(p_inf) + ss[jj + 1] * log(p_avoid)
          new_I <- pmax(-diff_S[jj], 0.1)
        }
    # ll <- sum(ss[-1] * log((alpha)^(ii[-(length(ii))])) + 
    #             (ss[-(length(ss))] - ss[-1]) * log(1 - (alpha)^(ii[-(length(ii))])))
    # print(ll)
    return(sum(ll[- which(is.infinite(ll))], na.rm = TRUE))
  }
  
  
  # loglike <- function(alpha, S, I){
  #   
  #   loglike <- numeric(length(S) - 1)
  #   diff_S <- diff(S) # new infections
  #   new_I <- I[1]
  #   for(ii in 1:length(loglike)){
  #     p_avoid <- (1 - alpha)^(new_I)
  #     p_inf <- 1 - p_avoid
  #     loglike[ii] <- -diff_S[ii] * log(p_inf) + S[ii + 1] * log(p_avoid)
  #     new_I <- - diff_S[ii]
  #   }
  #   return(sum(loglike))
  #   
  # }
  
  aa <- optimize(opt_func, c(0.001, 0.999), ss = sus.data, ii = inf.data, maximum = TRUE)
  r0_est <- 1/(1 - aa$maximum)
  aa_plus <- min(aa$maximum + 0.001, 0.999)
  aa_minus <- max(aa$maximum - 0.001, 0.001)
  
  ll_plus <- sum(inf.data[-(length(inf.data))] * sus.data[-(length(sus.data))] * inf.data[-(length(inf.data))] * log(aa_plus) + 
                   (sus.data[-(length(sus.data))] - sus.data[-1]) * log(1-aa_plus^(inf.data[-(length(inf.data))])))
  ll_minus <- sum(inf.data[-(length(inf.data))] * sus.data[-(length(sus.data))] * inf.data[-(length(inf.data))] * log(aa_minus) + 
                    (sus.data[-(length(sus.data))] - sus.data[-1]) * log(1-aa_minus^(inf.data[-(length(inf.data))])))
  ddll <- abs((ll_plus - ll_minus) / (aa_plus - aa_minus) / aa$objective)
  dh <- 1/(1-aa$maximum)
  r0_sd <- sqrt(dh^2 * ddll^(-1))
  return(list(est = r0_est, sd = r0_sd))
}