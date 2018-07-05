
wallinga <- function(start_pop = c("S" = 99950, "E" = 0, "I" = 50, "R" = 0), 
                     inf_data, gam){
  log_it <- log(pmax(inf_data, 0.1))
  log_istart <- log(start_pop["I"])
  y <- log_it - log_istart
  tt <- 1:length(inf_data)
  lm_r <- lm(y ~ tt - 1)
  r0 <- 1 + coef(lm_r)[1] / gam
  r0_sd <- summary(lm_r)$coef[1, 2] / gam
  
  return(list(est = r0, sd = r0_sd))
}