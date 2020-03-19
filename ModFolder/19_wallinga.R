
wallinga <- function(start_pop = c("S" = 99950, "E" = 0, "I" = 50, "R" = 0), 
                     inf_data, gam){
  # log_it <- log(pmax(inf_data, 0.1))
  # log_istart <- log(start_pop["I"])
  yy <- pmax(0.0001, inf_data / start_pop["I"])
  time_stop <- which.max(yy)
  tt <- c(1:time_stop)
  yy <- yy[1:time_stop]
  print(length(tt))
  lm_r <- glm(formula = yy ~ tt - 1,  family = Gamma)
  r0 <- 1 + summary(lm_r, dispersion = 1)$coefficients[1, 1] / gam
  r0_sd <- summary(lm_r, dispersion = 1)$coefficients[1, 2] / gam
  
  return(list(est = r0, sd = r0_sd))
}