
# true gamma
# staring parameters (sir, seir)
# starting population
# variance
# data


r0_estimator <- function(dat, start_pop, start_par, start_par_rep, gamma_true){
  mods <- 8
  results_mat <- data.frame(methods = c("EG", "RE", "rRE", "LL", "MC", "LBE", "IPR", "LMA"))
  res_est <- rep(NA, mods)
  res_sd <- rep(NA, mods)
  results_r0 <- list()
  
  source("19_wallinga.R")
  print("EG")
  results_r0$EG <- try(wallinga(start_pop, dat$I, gamma_true))
  if(!(class(results_r0$EG) == "try-error")){
    res_est[1] <- results_r0$EG$est
    res_sd[1] <- results_r0$EG$sd
  }
  
  source("5_SIR.R")
  print("RE")
  results_r0$RE <- try(SIRreg(start_par, start_pop, dat$Day, dat$I, dat$S, dat$R))
  if(!(class(results_r0$RE) == "try-error")){
    res_est[2] <- results_r0$RE$est
    res_sd[2] <- results_r0$RE$sd
  }
  
  source("5-2_SIRrepar.R")
  print("rRE")
  results_r0$rRE <- try(SIRrepar(start_par_rep, start_pop, dat$Day, dat$I, dat$S, dat$R))
  if(!(class(results_r0$rRE) == "try-error")){
    res_est[3] <- results_r0$rRE$est
    res_sd[3] <- results_r0$rRE$sd
  }
  
  source("6_Harko.R")
  print("LL")
  results_r0$LL <- try(harkoSIR(start_par, start_pop, dat$S, dat$I, dat$R))
  if(!(class(results_r0$rRE) == "try-error")){
    res_est[4] <- results_r0$LL$est
    res_sd[4] <- results_r0$LL$sd
  }
  
  source("5-6_SIRmc.R")
  print("MC")
  results_r0$MC <- try(SIRmc(start_pop, dat$I, dat$S))
  if(!(class(results_r0$MC) == "try-error")){
    res_est[5] <- results_r0$MC$est
    res_sd[5] <- results_r0$MC$sd
  }
  
  lbe_day <- c(1:nrow(dat))
  source("likelihood_shell.R")
  print("LBE")
  results_r0$LBE <- try(sir_likelihood(start_pop, dat$S, dat$I, lbe_day, start_par))
  if(!(class(results_r0$LBE) == "try-error")){
    res_est[6] <- results_r0$LBE$est
    res_sd[6] <- results_r0$LBE$sd
  }
  
  
  source("5-5_SIRipr.R")
  print("IPR")
  results_r0$IPR <- try(SIRipr(start_pop, dat$Day, dat$I, dat$S, gamma_true))
  if(!(class(results_r0$IPR) == "try-error")){
    res_est[7] <- results_r0$IPR$est
    res_sd[7] <- results_r0$IPR$sd
  }
  
  source("5-3_SIRlma.R")
  print("LMA")
  results_r0$LMA <- try(SIRlma(start_pop, dat$Day, dat$I, dat$S))
  if(!(class(results_r0$LMA) == "try-error")){
    res_est[8] <- results_r0$LMA$est
    res_sd[8] <- results_r0$LMA$sd
  }
  
  results_mat$estimate <- res_est
  results_mat$stddev <- res_sd
  return(results_mat)
}