setwd("C:/Users/Andersen/Documents/R0/Models")
library(knitr)
library(ggplot2)

### FIle path to folder where the data are located
data_path <- "C:/Users/Andersen/Documents/R0/DataSim/NewPar2/Data/"


### Character vector of all files within data_path
files_list <- paste0(data_path, "/", 
                     list.files(data_path, recursive = TRUE))
results_matlist <- list()

xx <- files_list
### Test dataset
for (jj in 1:length(xx)) {
# for (jj in 21:24) {
  dat <- read.csv(xx[jj])[,-1]
  start_pop <- c("S" = 99950, "E" = 0, "I" = 50, "R" = 0)
  start_par_sir <- c("beta" = 0.002, "gamma" = 0.007)
  start_par_rep <- c("r0" = 2, "gamma" = 0.007)
  start_par_seir <- c("beta" = 0.008, "gamma" = 0.007, "alpha" = 0.0014)
  
  # Surv func
  bxf <- function(t) {return(dexp(t / 2) * dnorm(t, 5, 3) * 10)}
  b_dis <- rnorm(11, 0.6, 0.1)
  f_dis <- dexp(0:10)
  max_surv <- 100
  
  # Direct
  latent_per <- 7
  inf_per <- 20
  latent_inf_per <- latent_per + inf_per
  k <- 2
  b <- 0.1
  d <- 14
  
  # Direct survival
  cont_inf <- function(t) {return(dexp(t / 1.5))}
  dis_inf <- rbeta(11, 0.2, 1)
  
  # Attack Rate
  ar <- (start_pop["S"] - dat[nrow(dat), 2]) / sum(dat[nrow(dat), -1])
  
  # GTD
  pw <- c(dunif(10:30, 10, 30))
  
  
  # Final size
  fs <- dat[nrow(dat), 2] / sum(dat[nrow(dat), -1])
  
  
  results_r0 <- list()
  print(results_r0)
  # ##### Survival function #####
  # source("2_Survival.R")
  # # Continuous test
  # results_r0$survival_cont <- try(cont_surv_func(bxf, max_surv))
  # # Discretized test
  # results_r0$survival_dis <- try(disc_surv_func(b_dis, f_dis))
  # 
  # 
  # ##### Direct parameter estimation #####
  # source("3_Direct.R")
  # dat2 <- dat
  # dat2[,2:4] <- dat[,2:4] / start_pop["I"]
  # results_r0$dpe <- try(exp_dpe(dat[100,"I"], dat[100,"S"] / 2000, 100, latent_inf_per, latent_per))
  # 
  # results_r0$pc <- try(plug_chug(k, b, d))
  # 
  # 
  # ##### Direct survival function #####
  # ##### Von Forester Equations #####
  # source("4_SurvFunc.R")
  # # Continuous test
  # results_r0$dirsf_cont <- try(cont_dsf(start_pop, cont_inf))
  # # Discretized test
  # results_r0$dirsf_dis <- try(discrete_dsf(start_pop, dis_inf))
  
  
  ##### SIR Model #####
  source("5_SIR.R")
  print("5")
  results_r0$sir <- try(SIRreg(start_par_sir, start_pop, dat[, 1], dat[, 3], dat[, 2]))
  
  source("5-2_SIRrepar.R")
  print("5")
  results_r0$sirrepar <- try(SIRrepar(start_par_rep, start_pop, dat[, 1], dat[, 3], dat[, 2]))
  
  source("5-3_SIRlma.R")
  print("5")
  results_r0$sirlma <- try(SIRlma(start_pop, dat[, 1], dat[, 3], dat[, 2]))
  results_r0$sirlmat <- try(SIRlmat(start_pop, dat[, 1], dat[, 3], dat[, 2]))
  
  source("5-4_SIRmax.R")
  print("5")
  results_r0$sirmax <- try(SIRmax(start_pop, dat[, 1], dat[, 3], dat[, 2]))
  results_r0$sirsmax <- try(SIRsmax(start_pop, dat[, 1], dat[, 3], dat[, 2]))
  
  source("5-5_SIRipr.R")
  print("5")
  results_r0$siripr <- try(SIRipr(start_pop, dat[, 1], dat[, 3], dat[, 2], 1/inf_per))
  results_r0$sirsipr <- try(SIRsipr(start_pop, dat[, 1], dat[, 3], dat[, 2], 1/inf_per))
  
  source("5-6_SIRmc.R")
  print("5")
  results_r0$sirmc <- try(SIRmc(start_pop, dat[, 3], dat[, 2]))
  
  ##### Harko SIR #####
  source("6_Harko.R")
  print("6")
  results_r0$harko <- try(harkoSIR(start_par_sir, start_pop, dat[, 2], dat[, 3], dat[, 4]))
  
  
  ##### SEIR #####
  source("7_SEIR.R")
  print("7")
  results_r0$seir <- try(seir_gls(start_par_seir,start_pop, dat[, 2], dat[, 3], 0))
  
  
  ##### Attack rate #####
  source("8_AR.R")
  print("8")
  results_r0$ar <- try(R0_attack_rate(ar, start_pop))
  
  
  ##### Other compartments? (May be changed) #####
  source("9_Comp.R")
  print("9")
  results_r0$comp <- try(comp_gls(dat[, 3], sum(start_pop), dat[, 1], start_pop = c("M" = 0, "E" = 0, "I" = 50, "R" = 0)))
  
  
  ##### Next generation method #####
  source("11_NextGen.R")
  print("11")
  results_r0$nextgen <- try(seir_ng(start_par_seir, start_pop, dat[, 2], dat[, 3]))
  
  
  ##### Branching #####
  source("12_Branching.R")
  print("12")
  results_r0$branch <- try(likelihood_branching(start_pop, dat[, 2], dat[, 3], pw))
  
  
#   #### Amplifier Model #####
#   source("13_Blower.R")
#   print("13")
#   results_r0$chareq <- try(seir_blower(start_pop = start_pop, inf_data = dat[, 3], days = dat[, 1]))
  
  
  ##### Exponential growth #####
  source("14_ExpGrowth.R")
  print("14")
  results_r0$expgrowth <- try(exp_growth(start_pop, dat[, 2], dat[, 3], pw))
  
  
  ##### Maximum likelihood #####
  source("15_MLE.R")
  print("15")
  results_r0$mle <- try(mle_secondary(start_pop, dat[, 2], dat[, 3], pw))
  
  
  ##### Sequential Bayes (SIR) #####
  source("16_SeqBayes.R")
  print("16")
  results_r0$seqbayes <- try(seq_bayes_gamma(start_pop, dat[, 2], dat[, 3], 1 / inf_per))
  
  
  ##### Time dependence #####
# #  source("17_TimeDep.R")
# #   print("17")
# #   results_r0$timedep <- try(td_rep(start_pop, dat[, 2], dat[, 3], pw))
  
  
  ##### Initial growth rate; final size #####
  source("18_InitFinal.R")
  print("18")
  results_r0$initgrowth <- try(init_growth(start_pop, inf_per, dat[, 2], dat[, 3]))
  results_r0$finalsize <- try(final_size(fs))
  
  
  
  results_mat <- data.frame(NA, nrow = length(results_r0), ncol = 3)
  colnames(results_mat) <- c("Model", "Estimate", "Std. Dev")
  for (ii in 1:length(results_r0)) {
    results_mat[ii, "Model"] <- names(results_r0)[ii]
    if(class(results_r0[[ii]]) == "try-error") {
      print(paste0("Error in ", names(results_r0)[ii], ":"))
      print(attr(results_r0[[ii]], "condition"))
      next
    }
    results_mat[ii, "Estimate"] <- results_r0[[ii]]$est
    results_mat[ii, "Std. Dev"] <- ifelse(is.null(results_r0[[ii]]$sd), NA, results_r0[[ii]]$sd)
  }
  results_matlist[[jj]] <- results_mat
}

