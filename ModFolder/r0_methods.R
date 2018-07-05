setwd("C:/Users/Andersen/Documents/R0/Models")
library(knitr)
library(ggplot2)

### File path to folder where the data are located
data_path <- "C:/Users/Andersen/Documents/R0/DataSim/PaperData/Baseline"
# data_path <- "C:/Users/Andersen/Dropbox/r0_refs/Data/H1N12009"

### Character vector of all files within data_path
files_list <- paste0(data_path, "/", 
                     list.files(data_path, recursive = TRUE))

results_matlist<- list()
output_lists <- list()

# xx <- files_list

xx <- files_list

### Test dataset
for (jj in 1:length(xx)) {
# for (jj in 5:20) {
  print(xx[jj])
  # r0_true <- as.numeric(strsplit(xx[[jj]], split = "_")[[1]][12]) / 
  #   as.numeric(strsplit(xx[[jj]], split = "_")[[1]][13])
  # beta_true <- as.numeric(strsplit(xx[[jj]], split = "_")[[1]][12])
  # gamma_true <- as.numeric(strsplit(xx[[jj]], split = "_")[[1]][11])
  gamma_true <- 0.03
  # output_list <- list()
  # sink(zz, append = TRUE)
  # sink(zz, type = "message", append = TRUE)
  # print("------------------------------------------")
  # print("------------------------------------------")
  # print("------------------------------------------")
  # print("------------------------------------------")
  # print("------------------------------------------")
  # print((paste0("Beta = ", beta_true, ", Gamma = ", gamma_true, ", R0 = ", r0_true)))
  # print("")
  # print("")
  dat <- read.csv(xx[jj])
  # start_pop <- c("S" = as.numeric(strsplit(strsplit(xx[jj], "/")[[1]][10], "_")[[1]][1]), "E" = 0,
  #                "I" = as.numeric(strsplit(strsplit(xx[jj], "/")[[1]][10], "_")[[1]][2]), "R" = 0)
  
  start_pop <- c("S" = 99950, "E" = 0,
                 "I" = 50, "R" = 0)
  start_par_sir <- c("beta" = 0.02, "gamma" = 0.08)
  start_par_rep <- c("r0" = 2, "gamma" = 0.007)
  start_par_seir <- c("beta" = 0.02, "gamma" = 0.07, "alpha" = 0.014)
  
  # Surv func
  bxf <- function(t) {return(dexp(t / 2) * dnorm(t, 5, 3) * 10)}
  b_dis <- rnorm(11, 0.6, 0.1)
  f_dis <- dexp(0:10)
  max_surv <- 100
  
  # Direct
  latent_per <- 7
  inf_per <- 33
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
  pw <- c(dunif(1:4, 1, 5))
  
  
  # Final size
  fs <- dat[nrow(dat), 2] / sum(dat[nrow(dat), -1])
  
  
  results_r0 <- list()
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
  print("SIR_LS")
  results_r0$sirls <- try(SIRreg(start_par_sir, start_pop, dat[, 1], dat[, 3], dat[, 2]))
  # if(results_r0$sirls$est < 0 ||
  #    results_r0$sirls$sd < 0 ||
  #    is.na(results_r0$sirls$est < 0) ||
  #    is.na(results_r0$sirls$sd < 0) ||
  #    abs(results_r0$sirls$est - r0_true) > 4 * results_r0$sirls$sd) {
  #    print(results_r0$sirls$output)
  #   }

source("5-2_SIRrepar.R")
print("SIR Repar")
results_r0$sirrepar <- try(SIRrepar(start_par_rep, start_pop, dat[, 1], dat[, 3], dat[, 2]))
  # if(results_r0$sirrepar$est < 0 ||
  #    results_r0$sirrepar$sd < 0 ||
  #    is.na(results_r0$sirrepar$est < 0) ||
  #    is.na(results_r0$sirrepar$sd < 0) ||
  #    abs(results_r0$sirrepar$est - r0_true) > 4 * results_r0$sirrepar$sd) {
  #   print(results_r0$sirrepar$output)
  # }
  #
  source("5-3_SIRlma.R")
  print("SIR LMA")
  results_r0$sirlma <- try(SIRlma(start_pop, dat[, 1], dat[, 3], dat[, 2]))
#   if(results_r0$sirlma$est < 0 ||
#      results_r0$sirlma$sd < 0 ||
#      is.na(results_r0$sirlma$est < 0) ||
#      is.na(results_r0$sirlma$sd < 0) ||
#      abs(results_r0$sirlma$est - r0_true) > 4 * results_r0$sirlma$sd) {
#     print(results_r0$sirlma$output)
#   }
  print("SIR LMAT")
  results_r0$sirlmat <- try(SIRlmat(start_pop, dat[, 1], dat[, 3], dat[, 2]))
#   if(results_r0$sirlmat$est < 0 ||
#      results_r0$sirlmat$sd < 0 ||
#      is.na(results_r0$sirlmat$est < 0) ||
#      is.na(results_r0$sirlmat$sd < 0) ||
#      abs(results_r0$sirlmat$est - r0_true) > 4 * results_r0$sirlmat$sd) {
#     print(results_r0$sirlmat$output)
#   }
# #   
# #   # source("5-4_SIRmax.R")
# #   # print("5")
# #   # results_r0$sirmax <- try(SIRmax(start_pop, dat[, 1], dat[, 3], dat[, 2]))
# #   # results_r0$sirsmax <- try(SIRsmax(start_pop, dat[, 1], dat[, 3], dat[, 2]))
# #   
  source("5-5_SIRipr.R")
  print("SIR IPR")
  results_r0$siripr <- try(SIRipr(start_pop, dat[, 1], dat[, 3], dat[, 2], gamma_true))
#   if(results_r0$siripr$est < 0 ||
#      results_r0$siripr$sd < 0 ||
#      is.na(results_r0$siripr$est < 0) ||
#      is.na(results_r0$siripr$sd < 0) ||
#      abs(results_r0$siripr$est - r0_true) > 4 * results_r0$siripr$sd) {
#     print(results_r0$siripr$output)
#   }
  print("SIR SIPR")
  results_r0$sirsipr <- try(SIRsipr(start_pop, dat[, 1], dat[, 3], dat[, 2], gamma_true))
#   if(results_r0$sirsipr$est < 0 ||
#      results_r0$sirsipr$sd < 0 ||
#      is.na(results_r0$sirsipr$est < 0) ||
#      is.na(results_r0$sirsipr$sd < 0) ||
#      abs(results_r0$sirsipr$est - r0_true) > 4 * results_r0$sirsipr$sd) {
#     print(results_r0$sirsipr$output)
#   }
#
#   
  ##### Harko SIR #####
  source("6_Harko.R")
  print("HARKO")
  results_r0$loglinear <- try(harkoSIR(start_par_sir, start_pop, dat[, 2], dat[, 3], dat[, 4]))

  source("5-6_SIRmc.R")
  print("SIR MC")
  results_r0$sirmc <- try(SIRmc(start_pop, dat[, 3], dat[, 2]))
# #   if(results_r0$sirmc$est < 0 || 
# #      results_r0$sirmc$sd < 0 || 
# #      is.na(results_r0$sirmc$est < 0) ||
# #      is.na(results_r0$sirmc$sd < 0) ||
# #      abs(results_r0$sirmc$est - r0_true) > 4 * results_r0$sirmc$sd) {
# #     print(results_r0$sirmc$output)
# #   }
# #   
# #   if(results_r0$harko$est < 0 || 
# #      results_r0$harko$sd < 0 || 
# #      is.na(results_r0$harko$est < 0) ||
# #      is.na(results_r0$harko$sd < 0) ||
# #      abs(results_r0$harko$est - r0_true) > 4 * results_r0$harko$sd) {
# #     print(results_r0$harko$output)
# #   }
# #   
# # #   ##### SEIR #####
# # #   source("7_SEIR.R")
# # #   print("7")
# # #   results_r0$seir <- try(seir_gls(start_par_seir,start_pop, dat[, 2], dat[, 3], 0))
# # #   
# # #   
# # #   ##### Attack rate #####
# # #   source("8_AR.R")
# # #   print("8")
# # #   results_r0$ar <- try(R0_attack_rate(ar, start_pop))
# # #   
# # #   
# # #   ##### Other compartments? (May be changed) #####
# # #   source("9_Comp.R")
# # #   print("9")
# # #   results_r0$comp <- try(comp_gls(dat[, 3], sum(start_pop), dat[, 1], start_pop = c("M" = 0, "E" = 0, "I" = 50, "R" = 0)))
# # #   
# # #   
# # #   ##### Next generation method #####
# # #   source("11_NextGen.R")
# # #   print("11")
# # #   results_r0$nextgen <- try(seir_ng(start_par_seir, start_pop, dat[, 2], dat[, 3]))
# # #   
# # #   
# # #   ##### Branching #####
# # #   source("12_Branching.R")
# # #   print("12")
# # #   results_r0$branch <- try(likelihood_branching(start_pop, dat[, 2], dat[, 3], pw))
# # #   
# # #   
# # # #   #### Amplifier Model #####
# # # #   source("13_Blower.R")
# # # #   print("13")
# # # #   results_r0$chareq <- try(seir_blower(start_pop = start_pop, inf_data = dat[, 3], days = dat[, 1]))
# # #   
# # #   
  # ##### Exponential growth #####
  # source("14_ExpGrowth.R")
  # print("14")
  # results_r0$expgrowth <- try(exp_growth(start_pop, dat[, 2], dat[, 3], pw))

# # #   
# # #   ##### Maximum likelihood #####
# # #   source("15_MLE.R")
# # #   print("15")
# # #   results_r0$mle <- try(mle_secondary(start_pop, dat[, 2], dat[, 3], pw))
# # #   
# # #   
  ##### Sequential Bayes (SIR) #####
  source("16_SeqBayes.R")
  print("Sequential Bayes")
  results_r0$seqbayes <- try(seq_bayes_gamma(start_pop, dat[, 2], dat[, 3], 1 / inf_per))
#   
#   
#   ##### Time dependence #####
# # #  source("17_TimeDep.R")
# # #   print("17")
# # #   results_r0$timedep <- try(td_rep(start_pop, dat[, 2], dat[, 3], pw))
#   
#   
#   ##### Initial growth rate; final size #####
#   source("18_InitFinal.R")
#   print("18")
#   results_r0$initgrowth <- try(init_growth(start_pop, inf_per, dat[, 2], dat[, 3]))
#   results_r0$finalsize <- try(final_size(fs))
#   
  
  ##### Exponential growth #####
  # source("14_ExpGrowth.R")
  # print("14")
  # results_r0$expgrowth <- try(exp_growth(start_pop, dat[, 2], dat[, 3], pw))
  # 
  # source("19_wallinga.R")
  # print("19")
  # results_r0$IDEA <- try(wallinga(start_pop, dat[, 2], gamma_true))
  # 
  
  results_mat <- data.frame(NA, nrow = length(results_r0), ncol = 3)
  colnames(results_mat) <- c("Model", "Estimate", "Std. Dev")
  for (ii in 1:length(results_r0)) {
    results_mat[ii, "Model"] <- names(results_r0)[ii]
    # output_list[[names(results_r0)[ii]]]$model <- names(results_r0)[ii]
    # output_list[[names(results_r0)[ii]]]$output <- results_r0[[ii]]$output
    if(class(results_r0[[ii]]) == "try-error") {
      # print(paste0("Error in ", names(results_r0)[ii], ":"))
      # print(attr(results_r0[[ii]], "condition"))
      next
    }
    results_mat[ii, "Estimate"] <- results_r0[[ii]]$est
    results_mat[ii, "Std. Dev"] <- ifelse(is.null(results_r0[[ii]]$sd), NA, results_r0[[ii]]$sd)
  }
  results_matlist[[jj]] <- results_mat
  # output_lists[[jj]] <- output_list
  # sink(NULL, type = "message")
  # sink(NULL)
}


# x <- results_matlist[[1]]
# x$xloc <- 1:nrow(x)
# x$upper <- x$Estimate + 2 * x$`Std. Dev`
# x$lower <- x$Estimate - 2 * x$`Std. Dev`
# 
# ggplot(x) + 
#   geom_text(aes(x = xloc, y = Estimate - 0.01, label = Model)) +
#   geom_point(aes(x = xloc, y = Estimate, label = Model),
#              alpha = 0.5) +
#   geom_errorbar(aes(x = xloc, ymin = lower, ymax = upper)) + 
#   scale_x_continuous() +
#   scale_y_continuous(limits = c(1, 2.5)) 
#   theme_bw() +
#   labs(y = "Estimate",
#        title = "R0 Estimates",
#        x = "")

# errcomp <- list()
# yy <- matrix(c(1:8, 25:28, 9:12, 21:24, 29:36, 13:20),
#              ncol = 4, byrow = TRUE)
# for (ii in 1:length(results_matlistpop1[[1]]$Model)) {
#   errcomp_mat <- matrix(NA, ncol = 8, nrow = 0)
#   for (jj in 1:nrow(yy)) {
#     newrow <- c()
#     for(kk in yy[jj,]) {
#       newrow <- c(newrow, results_matlistpop1[[kk]][ii, 2], results_matlistpop1[[kk]][ii, 3])
#     }
#     errcomp_mat <- rbind(errcomp_mat, round(newrow, 6))
#   }
#   err_names <- c("$sigma_S = 10, sigma_I = 1$", "$sigma_S = 100, sigma_I = 10$", 
#                   "$sigma_S = 500, sigma_I = 50$", "$sigma_S = 1000, sigma_I = 100$", "$sigma_S = 2000, sigma_I = 200$",
#                  "$sigma_S = 5000, sigma_I = 500$", "$sigma_S = 7500, sigma_I = 750$", 
#                  "$sigma_S = 10000, sigma_I = 1000$", "$sigma_S = 15000, sigma_I = 1500$")
#   errcomp_mat <- data.frame(err_names, errcomp_mat)
#   colnames(errcomp_mat) <- c("Error", "Auto Est", "Auto SE", 
#                              "AutoM Est", "AutoM SE",
#                              "Norm Est", "Norm SE",
#                              "NormM Est", "NormM SE")
#   errcomp[[ii]] <- errcomp_mat
#   s0  <- strsplit(strsplit(files_list[1], "_")[[1]][1], "/")
#   cap = paste0("$R_0$ Estimates and Std. Errs, ", toupper(results_matlistpop1[[1]]$Model[ii]), " Model, \n Different Variances, ",
#                "$S_0 = ", s0[[1]][length(s0[[1]])], "$, $I_0 = ", 
#                100000 - as.numeric(s0[[1]][length(s0[[1]])]), "$")
#   cap = paste0("$R_0$ Estimates and Std. Errs, ", toupper(results_matlistpop1[[1]]$Model[ii]), " Model, \n Different Variances, ",
#                "$S_0 = ", s0[[1]][length(s0[[1]])], "$, $I_0 = ", 
#                100000 - as.numeric(s0[[1]][length(s0[[1]])]), "$")
#   print(kable(errcomp_mat, caption = cap, 
#               format = 'latex'))
# }
# names(errcomp) <- results_matlistpop1[[1]]$Model


errcomp <- list()
yy <- matrix(c(1:4),
             ncol = 4, byrow = TRUE)
# yy <- matrix(c(9:12, 13:16, 5:8, 1:4),
#                      ncol = 4, byrow = TRUE)
for (ii in 1:length(results_matlist[[1]]$Model)) {
  errcomp_mat <- matrix(NA, ncol = 8, nrow = 0)
  for (jj in 1:nrow(yy)) {
    newrow <- c()
    for(kk in yy[jj,]) {
      newrow <- c(newrow, results_matlist[[kk]][ii, 2], results_matlist[[kk]][ii, 3])
    }
    # errcomp_mat <- rbind(errcomp_mat, round(newrow, 4))
    errcomp_mat <- rbind(errcomp_mat, round(newrow, 8))
  }
  r0_true <- unlist(lapply(strsplit(files_list, "_"),
                           function(x){as.numeric(x[11]) / as.numeric(x[12])}))[1]
  err_names <- rep("", 3)
  # err_names <- unlist(lapply(strsplit(files_list, "_"),
  #                     function(x){paste0("$beta = ",
  #                                        x[12],
  #                                        ", gamma = ",
  #                                        x[13],
  #                                        ", rrr = ",
  #                                        round(as.numeric(x[12]) / as.numeric(x[13]), 3),
  #                                        "$")}))[(1:(length(results_matlist[[1]]$Model) / 4))*4]
  errcomp_mat <- data.frame(err_names, errcomp_mat)[, c(1, 6:9, 2:5)]
  errcomp_mat2 <- errcomp_mat
#  apply(errcomp_mat[, -1], 2, sd)
  errcomp_mat2[, (1:4)*2] <-  round((errcomp_mat[, (1:4)*2] - r0_true) / errcomp_mat[, (1:4)*2 + 1], 4)
  colnames(errcomp_mat) <- c("Data Set", "AR Est", "AR SE",
                             "AR-M Est", "AR-M SE",
                             "Norm Est", "Norm SE",
                             "Norm-M Est", "Norm-M SE")[c(1, 6:9, 2:5)]
  colnames(errcomp_mat2) <- c("Data Set", "AR Est", "AR SE",
                             "AR-M Est", "AR-M SE",
                             "Norm Est", "Norm SE",
                             "Norm-M Est", "Norm-M SE")[c(1, 6:9, 2:5)]
  errcomp[[ii]] <- errcomp_mat
  s0  <- strsplit(strsplit(files_list[kk], "_")[[1]][1], "/")
  cap <- paste0("$\\rr$ Estimates and Std. Errs, ", toupper(results_matlist[[1]]$Model[ii]), " Model,\n",
               "$\\beta = 0.06, \\gamma = 0.03$, $X_0 = 99950, Y_0 = 50$, $\\sigma_X = 100, \\sigma_Y = 5$")
  print(kable(errcomp_mat, caption = cap,
              format = 'latex'))
}

# sink(NULL)
names(errcomp) <- results_matlist[[1]]$Model
