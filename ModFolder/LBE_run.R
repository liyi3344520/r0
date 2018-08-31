this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("./likelihood_shell.R")

data_path <- "C:/Users/Andersen/Documents/R0/DataSim/PaperData/Other"

files_list <- paste0(data_path, "/", 
                     list.files(data_path, recursive = TRUE))

results_matlist_exp <- list()
output_lists <- list()

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
  # start_pop <- c("S" = as.numeric(strsplit(strsplit(xx[jj], "/")[[1]][10], "_")[[1]][1]),
  #                "I" = as.numeric(strsplit(strsplit(xx[jj], "/")[[1]][10], "_")[[1]][2]), "R" = 0)
  # print(start_pop)

  start_pop <- c("S" = 99950,
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
  
  results_r0$LBE <- try(sir_likelihood(start_pop, dat[, 3], dat[, 4], dat[, 2]))
  
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
  results_matlist_exp[[jj]] <- results_mat
  # output_lists[[jj]] <- output_list
  # sink(NULL, type = "message")
  # sink(NULL)
}
