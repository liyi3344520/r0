# *set the working directory!*

source("r0_estimator.R")

# *set variable 'data_path' to be the folder where the data are!*
## data_path <- 

### Character vector of all files within data_path
files_list <- paste0(data_path, "/", 
                     list.files(data_path, recursive = TRUE))

# simulation parameters for reference
sim_param_table <- data.frame(dataset = c("Baseline", 
                                          "InfRec1", "InfRec2", "InfRec3", "InfRec4",
                                          "Time1", "Time2", "Time3", "Time4",
                                          "Initial1", "Initial2", "Initial3", "Initial4",
                                          "PopSize1", "PopSize2", "PopSize3", "PopSize4", 
                                          "Var1", "Var2", "Var3", "Var4", 
                                          "Poly1", "Poly2", "Exp", "SEIR"),
                              model = c(rep("SIR", 21), "Linear", "Quadratic", "Exponential", "SEIR"),
                              pars_beta = rep(0.1, 25),
                              pars_gamma = c(0.05, 0.01, 0.025, 0.1, 0.4,
                                             rep(0.05, 20)),
                              obs = c(rep(365, 5), c(200, 100, 50, 20), rep(365, 16)),
                              init_s = c(rep(99950, 9), c(99999, 99990, 99900, 99000, 999500, 9950, 990, 99),
                                         rep(99950, 8)),
                              init_i = c(rep(50, 9), c(1, 10, 100, 1000, 500, 50, 10, 1),
                                         rep(50, 8)),
                              var_s = c(rep(10, 17), c(50, 250, 1000, 5000),
                                        rep(10, 4)),
                              var_r = c(rep(5, 17), c(2, 15, 50, 250),
                                        rep(5, 4))
)
reps <- 5

res_list <- list()
iter <- 0

for(ff in files_list){
  iter <- iter + 1
  print(iter)
  curr_dat <- read.csv(ff, header = TRUE, sep = ",")
  table_row <- which(sapply(sim_param_table$dataset, grepl, x = ff))
  if(length(table_row) != 1){
    print(ff)
    next
  }
  sim_param <- sim_param_table[table_row, ]
  if(sim_param$dataset == "SEIR") {
    curr_dat$S <- curr_dat$S + curr_dat$E
    curr_dat$E <- NULL
  }
  initpop <- c("S" = sim_param$init_s, "I" = sim_param$init_i, "R" = 0)
  startpar <- c("beta" = 0.3, "gamma" = 0.1)
  startparrep <- c("r0" = 3, "gamma" = 0.1)
  truegam <- sim_param$pars_gamma

  rez <- r0_estimator(curr_dat, initpop, startpar, startparrep, truegam)
  listindexname <- paste0(sim_param$dataset, strsplit(ff, "_")[[1]][2])
  res_list[[listindexname]][[strsplit(strsplit(ff, "_")[[1]][3], "[.]")[[1]][1]]] <- rez
}

