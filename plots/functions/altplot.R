res_path <- "C:/Users/Andersen/Documents/R0/DataSim/PaperResults/3"

res_list <- paste0(res_path, "/", 
                     list.files(res_path))[-3]

alt_list <- vector("list", length = 3)

alt_list2 <- vector("list", length = 3)

for(kk in 1:length(alt_list)){
  alt_list[[kk]] <- matrix(NA, nrow = 1, ncol = 2)
  colnames(alt_list[[kk]]) <- c("Estimate", "Std. Dev")
}

blah <- c(1, 2, 7)
for(ii in 1:length(res_list)){
  load(res_list[ii])
  for(jj in 1:length(results_matlist)){
    for(kk in 1:3) {
      alt_list[[kk]] <- rbind(alt_list[[kk]], results_matlist[[jj]][blah[kk], 2:3])
    }
  }
}

dat_names_anorder <- c("Base", "Base3", "Base4", "Base5",
                       "SD = (10000, 500)", "SD = (2500, 100)", "SD = (50, 2)", "SD = (500, 20)", 
                       "R0 = 60", "R0 = 1.5", "R0 = 1", "R0 = 0.25",
                       "N = 100", "N = 1000", "N = 10000", "N = 1000000",
                       "Y(0) = 1000", "Y(0) = 100", "Y(0) = 10", "Y(0) = 1",
                       "T = 100", "T = 20", "T = 200", "T = 50")

dat_names_paperorder <- c("Base",
                          "R0 = 60", "R0 = 1.5", "R0 = 1", "R0 = 0.25",
                          "T = 200", "T = 100", "T = 50", "T = 20",
                          "Y(0) = 1000", "Y(0) = 100", "Y(0) = 10", "Y(0) = 1",
                          "N = 1000000", "N = 10000", "N = 1000", "N = 100",
                          "SD = (50, 2)", "SD = (500, 20)", "SD = (2500, 100)", "SD = (10000, 500)")
  
mod_names <- c("RE", "rRE", "LL")

for(kk in 1:length(alt_list2)){
  alt_list2[[kk]] <- data.frame(Estimate = alt_list[[kk]][-1, 1],
                                SE = alt_list[[kk]][-1, 2])
  alt_list2[[kk]]$Error <- rep(c("AR", "AR-M", "Norm", "Norm-M"), times = 96/4)
  alt_list2[[kk]]$Names <- rep(dat_names_anorder, each = 4)
  alt_list2[[kk]]$Model <- mod_names[kk]
  alt_list2[[kk]]$Standard <- (alt_list2[[kk]]$Estimate - 2) / alt_list2[[kk]]$SE
}

alt_list2[[2]][85, 1] <- -0.1539
alt_list2[[2]][85, 2] <- 2.7692
alt_list2[[2]][85, 6] <- (-0.1539 - 2) / 2.7692

results_matlist <- rbind(alt_list2[[1]][-c(5:16), ], alt_list2[[2]][-c(5:16), ], alt_list2[[3]][-c(5:16), ])

for(ii in c(1)){
  results_matlist$Standard <- pmax(pmin(results_matlist$Standard, 10.5), -7.5)
  results_matlist$Names <- factor(results_matlist$Names, levels = rev(dat_names_paperorder))
  results_matlist$Error <- factor(results_matlist$Error, levels = c("AR", "AR-M", "Norm", "Norm-M"))
  results_matlist$Model <- factor(results_matlist$Model, levels = c("RE", "rRE", "LL"))
}

ggplot(data = results_matlist, aes(y = Names, x = Standard)) +
  geom_vline(xintercept = 0, col = "black", size = 1, linetype = "dashed",
             alpha = 0.6) +
  geom_point(size = 1) +
  facet_wrap(Model~Error, nrow = 3) + 
  labs(x = TeX('Standardized $\\textbf{R}_0$'), y = "", title = "Error Robustness") + 
  xlim(-8, 11) + theme_bw() + theme(title = element_text(size = 18),
                                     axis.text = element_text(size = 4, angle = 20),
                                     legend.text = element_text(size = 12))




### Other Model
res_path <- "C:/Users/Andersen/Documents/R0/DataSim/PaperResults/3"

res_list <- paste0(res_path, "/", 
                   list.files(res_path))[c(1, 3)]

alt_list <- vector("list", length = 3)

alt_list2 <- vector("list", length = 3)

for(kk in 1:length(alt_list)){
  alt_list[[kk]] <- matrix(NA, nrow = 1, ncol = 2)
  colnames(alt_list[[kk]]) <- c("Estimate", "Std. Dev")
}

blah <- c(1, 2, 7)
for(ii in 1:length(res_list)){
  load(res_list[ii])
  for(jj in 1:length(results_matlist)){
    for(kk in 1:3) {
      alt_list[[kk]] <- rbind(alt_list[[kk]], results_matlist[[jj]][blah[kk], 2:3])
    }
  }
}

dat_names_order <- c("Baseline","Baseline3","Baseline4","Baseline5", "Linear", "Quartic", "Linear SIR")



mod_names <- c("RE", "rRE", "LL")

for(kk in 1:length(alt_list2)){
  alt_list2[[kk]] <- data.frame(Estimate = alt_list[[kk]][-1, 1],
                                SE = alt_list[[kk]][-1, 2])
  alt_list2[[kk]]$Error <- rep(c("AR", "AR-M", "Norm", "Norm-M"), times = 28/4)
  alt_list2[[kk]]$Names <- rep(dat_names_order, each = 4)
  alt_list2[[kk]]$Model <- mod_names[kk]
  alt_list2[[kk]]$Standard <- (alt_list2[[kk]]$Estimate - 2) / alt_list2[[kk]]$SE
}

results_matlist <- rbind(alt_list2[[1]][-c(5:16), ], alt_list2[[2]][-c(5:16), ], alt_list2[[3]][-c(5:16), ])

for(ii in c(1)){
  results_matlist$Standard <- pmax(pmin(results_matlist$Standard, 10.5), -7.5)
  results_matlist$Names <- factor(results_matlist$Names, levels = dat_names_order)
  results_matlist$Error <- factor(results_matlist$Error, levels = c("AR", "AR-M", "Norm", "Norm-M"))
  results_matlist$Model <- factor(results_matlist$Model, levels = c("RE", "rRE", "LL"))
}

ggplot(data = results_matlist, aes(y = Error, x = Standard)) +
  geom_vline(xintercept = 0, col = "black", size = 1, linetype = "dashed",
             alpha = 0.6) +
  geom_point(size = 1.5) +
  facet_wrap(Model~Names, nrow = 3) + 
  labs(x = TeX('Standardized $\\textbf{R}_0$'), y = "", title = "Model Robustness") + 
  xlim(-8, 11) + theme_bw() + theme(title = element_text(size = 18),
                                    axis.text = element_text(size = 6, angle = 20),
                                    legend.text = element_text(size = 12))




### Time1
model_names <- c("RE", "rRE", "LMA", "LMAT", "IPR", "SIPR", "LL", "MC", "SB", "a", "b")

for(ii in c(3, 7, 11, 15)){
  results_matlist[[ii]]$Model <- model_names
  results_matlist2 <- results_matlist[[ii]]
  results_matlist2$Estimate <- round(results_matlist2$Estimate, 4)
  results_matlist2[["Std. Dev"]] <- round(results_matlist2[["Std. Dev"]], 4)
  results_matlist[[ii]]$Lower <- pmax(results_matlist[[ii]]$Estimate - 2 * pmax(results_matlist[[ii]][["Std. Dev"]], 0.025), -8)
  results_matlist[[ii]]$Upper <- pmin(results_matlist[[ii]]$Estimate + 2 * pmax(results_matlist[[ii]][["Std. Dev"]], 0.025), 11)
  results_matlist[[ii]]$Estimate <- pmax(pmin(results_matlist[[ii]]$Estimate, 10.5), -7.5)
  results_matlist[[ii]]$Model <- factor(results_matlist[[ii]]$Model, levels = results_matlist[[ii]]$Model)
}

xx <- rbind(results_matlist[[3]][1:9, ], results_matlist[[7]][1:9, ], results_matlist[[11]][1:9, ], results_matlist[[15]][1:9, ])
xx$inc <- factor(data.table::between(2, xx$Lower, xx$Upper))
xx$diff <- c(rep("T = 100", 9), rep("T = 20", 9), rep("T = 200", 9), rep("T = 50", 9))
xx$diff <- factor(xx$diff, levels = rev(c("T = 200", "T = 100", "T = 50",  "T = 20")))

ggplot(data = xx, aes(y = diff, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = .3, size = 1.5, col = "black",
                 alpha = 0.6) +
  geom_vline(xintercept = 2, col = "black", size = 1, linetype = "dashed",
             alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~Model, nrow = 3) + 
  labs(x = TeX('$\\textbf{R}_0$'), y = "T*", title = "Time Point Differences") + 
  xlim(-8, 11) + theme_bw() + theme(title = element_text(size = 22),
                                     axis.text = element_text(size = 16),
                                     legend.text = element_text(size = 12))


### Time2

model_names <- c("RE", "rRE", "LMA", "LMAT", "IPR", "SIPR", "LL", "MC", "SB", "a", "b")

results_matlist <- list(results_matlist_daily[[1]],
                        results_matlist_weekly[[1]],
                        results_matlist_monthly[[1]],
                        results_matlist_quarterly[[1]])

for(ii in c(1, 2, 3, 4)){
  results_matlist[[ii]]$Model <- model_names[1:9]
  results_matlist2 <- results_matlist[[ii]]
  results_matlist2$Estimate <- round(results_matlist2$Estimate, 4)
  results_matlist2[["Std. Dev"]] <- round(results_matlist2[["Std. Dev"]], 4)
  results_matlist[[ii]]$Lower <- pmax(results_matlist[[ii]]$Estimate - 2 * pmax(results_matlist[[ii]][["Std. Dev"]], 0.025), -8)
  results_matlist[[ii]]$Upper <- pmin(results_matlist[[ii]]$Estimate + 2 * pmax(results_matlist[[ii]][["Std. Dev"]], 0.025), 11)
  results_matlist[[ii]]$Estimate <- pmax(pmin(results_matlist[[ii]]$Estimate, 10.5), -7.5)
  results_matlist[[ii]]$Model <- factor(results_matlist[[ii]]$Model, levels = results_matlist[[ii]]$Model)
}

xx <- rbind(results_matlist[[1]][1:9, ], results_matlist[[2]][1:9, ], results_matlist[[3]][1:9, ], results_matlist[[4]][1:9, ])
xx$inc <- factor(data.table::between(2, xx$Lower, xx$Upper))
xx$diff <- c(rep("Daily", 9), rep("Weekly", 9), rep("Monthly", 9), rep("Quarterly", 9))
xx$diff <- factor(xx$diff, levels = rev(c("Daily", "Weekly", "Monthly",  "Quarterly")))

ggplot(data = xx, aes(y = diff, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = .3, size = 1.5, col = "black",
                 alpha = 0.6) +
  geom_vline(xintercept = 2, col = "black", size = 1, linetype = "dashed",
             alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~Model, nrow = 3) + 
  labs(x = TeX('$\\textbf{R}_0$'), y = "Times", title = "Observation Times") + 
  xlim(-8, 11) + theme_bw() + theme(title = element_text(size = 22),
                                    axis.text = element_text(size = 16),
                                    legend.text = element_text(size = 12))