xx <- results_matlist[[4]]

xx$Model <- toupper(xx$Model)

xxx <- cbind(xx, results_matlist[[2]][, 2:3])

colnames(xxx)[-1] <- c("Norm-M Est.", "Norm-M SE", "AR-M Est.", "AR-E SE")

kable(xxx, format = "latex", caption = "")

library(tidyverse)
library(gridExtra)
library(latex2exp)

for(ii in c(2, 4)){
  results_matlist[[ii]]$Model[11] <- "IDEA"
  results_matlist[[ii]]$Model <- toupper(results_matlist[[ii]]$Model)
  results_matlist[[ii]]$Lower <- pmax(results_matlist[[ii]]$Estimate - 2 * results_matlist[[ii]][["Std. Dev"]], -4)
  results_matlist[[ii]]$Upper <- pmin(results_matlist[[ii]]$Estimate + 2 * results_matlist[[ii]][["Std. Dev"]], 5.5)
  results_matlist[[ii]]$Model <- factor(results_matlist[[ii]]$Model, levels = rev(results_matlist[[ii]]$Model))
}

g1 <- ggplot(data = results_matlist[[2]][1:9,], aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 2, col = "black", size = 2) +
  geom_errorbarh(height = .3, size = 2, col = "black") +
  geom_point(size = 4) +
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = "AR-M Errors") + 
  xlim(-4, 5.5) + theme_bw() + theme(title = element_text(size = 18),
                                       axis.text = element_text(size = 11))

g2 <- ggplot(data = results_matlist[[4]][1:9,], aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 2, col = "black", size = 2) +
  geom_errorbarh(height = .3, size = 2, col = "black") +
  geom_point(size = 4) +
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = "Norm-M Errors") + 
  xlim(-4, 5.5) + theme_bw() + theme(title = element_text(size = 18),
                                       axis.text = element_text(size = 11))

grid.arrange(g2, g1, ncol = 2)

xx <- results_matlist[[11]]

xx$Model <- toupper(xx$Model)
colnames(xx)[3] <- "Std. Err"
kable(xx, format = "latex", caption = "")

xx <- results_matlist[[7]]

xx$Model <- toupper(xx$Model)
colnames(xx)[3] <- "Std. Err"
kable(xx, format = "latex", caption = "")

xx <- results_matlist[[3]]

xx$Model <- toupper(xx$Model)
colnames(xx)[3] <- "Std. Err"
kable(xx, format = "latex", caption = "")

xx <- results_matlist[[15]]

xx$Model <- toupper(xx$Model)
colnames(xx)[3] <- "Std. Err"
kable(xx, format = "latex", caption = "")

for(ii in c(3, 7, 11, 15)){
  results_matlist[[ii]]$Model <- toupper(results_matlist[[ii]]$Model)
  results_matlist[[ii]]$Lower <- pmax(results_matlist[[ii]]$Estimate - 2 * results_matlist[[ii]][["Std. Dev"]], -14)
  results_matlist[[ii]]$Upper <- pmin(results_matlist[[ii]]$Estimate + 2 * results_matlist[[ii]][["Std. Dev"]], 25.5)
  results_matlist[[ii]]$Estimate <- pmax(pmin(results_matlist[[ii]]$Estimate, 22.5), -12)
  results_matlist[[ii]]$Model <- factor(results_matlist[[ii]]$Model, levels = rev(results_matlist[[ii]]$Model))
}

xx <- rbind(results_matlist[[11]], results_matlist[[15]], results_matlist[[7]], results_matlist[[3]])
xx$diff <- c(rep("(50, 2)", 11), rep("(500, 20)", 11), rep("(2500, 100)", 11), rep("(10000, 500)", 11))
xx$diff <- factor(xx$diff, levels = c("(50, 2)","(500, 20)","(2500, 100)","(10000, 500)"))

ggplot(data = xx, aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 2, col = "black", size = 2) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = .3, size = 2, col = "black") +
  geom_point(size = 4) +
  facet_wrap(~diff, nrow = 2) + 
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = TeX('$(\\sigma_X, \\sigma_Y)$')) + 
  xlim(-14, 25.5) + theme_bw() + theme(title = element_text(size = 18),
                                     axis.text = element_text(size = 11))


for(ii in c(3)){
  results_matlist[[ii]]$Model[11] <- "IDEA"
  results_matlist[[ii]]$Model <- toupper(results_matlist[[ii]]$Model)
  results_matlist[[ii]]$Lower <- pmax(results_matlist[[ii]]$Estimate - 2 * results_matlist[[ii]][["Std. Dev"]], -4)
  results_matlist[[ii]]$Upper <- pmin(results_matlist[[ii]]$Estimate + 2 * results_matlist[[ii]][["Std. Dev"]], 5.5)
  results_matlist[[ii]]$Model <- factor(results_matlist[[ii]]$Model, levels = rev(results_matlist[[ii]]$Model))
}

xx <- results_matlist[[3]]

colnames(xx)[3] <- "Std. Err"
kable(xx, format = "latex", caption = "")

ggplot(data = xx, aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 2, col = "black", size = 2) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = .3, size = 2, col = "black") +
  geom_point(size = 4) +
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = "Baseline Data") + 
  xlim(-4, 5.5) + theme_bw() + theme(title = element_text(size = 18),
                                     axis.text = element_text(size = 11))

for(ii in c(1)){
  results_matlist[[ii]]$Model[11] <- "IDEA"
  results_matlist[[ii]]$Model <- toupper(results_matlist[[ii]]$Model)
  results_matlist[[ii]]$Lower <- pmax(results_matlist[[ii]]$Estimate - 2 * results_matlist[[ii]][["Std. Dev"]], -4)
  results_matlist[[ii]]$Upper <- pmin(results_matlist[[ii]]$Estimate + 2 * results_matlist[[ii]][["Std. Dev"]], 5.5)
  results_matlist[[ii]]$Model <- factor(results_matlist[[ii]]$Model, levels = rev(results_matlist[[ii]]$Model))
}

xx <- results_matlist[[1]]

colnames(xx)[3] <- "Std. Err"
kable(xx[, -c(4, 5)], format = "latex", caption = "")

ggplot(data = xx, aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 2, col = "black", size = 2) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = .3, size = 2, col = "black") +
  geom_point(size = 4) +
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = "Autoregressive Errors") + 
  xlim(-4, 5.5) + theme_bw() + theme(title = element_text(size = 18),
                                     axis.text = element_text(size = 11))

for(ii in c(2, 4)){
  results_matlist[[ii]]$Model[11] <- "IDEA"
  results_matlist[[ii]]$Model <- toupper(results_matlist[[ii]]$Model)
  results_matlist[[ii]]$Lower <- pmax(results_matlist[[ii]]$Estimate - 2 * results_matlist[[ii]][["Std. Dev"]], -4)
  results_matlist[[ii]]$Upper <- pmin(results_matlist[[ii]]$Estimate + 2 * results_matlist[[ii]][["Std. Dev"]], 5.5)
  results_matlist[[ii]]$Model <- factor(results_matlist[[ii]]$Model, levels = rev(results_matlist[[ii]]$Model))
}

xx <- rbind(results_matlist[[4]][1:9, ], results_matlist[[2]][1:9, ])
xx$diff <- c(rep("Norm", 9), rep("AR", 9))
xx$diff <- factor(xx$diff, levels = c("Norm", "AR"))

colnames(xx)[3] <- "Std. Err"
kable(xx, format = "latex", caption = "")

ggplot(data = xx, aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 2, col = "black", size = 2) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = .3, size = 2, col = "black") +
  geom_point(size = 4) +
  facet_wrap(~diff, ncol = 2) +
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = "Monotone Data") + 
  xlim(-4, 5.5) + theme_bw() + theme(title = element_text(size = 18),
                                     axis.text = element_text(size = 11))


results_matlist[[3]]$Model[11] <- "IDEA"
results_matlist[[3]]$Model <- toupper(results_matlist[[3]]$Model)
results_matlist[[3]]$Lower <- pmax(results_matlist[[3]]$Estimate - 2 * results_matlist[[3]][["Std. Dev"]], -120)
results_matlist[[3]]$Upper <- pmin(results_matlist[[3]]$Estimate + 2 * results_matlist[[3]][["Std. Dev"]], 120)
results_matlist[[3]]$Model <- factor(results_matlist[[3]]$Model, levels = rev(results_matlist[[3]]$Model))
xx <- results_matlist[[3]]

colnames(xx)[3] <- "Std. Err"
kable(xx[, -c(4, 5)], format = "latex", caption = "")

results_matlist[[7]]$Model[11] <- "IDEA"
results_matlist[[7]]$Model <- toupper(results_matlist[[7]]$Model)
results_matlist[[7]]$Lower <- pmax(results_matlist[[7]]$Estimate - 2 * results_matlist[[7]][["Std. Dev"]], -4)
results_matlist[[7]]$Upper <- pmin(results_matlist[[7]]$Estimate + 2 * results_matlist[[7]][["Std. Dev"]], 5.5)
results_matlist[[7]]$Model <- factor(results_matlist[[7]]$Model, levels = rev(results_matlist[[7]]$Model))
xx <- results_matlist[[7]]

colnames(xx)[3] <- "Std. Err"
kable(xx[, -c(4, 5)], format = "latex", caption = "")
  
results_matlist[[11]]$Model[11] <- "IDEA"
results_matlist[[11]]$Model <- toupper(results_matlist[[11]]$Model)
results_matlist[[11]]$Lower <- pmax(results_matlist[[11]]$Estimate - 2 * results_matlist[[11]][["Std. Dev"]], -4)
results_matlist[[11]]$Upper <- pmin(results_matlist[[11]]$Estimate + 2 * results_matlist[[11]][["Std. Dev"]], 5.5)
results_matlist[[11]]$Estimate <- pmin(results_matlist[[11]]$Estimate, 5.25)
results_matlist[[11]]$Model <- factor(results_matlist[[11]]$Model, levels = rev(results_matlist[[11]]$Model))

results_matlist[[15]]$Model[11] <- "IDEA"
results_matlist[[15]]$Model <- toupper(results_matlist[[15]]$Model)
results_matlist[[15]]$Lower <- pmax(results_matlist[[15]]$Estimate - 2 * results_matlist[[15]][["Std. Dev"]], -40)
results_matlist[[15]]$Upper <- pmin(results_matlist[[15]]$Estimate + 2 * results_matlist[[15]][["Std. Dev"]], 55)
results_matlist[[15]]$Estimate <- pmin(results_matlist[[15]]$Estimate, 52.5)
results_matlist[[15]]$Model <- factor(results_matlist[[15]]$Model, levels = rev(results_matlist[[15]]$Model))
xx <- results_matlist[[15]]

colnames(xx)[3] <- "Std. Err"
kable(xx, format = "latex", caption = "")

g1 <- ggplot(data =  results_matlist[[3]][1:9, ], aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 60, col = "black", size = 2) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = .3, size = 2, col = "black") +
  geom_point(size = 4) +
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = TeX('$\\textbf{R}_0 = 60$')) + 
  xlim(-120, 120) + theme_bw() + theme(title = element_text(size = 14),
                                     axis.text = element_text(size = 10))

g2 <- ggplot(data =  results_matlist[[7]][1:9, ], aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 1.5, col = "black", size = 2) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = .3, size = 2, col = "black") +
  geom_point(size = 4) +
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = TeX('$\\textbf{R}_0 = 1.5$')) + 
  xlim(-4, 5.5) + theme_bw() + theme(title = element_text(size = 14),
                                     axis.text = element_text(size = 10))

g3 <- ggplot(data =  results_matlist[[11]][1:9, ], aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 1, col = "black", size = 2) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = .3, size = 2, col = "black") +
  geom_point(size = 4) +
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = TeX('$\\textbf{R}_0 = 1$')) + 
  xlim(-4, 5.5) + theme_bw() + theme(title = element_text(size = 14),
                                     axis.text = element_text(size = 10))

g4 <- ggplot(data =  results_matlist[[15]][1:9, ], aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 0.25, col = "black", size = 2) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = .3, size = 2, col = "black") +
  geom_point(size = 4) +
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = TeX('$\\textbf{R}_0 = 0.25$')) + 
  xlim(-40, 55) + theme_bw() + theme(title = element_text(size = 14),
                                     axis.text = element_text(size = 10))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol = 2)

errcomp <- list()
yy <- matrix(c(1:12),
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

for(ii in c(3, 7, 11, 15)){
  results_matlist[[ii]]$Model <- toupper(results_matlist[[ii]]$Model)
  results_matlist[[ii]]$Lower <- pmax(results_matlist[[ii]]$Estimate - 2 * results_matlist[[ii]][["Std. Dev"]], -4)
  results_matlist[[ii]]$Upper <- pmin(results_matlist[[ii]]$Estimate + 2 * results_matlist[[ii]][["Std. Dev"]], 5.5)
  results_matlist[[ii]]$Estimate <- pmax(pmin(results_matlist[[ii]]$Estimate, 5), -3.5)
  results_matlist[[ii]]$Model <- factor(results_matlist[[ii]]$Model, levels = rev(results_matlist[[ii]]$Model))
}

xx <- rbind(results_matlist[[11]][1:9, ], results_matlist[[15]][1:9, ], results_matlist[[7]][1:9, ], results_matlist[[3]][1:9, ])
xx$diff <- c(rep("(50, 2)", 9), rep("(500, 20)", 9), rep("(2500, 100)", 9), rep("(10000, 500)", 9))
xx$diff <- factor(xx$diff, levels = c("(50, 2)", "(500, 20)", "(2500, 100)", "(10000, 500)"))

ggplot(data = xx, aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 2, col = "black", size = 2) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = .3, size = 2, col = "black") +
  geom_point(size = 4) +
  facet_wrap(~diff, nrow = 2) + 
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = TeX('$(\\sigma_X, \\sigma_Y)$')) + 
  xlim(-4, 5.5) + theme_bw() + theme(title = element_text(size = 18),
                                       axis.text = element_text(size = 11))