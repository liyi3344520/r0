library(tidyverse)
library(latex2exp)
library(data.table)

errcomp <- list()
# yy <- matrix(c(1:16),
#              ncol = 4, byrow = TRUE)
yy <- matrix(c(9:12, 1:4, 13:16, 5:8),
                     ncol = 4, byrow = TRUE)
for (ii in 1:length(results_matlist[[1]]$Model)) {
  errcomp_mat <- matrix(NA, ncol = 8, nrow = 0)
  for (jj in 1:nrow(yy)) {
    newrow <- c()
    for(kk in yy[jj,]) {
      newrow <- c(newrow, results_matlist[[kk]][ii, 2], results_matlist[[kk]][ii, 3])
    }
    # errcomp_mat <- rbind(errcomp_mat, round(newrow, 4))
    errcomp_mat <- rbind(errcomp_mat, signif(newrow, 4))
  }
  r0_true <- unlist(lapply(strsplit(files_list, "_"),
                           function(x){as.numeric(x[11]) / as.numeric(x[12])}))[1]
  err_names <- rep("", 4)
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
  cap <- paste0("$\\rr$ Estimates and Std. Errs, ", model_names[ii], " Model,\n",
                "$\\beta = 0.06, \\gamma = 0.03$, $X_0 = 99950, Y_0 = 50$, $\\sigma_X = 100, \\sigma_Y = 5$")
  print(kable(errcomp_mat, caption = cap,
              format = 'latex'))
}

model_names <- c("RE", "rRE", "LMA", "LMAT", "IPR", "SIPR", "LL", "MC", "SB", "a", "b")


for(ii in c(1)){
  results_matlist[[ii]]$Model <- model_names
  results_matlist[[ii]]$Lower <- pmax(results_matlist[[ii]]$Estimate - 2 * pmax(results_matlist[[ii]][["Std. Dev"]], 0.025), -4)
  results_matlist[[ii]]$Upper <- pmin(results_matlist[[ii]]$Estimate + 2 * pmax(results_matlist[[ii]][["Std. Dev"]], 0.025), 5.5)
  results_matlist[[ii]]$Model <- factor(results_matlist[[ii]]$Model, levels = rev(results_matlist[[ii]]$Model))
}

xx <- rbind(results_matlist[[1]][1:9, ])
xx$inc <- factor(data.table::between(2, xx$Lower, xx$Upper))

ggplot(data = xx, aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 2, col = "black", size = 2,
             linetype = "dashed", alpha = 0.6) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = .3, size = 1.5, col = "black",
                 alpha = 0.6) +
  geom_point(aes(shape = inc),
             size = 4) +
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = "Autoregressive Data",
       shape = "True Value\nCoverage") + 
  xlim(-4, 5.5) + theme_bw() + theme(title = element_text(size = 22),
                                     axis.text = element_text(size = 16),
                                     legend.text = element_text(size = 12))


for(ii in c(2, 4)){
  results_matlist[[ii]]$Model <- model_names
  results_matlist[[ii]]$Lower <- pmax(results_matlist[[ii]]$Estimate - 2 * pmax(results_matlist[[ii]][["Std. Dev"]], 0.025), -4)
  results_matlist[[ii]]$Upper <- pmin(results_matlist[[ii]]$Estimate + 2 * pmax(results_matlist[[ii]][["Std. Dev"]], 0.025), 5.5)
  results_matlist[[ii]]$Model <- factor(results_matlist[[ii]]$Model, levels = rev(results_matlist[[ii]]$Model))
}

xx <- rbind(results_matlist[[4]][1:9, ], results_matlist[[2]][1:9, ])
xx$inc <- factor(data.table::between(2, xx$Lower, xx$Upper))
xx$diff <- c(rep("Norm", 9), rep("AR", 9))
xx$diff <- factor(xx$diff, levels = c("Norm", "AR"))

colnames(xx)[3] <- "Std. Err"
kable(xx, format = "latex", caption = "")

ggplot(data = xx, aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 2, col = "black", size = 2,
             linetype = "dashed", alpha = 0.6) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = .3, size = 1.5, col = "black",
                 alpha = 0.6) +
  geom_point(aes(shape = inc),
             size = 4) +
  facet_wrap(~diff, ncol = 2) +
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = "Monotone Error Data",
       shape = "True Value\nCoverage") + 
  xlim(-4, 5.5) + theme_bw() + theme(title = element_text(size = 22),
                                     axis.text = element_text(size = 16),
                                     legend.text = element_text(size = 12))




model_names <- c("RE", "rRE", "LMA", "LMAT", "IPR", "SIPR", "LL", "MC", "SB", "a", "b")

for(ii in c(4, 8, 12, 16)){
  results_matlist[[ii]]$Model <- model_names
  results_matlist2 <- results_matlist[[ii]]
  results_matlist2$Estimate <- round(results_matlist2$Estimate, 4)
  results_matlist2[["Std. Dev"]] <- round(results_matlist2[["Std. Dev"]], 4)
  results_matlist[[ii]]$Lower <- pmax(results_matlist[[ii]]$Estimate - 2 * pmax(results_matlist[[ii]][["Std. Dev"]], 0.025), -4)
  results_matlist[[ii]]$Upper <- pmin(results_matlist[[ii]]$Estimate + 2 * pmax(results_matlist[[ii]][["Std. Dev"]], 0.025), 5.5)
  results_matlist[[ii]]$Estimate <- pmax(pmin(results_matlist[[ii]]$Estimate, 5), -3.5)
  results_matlist[[ii]]$Model <- factor(results_matlist[[ii]]$Model, levels = rev(results_matlist[[ii]]$Model))
}

xx <- rbind(results_matlist[[4]][1:9, ], results_matlist[[8]][1:9, ], results_matlist[[12]][1:9, ], results_matlist[[16]][1:9, ])
xx$inc <- factor(data.table::between(2, xx$Lower, xx$Upper))
xx$diff <- c(rep("T = 100", 9), rep("T = 20", 9), rep("T = 200", 9), rep("T = 50", 9))
xx$diff <- factor(xx$diff, levels = c("T = 200", "T = 100", "T = 50",  "T = 20"))

ggplot(data = xx, aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 2, col = "black", size = 2, linetype = "dashed",
             alpha = 0.6) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = .3, size = 1.5, col = "black",
                 alpha = 0.6) +
  geom_point(aes(shape = inc),
             size = 4) +
  facet_wrap(~diff, nrow = 2) + 
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = "Time Points",
       subtitle = "Gaussian Monotonic Error",
       shape = "True Value\nCoverage") + 
  xlim(-4, 5.5) + theme_bw() + theme(title = element_text(size = 22),
                                     axis.text = element_text(size = 16),
                                     legend.text = element_text(size = 12))




model_names <- c("LS", "ReLS", "LMA", "LMAT", "IPR", "SIPR", "LL", "MC", "SB", "a", "b")

for(ii in c(4, 8, 12)){
  results_matlist[[ii]]$Model <- model_names
  results_matlist2 <- results_matlist[[ii]]
  results_matlist2$Estimate <- round(results_matlist2$Estimate, 4)
  results_matlist2[["Std. Dev"]] <- round(results_matlist2[["Std. Dev"]], 4)
  results_matlist[[ii]]$Lower <- pmax(results_matlist[[ii]]$Estimate - 2 * pmax(results_matlist[[ii]][["Std. Dev"]], 0.025), -4)
  results_matlist[[ii]]$Upper <- pmin(results_matlist[[ii]]$Estimate + 2 * pmax(results_matlist[[ii]][["Std. Dev"]], 0.025), 5.5)
  results_matlist[[ii]]$Estimate <- pmax(pmin(results_matlist[[ii]]$Estimate, 5), -3.5)
  results_matlist[[ii]]$Model <- factor(results_matlist[[ii]]$Model, levels = rev(results_matlist[[ii]]$Model))
}

xx <- rbind(results_matlist[[4]][1:9, ], results_matlist[[8]][1:9, ], results_matlist[[12]][1:9, ])
xx$inc <- factor(data.table::between(1.2, xx$Lower, xx$Upper))
xx$diff <- c(rep("Linear", 9), rep("Quartic", 9), rep("Linear SIR", 9))
xx$diff <- factor(xx$diff, levels = c("Linear", "Quartic", "Linear SIR"))

ggplot(data = xx, aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 1.2, col = "black", size = 2, linetype = "dashed",
             alpha = 0.6) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = .3, size = 1.5, col = "black",
                 alpha = 0.6) +
  geom_point(aes(shape = inc),
             size = 4) +
  facet_wrap(~diff, nrow = 2) + 
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = "Other Models",
       subtitle = "Gaussian Monotonic Error",
       shape = "True Value\nCoverage") + 
  xlim(-4, 5.5) + theme_bw() + theme(title = element_text(size = 22),
                                     axis.text = element_text(size = 16),
                                     legend.text = element_text(size = 12))



model_names <- c("LS", "ReLS", "LMA", "LMAT", "IPR", "SIPR", "LL", "MC", "SB", "a", "b")


results_matlist[[4]]$Model <- model_names
results_matlist[[4]]$Lower <- pmax(results_matlist[[4]]$Estimate - 2 * results_matlist[[4]][["Std. Dev"]], -120)
results_matlist[[4]]$Upper <- pmin(results_matlist[[4]]$Estimate + 2 * results_matlist[[4]][["Std. Dev"]], 120)
results_matlist[[4]]$Model <- factor(results_matlist[[4]]$Model, levels = rev(results_matlist[[4]]$Model))

results_matlist[[8]]$Model <- model_names
results_matlist[[8]]$Lower <- pmax(results_matlist[[8]]$Estimate - 2 * results_matlist[[8]][["Std. Dev"]], -4)
results_matlist[[8]]$Upper <- pmin(results_matlist[[8]]$Estimate + 2 * results_matlist[[8]][["Std. Dev"]], 5.5)
results_matlist[[8]]$Model <- factor(results_matlist[[8]]$Model, levels = rev(results_matlist[[8]]$Model))

results_matlist[[12]]$Model <- model_names
results_matlist[[12]]$Lower <- pmax(results_matlist[[12]]$Estimate - 2 * results_matlist[[12]][["Std. Dev"]], -4)
results_matlist[[12]]$Upper <- pmin(results_matlist[[12]]$Estimate + 2 * results_matlist[[12]][["Std. Dev"]], 5.5)
results_matlist[[12]]$Estimate <- pmin(results_matlist[[12]]$Estimate, 5.25)
results_matlist[[12]]$Model <- factor(results_matlist[[12]]$Model, levels = rev(results_matlist[[12]]$Model))

results_matlist[[16]]$Model <- model_names
results_matlist[[16]]$Lower <- pmax(results_matlist[[16]]$Estimate - 2 * results_matlist[[16]][["Std. Dev"]], -40)
results_matlist[[16]]$Upper <- pmin(results_matlist[[16]]$Estimate + 2 * results_matlist[[16]][["Std. Dev"]], 55)
results_matlist[[16]]$Estimate <- pmin(results_matlist[[16]]$Estimate, 52.5)
results_matlist[[16]]$Model <- factor(results_matlist[[16]]$Model, levels = rev(results_matlist[[16]]$Model))


g1 <- ggplot(data =  results_matlist[[4]][1:9, ], aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 60, col = "black", size = 2,
             alpha = 0.6) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = .3, size = 2, col = "black",
                 alpha = 0.6) +
  geom_point(aes(shape = factor(data.table::between(60, results_matlist[[4]][1:9, ]$Lower, 
                                                    results_matlist[[4]][1:9, ]$Upper))), size = 4) +
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = TeX('$\\textbf{R}_0 = 60$'),
       subtitle = "Gaussian Monotonic Error",
       shape = "True Value\nCoverage") + 
  xlim(-120, 120) + theme_bw() + theme(title = element_text(size = 18),
                                       axis.text = element_text(size = 13),
                                       legend.text = element_text(size = 10))

g2 <- ggplot(data =  results_matlist[[8]][1:9, ], aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 1.5, col = "black", size = 2,
             alpha = 0.6) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = .3, size = 2, col = "black",
                 alpha = 0.6) +
  geom_point(aes(shape = factor(data.table::between(1.5, results_matlist[[8]][1:9, ]$Lower, 
                                                    results_matlist[[8]][1:9, ]$Upper))), size = 4) +
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = TeX('$\\textbf{R}_0 = 1.5$'),
       subtitle = "Gaussian Monotonic Error",
       shape = "True Value\nCoverage") + 
  xlim(-4, 5.5) + theme_bw() + theme(title = element_text(size = 18),
                                     axis.text = element_text(size = 13),
                                     legend.text = element_text(size = 10))

g3 <- ggplot(data =  results_matlist[[12]][1:9, ], aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 1, col = "black", size = 2,
             alpha = 0.6) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = .3, size = 2, col = "black",
                 alpha = 0.6) +
  geom_point(aes(shape = factor(data.table::between(1, results_matlist[[12]][1:9, ]$Lower, 
                                                    results_matlist[[12]][1:9, ]$Upper))), size = 4) +
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = TeX('$\\textbf{R}_0 = 1$'),
       subtitle = "Gaussian Monotonic Error",
       shape = "True Value\nCoverage") + 
  xlim(-4, 5.5) + theme_bw() + theme(title = element_text(size = 18),
                                     axis.text = element_text(size = 13),
                                     legend.text = element_text(size = 10))

g4 <- ggplot(data =  results_matlist[[16]][1:9, ], aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 0.25, col = "black", size = 2,
             alpha = 0.6) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = .3, size = 2, col = "black",
                 alpha = 0.6) +
  geom_point(aes(shape = factor(data.table::between(0.25, results_matlist[[16]][1:9, ]$Lower, 
                                                    results_matlist[[16]][1:9, ]$Upper))), size = 4) +
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = TeX('$\\textbf{R}_0 = 0.25$'),
       subtitle = "Gaussian Monotonic Error",
       shape = "True Value\nCoverage") + 
  xlim(-40, 55) + theme_bw() + theme(title = element_text(size = 18),
                                     axis.text = element_text(size = 13),
                                     legend.text = element_text(size = 10))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol = 2)


results_matlist2 <- results_matlist[[16]]
results_matlist2$Estimate <- round(results_matlist2$Estimate, 4)
results_matlist2[["Std. Dev"]] <- round(results_matlist2[["Std. Dev"]], 4)
print(kable(results_matlist2[, 1:3], format = "latex"))










results_matlist[[1]]$Lower <- pmax(results_matlist[[1]]$Estimate - 2 * pmax(results_matlist[[1]][["Std. Dev"]], 0.05), -20)
results_matlist[[1]]$Upper <- pmin(results_matlist[[1]]$Estimate + 2 * pmax(results_matlist[[1]][["Std. Dev"]], 0.05), 45)
results_matlist[[1]]$Model <- factor(results_matlist[[1]]$Model, levels = rev(results_matlist[[1]]$Model))

ggplot(data =  results_matlist[[1]][1:9, ], aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = .3, size = 2, col = "black",
                 alpha = 0.6) +
  geom_point(size = 3) +
  scale_shape_discrete(guide = "none") +
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = "H1N1 Data") + 
  xlim(-20, 45) + theme_bw() + theme(title = element_text(size = 22),
                                       axis.text = element_text(size = 16))