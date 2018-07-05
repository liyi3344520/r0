# setwd("~/R0/Models")
# load("sir_all.RData")
# load("refit.RData")


library(ggplot2)
library(latex2exp)
library(knitr)

results_matlist_comb <- list()
results_matlistbase_comb <- list()
results_matlistvar_comb <- list()
results_matlistpop_comb <- list()
results_matlistoth_comb <- list()

# for(ii in 1:length(results_matlistbase)){
for(ii in 3:3){
  rez <- results_matlistbase[[ii]]
  results_matlistbase_comb[[ii]] <- data.frame(rbind(rez[c(1:10, 18, 11, 13, 14, 16, 17, 19, 20, 15),]))
  results_matlistbase_comb[[ii]]$Method <- factor(results_matlistbase_comb[[ii]]$Model,
                                                  levels = rev(results_matlistbase_comb[[ii]]$Model),
                                                  labels = rev(c("SIR", "SIR Repar.",
                                                             "SIR LMA", "SIR LMAT",
                                                             "SIR Max", "SIR SMax",
                                                             "SIR IPR", "SIR SIPR",
                                                             "SIR MC", "SIR LL",
                                                             "Seq. Bayes",
                                                             "SEIR",
                                                             "Comp", "Next Gen.",
                                                             "Exp. Growth", "MLE",
                                                             "Init. Growth", "Final Size",
                                                             "Branch")))
  results_matlistbase_comb[[ii]]$Lower <- results_matlistbase_comb[[ii]]$Estimate - 2 * results_matlistbase_comb[[ii]]$Std..Dev
  results_matlistbase_comb[[ii]]$Upper <- results_matlistbase_comb[[ii]]$Estimate + 2 * results_matlistbase_comb[[ii]]$Std..Dev
}

for(ii in 1:length(results_matlistvar)){
  rez <- results_matlistvar[[ii]]
  results_matlistvar_comb[[ii]] <- data.frame(rbind(rez[c(1:10, 18, 11, 13, 14, 16, 17, 19, 20, 15),]))
  results_matlistvar_comb[[ii]]$Method <- factor(results_matlistvar_comb[[ii]]$Model,
                                                 levels = rev(results_matlistvar_comb[[ii]]$Model),
                                                 labels = rev(c("SIR", "SIR Repar.",
                                                            "SIR LMA", "SIR LMAT",
                                                            "SIR Max", "SIR SMax",
                                                            "SIR IPR", "SIR SIPR",
                                                            "SIR MC", "SIR LL",
                                                            "Seq. Bayes",
                                                            "SEIR",
                                                            "Comp", "Next Gen.",
                                                            "Exp. Growth", "MLE",
                                                            "Init. Growth", "Final Size",
                                                            "Branch")))
  results_matlistvar_comb[[ii]]$Lower <- results_matlistvar_comb[[ii]]$Estimate - 2 * results_matlistvar_comb[[ii]]$Std..Dev
  results_matlistvar_comb[[ii]]$Upper <- results_matlistvar_comb[[ii]]$Estimate + 2 * results_matlistvar_comb[[ii]]$Std..Dev
}

for(ii in 1:length(results_matlistpop)){
  rez <- results_matlistpop[[ii]]
  results_matlistpop_comb[[ii]] <- data.frame(rbind(rez[c(1:10, 18, 11, 13, 14, 16, 17, 19, 20, 15),]))
  results_matlistpop_comb[[ii]]$Method <- factor(results_matlistpop_comb[[ii]]$Model,
                                                 levels = rev(results_matlistpop_comb[[ii]]$Model),
                                                 labels = rev(c("SIR", "SIR Repar.",
                                                            "SIR LMA", "SIR LMAT",
                                                            "SIR Max", "SIR SMax",
                                                            "SIR IPR", "SIR SIPR",
                                                            "SIR MC", "SIR LL",
                                                            "Seq. Bayes",
                                                            "SEIR",
                                                            "Comp", "Next Gen.",
                                                            "Exp. Growth", "MLE",
                                                            "Init. Growth", "Final Size",
                                                            "Branch")))
  results_matlistpop_comb[[ii]]$Lower <- results_matlistpop_comb[[ii]]$Estimate - 2 * results_matlistpop_comb[[ii]]$Std..Dev
  results_matlistpop_comb[[ii]]$Upper <- results_matlistpop_comb[[ii]]$Estimate + 2 * results_matlistpop_comb[[ii]]$Std..Dev
}

for(ii in 1:length(results_matlistoth)){
  rez <- results_matlistoth[[ii]]
  results_matlistoth_comb[[ii]] <- data.frame(rbind(rez[c(1:10, 18, 11, 13, 14, 16, 17, 19, 20, 15),]))
  results_matlistoth_comb[[ii]]$Method <- factor(results_matlistoth_comb[[ii]]$Model,
                                                 levels = rev(results_matlistoth_comb[[ii]]$Model),
                                                 labels = rev(c("SIR", "SIR Repar.",
                                                   "SIR LMA", "SIR LMAT",
                                                   "SIR Max", "SIR SMax",
                                                   "SIR IPR", "SIR SIPR",
                                                   "SIR MC", "SIR LL",
                                                   "Seq. Bayes",
                                                   "SEIR",
                                                   "Comp", "Next Gen.",
                                                   "Exp. Growth", "MLE",
                                                   "Init. Growth", "Final Size",
                                                   "Branch")))
  results_matlistoth_comb[[ii]]$Lower <- results_matlistoth_comb[[ii]]$Estimate - 2 * results_matlistoth_comb[[ii]]$Std..Dev
  results_matlistoth_comb[[ii]]$Upper <- results_matlistoth_comb[[ii]]$Estimate + 2 * results_matlistoth_comb[[ii]]$Std..Dev
}


data_path <- "C:/Users/Andersen/Documents/PubData/data/simdat/OtherModels"


### Character vector of all files within data_path
files_list <- paste0(data_path, "/", 
                     list.files(data_path, recursive = TRUE))
files_list

ggplot(data = results_matlistbase_comb[[3]], aes(y = Method, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 2, col = "red", size = 2) +
  geom_errorbarh(height = .3, size = 2, col = "black") +
  geom_point(size = 4) +
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = "Baseline Data Set") + 
  xlim(-1.5,5.5) + theme_bw() + theme(title = element_text(size = 18),
                                 axis.text = element_text(size = 11))

ggplot(data = results_matlistvar_comb[[3]], aes(y = Method, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 2, col = "red", size = 2) +
  geom_errorbarh(height = .3, size = 2, col = "black") +
  geom_point(size = 4) +
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = TeX('$\\sigma_X = 1000, \\sigma_Y = 100$')) + 
  xlim(-1.5,5.5) + theme_bw() + theme(title = element_text(size = 18),
                                 axis.text = element_text(size = 11))

ggplot(data = results_matlistpop_comb[[3]], aes(y = Method, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 2, col = "red", size = 2) +
  geom_errorbarh(height = .3, size = 2, col = "black") +
  geom_point(size = 4) +
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = TeX('$X_0 = 99000, Y_0 = 1000$')) + 
  xlim(-1.5,5.5) + theme_bw() + theme(title = element_text(size = 18),
                                 axis.text = element_text(size = 11))

ggplot(data = results_matlistoth_comb[[7]], aes(y = Method, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 1.2, col = "red", size = 2) +
  geom_errorbarh(height = .3, size = 2, col = "black") +
  geom_point(size = 4) +
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = "1st Order Linear Data") + 
  xlim(-1.5,5.5) + theme_bw() + theme(title = element_text(size = 18),
                                 axis.text = element_text(size = 11))






# for(ii in 1:length(results_matlist)){
#   rez <- results_matlist[[ii]]
#   results_matlist_comb[[ii]] <- data.frame(rez[c(1:10, 18, 11, 13, 14, 16, 17, 19, 20, 15), ])
#   results_matlist_comb[[ii]]$Method <- factor(results_matlist_comb[[ii]]$Model,
#                                                   levels = rev(results_matlist_comb[[ii]]$Model),
#                                                   labels = rev(c("SIR", "SIR Repar.",
#                                                                  "SIR LMA", "SIR LMAT",
#                                                                  "SIR Max", "SIR SMax",
#                                                                  "SIR IPR", "SIR SIPR",
#                                                                  "SIR MC", "SIR LL",
#                                                                  "Seq. Bayes",
#                                                                  "SEIR",
#                                                                  "Comp", "Next Gen.",
#                                                                  "Exp. Growth", "MLE",
#                                                                  "Init. Growth", "Final Size",
#                                                                  "Branch")))
#   results_matlist_comb[[ii]]$Lower <- results_matlist_comb[[ii]]$Estimate - 2 * results_matlist_comb[[ii]]$Std..Dev
#   results_matlist_comb[[ii]]$Upper <- results_matlist_comb[[ii]]$Estimate + 2 * results_matlist_comb[[ii]]$Std..Dev
# }
# 
# setwd("~/R0/DataSim/NewPar2/Plots")
# for(num in 1:length(files_list)){
#   bet <- strsplit(strsplit(files_list[num], "/")[[1]][10], "_")[[1]][5]
#   gam <- strsplit(strsplit(files_list[num], "/")[[1]][10], "_")[[1]][6]
#   tit <- paste0("$\\beta = ", bet, ", \\gamma = ", gam, "$")
#   p <- ggplot(data = results_matlist_comb[[num]], aes(y = Method, x = Estimate, xmin = Lower, xmax = Upper)) +
#     geom_vline(xintercept = as.numeric(bet) / as.numeric(gam), col = "red", size = 2) +
#     geom_errorbarh(height = .3, size = 2, col = "black") +
#     geom_point(size = 4) +
#     labs(x = TeX('$\\textbf{R}_0$'), y = "Method",
#          title = TeX(tit)) +
#     xlim(-1.5,5.5) + theme_bw() + theme(title = element_text(size = 18),
#                                         axis.text = element_text(size = 11))
#   ggsave(paste0(bet, "_", gam, ".png"), p)
# }
# 
# for(num in 1:length(files_list)){
#   bet <- strsplit(strsplit(files_list[num], "/")[[1]][10], "_")[[1]][5]
#   gam <- strsplit(strsplit(files_list[num], "/")[[1]][10], "_")[[1]][6]
#   tit <- paste0("$\\beta = ", bet, ", \\gamma = ", gam, "$")
#   print(kable(results_matlist[[num]], format = "latex",
#               caption = paste0(tit, ", $\\textbf{R}_0 = ", round(as.numeric(bet)/as.numeric(gam), 2), "$")))
# }

for(ii in 1:16){
  results_matlist[[ii]]$Lower <- pmax(results_matlist[[ii]]$Estimate - 2 * results_matlist[[ii]][["Std. Dev"]], -4)
  results_matlist[[ii]]$Upper <- pmin(results_matlist[[ii]]$Estimate + 2 * results_matlist[[ii]][["Std. Dev"]], 5.5)
  results_matlist[[ii]]$Model <- factor(results_matlist[[ii]]$Model, levels = rev(results_matlist[[ii]]$Model))
}

ggplot(data = results_matlist[[1]], aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 2, col = "red", size = 2) +
  geom_errorbarh(height = .3, size = 2, col = "black") +
  geom_point(size = 4) +
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = "AR Errors") + 
  xlim(-1.5, 5.5) + theme_bw() + theme(title = element_text(size = 18),
                                      axis.text = element_text(size = 11))

library(gridExtra)
xx <- rbind(results_matlist[[3]], results_matlist[[7]], results_matlist[[11]], results_matlist[[7]])
xx$diff <- c(rep("200", 9), rep("100", 9), rep("50", 9), rep("20", 9))
xx$diff <- factor(xx$diff, levels = c("200", "100", "50", "20"))

ggplot(data = xx, aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 2, col = "red", size = 2) +
  geom_errorbarh(height = .3, size = 2, col = "black") +
  geom_point(size = 4) +
  facet_wrap(~diff, nrow = 2) + 
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = "Time Points") + 
  xlim(-4, 5.5) + theme_bw() + theme(title = element_text(size = 18),
                                       axis.text = element_text(size = 11))

xx <- rbind(results_matlist[[3]], results_matlist[[7]], results_matlist[[11]])
xx$diff <- c(rep("1st", 9), rep("4th", 9), rep("Linear SIR", 9))
xx$diff <- factor(xx$diff, levels = c("1st", "4th", "Linear SIR"))

ggplot(data = xx, aes(y = Model, x = Estimate, xmin = Lower, xmax = Upper)) +
  geom_vline(xintercept = 1.2, col = "red", size = 2) +
  geom_errorbarh(height = .3, size = 2, col = "black") +
  geom_point(size = 4) +
  facet_wrap(~diff, nrow = 2) + 
  labs(x = TeX('$\\textbf{R}_0$'), y = "Method", title = "Other Model") + 
  xlim(-4, 5.5) + theme_bw() + theme(title = element_text(size = 18),
                                     axis.text = element_text(size = 11))