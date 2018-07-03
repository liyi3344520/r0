## SKG
## Plotting the baseline SIR

## Package from simCAM (https://github.com/shannong19/simCAM)

devtools::load_all("~/simCAM")
library(ggplot2)
library(reshape2)
library(latex2exp)


T <- 364
init_vals <- c(99950, 50, 0)
beta <- .06
gamma <- .03

sir_out <- SIR(T, init_vals, beta, gamma)
head(sir_out)
colnames(sir_out)[2:4] <- c("X", "Y", "Z")

df_melt <- melt(sir_out, id.vars = "t")
colnames(df_melt)[2] <- "Compartment"

g <- ggplot(data = df_melt, aes(x=t, y=value, linetype=Compartment,
                                col=Compartment)) +
    geom_line(size=2) +
    scale_colour_grey() + 
    theme_bw() +
    labs(x = "Time", y = "Number of Individuals",
         title = "SIR (XYZ) Curves",
         subtitle = TeX(sprintf("$N$ = %.2e; $\\beta$ = %.2f; $\\gamma=$%.2f; $(X(0), Y(0))$= (%.3e, %.1e)",
                                sum(init_vals), beta, gamma, init_vals[1], init_vals[2]))) +
    ggplot2::theme(
                     axis.text.x = ggplot2::element_text(size = 16),
                     axis.text.y= ggplot2::element_text(size = 16),
                     axis.title.x= ggplot2::element_text(size = 18),
                     axis.title.y= ggplot2::element_text(size = 18),
                     plot.title = ggplot2::element_text(size = 24),
                     legend.title = ggplot2::element_text(size = 20),
                     legend.text = ggplot2::element_text(size=16),
                     legend.key.size = ggplot2::unit(3, "line"),
                     plot.subtitle = ggplot2::element_text(size=16)
                     )
g


## H1N1 Data plot

library(lubridate)
library(ggplot2)
library(reshape2)
library(scales)
sir_out <- read.csv("../Data/H1N12009/data.csv")
sir_out$t <- 15:34
sir_out$date <- ymd(sir_out$date)

colnames(sir_out)[1:3] <- c("X", "Y", "Z")

df_melt <- melt(sir_out, id.vars = c("t", "date"))
colnames(df_melt)[3] <- "Compartment"

g <- ggplot(data = df_melt, aes(x=date, y=value, linetype=Compartment,
                                col=Compartment)) +
    geom_line(size=1 ) + geom_point(size=2) + 
    scale_colour_grey() + 
    theme_bw() + facet_wrap(~Compartment, scales = "free_y",
                            dir = "v") + 
    labs(x = "Date", y = "Number of Individuals",
         title = "SIR (XYZ) Curves for H1N1 2009 in USA") +
    scale_x_date(labels=date_format("%b %y")) + 
    ggplot2::theme(
                     axis.text.x = ggplot2::element_text(size = 16),
                     axis.text.y= ggplot2::element_text(size = 16),
                     axis.title.x= ggplot2::element_text(size = 18),
                     axis.title.y= ggplot2::element_text(size = 18),
                     plot.title = ggplot2::element_text(size = 24),
                     legend.title = ggplot2::element_text(size = 20),
                     legend.text = ggplot2::element_text(size=16),
                     legend.key.size = ggplot2::unit(3, "line"),
                     plot.subtitle = ggplot2::element_text(size=16)
             ) +
    geom_vline(xintercept=df_melt$date[which(df_melt$date == as.Date("2009-04-30"))[1]],
               linetype = "dotted") +
    geom_vline(xintercept=df_melt$date[which(df_melt$date == as.Date("2009-08-27"))[1]],
                   linetype = "dotted") 
g

ggsave("h1n1-sir.pdf", width=10, height=6)
