## SKG
## June 15, 2018
## Getting data for Pandemic influenza between April and August 2009 for USA


## DOWNLOAD DATA:
## SEASON: 2008-2009
## REGION: NATIONAL
## DATA SOURCES: WHO/NREVSS and ILINET
## https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html

## READ IN DATA
flu <- read.csv("ILINet.csv", skip = 1)
head(flu)

plot(flu$X..WEIGHTED.ILI, type = "l", lwd=2)

## Get the date
## https://www.cmmcp.org/sites/cmmcp/files/uploads/2009epi.pdf
## Week 21 start 5/23, Week 33 ends 8/22
## Week 15 (April 13) - Week 34 (ends Aug. 29)

start_week <- 15
end_week <- 34
start_ind <- which(flu$WEEK == start_week)
end_ind <- which(flu$WEEK == end_week)
flu_sub <- flu[start_ind:end_ind,]
plot(flu_sub$X.UNWEIGHTED.ILI, type = "l", lwd = 2)

## Population of US on April 1, 2010:
## https://www.census.gov/quickfacts/fact/table/US/POP010210#viewtop
## 308,740,000 individuals

## So assumingggg

us_pop <- 30874000
incidence <- flu_sub$X.UNWEIGHTED.ILI / 100 * us_pop
S <- us_pop - cumsum(incidence)

## gamma = 1/3 days{-1} (inverse of avg recovery = 3days)
## gamma = (7/3) weeks^{-1} (i.e. average duration is 3/7 week)
## SOURCE: VESPIGNANI ET AL 2006 http://journals.plos.org/plosmedicine/article/file?id=10.1371/journal.pmed.0040013&type=printable

## Incidence to prevalence
## http://sphweb.bumc.bu.edu/otlt/MPH-Modules/EP/EP713_DiseaseFrequency/EP713_DiseaseFrequency7.html
I <- 3/7 * incidence
R <- us_pop - S - I


library(lubridate)
df <- data.frame(S = S, I =I, R=R)
df$date <- ymd("2009-01-01") + weeks(flu_sub$WEEK )
write.csv(df, "../../r0_refs/Data/H1N12009/data.csv", row.names = FALSE)


##############################################################
## Trying again with confirmed cases
## Same website, looking at confirmed H1N1 caes

## READ IN DATA
h1n1 <- read.csv("FluView_StackedColumnChart_Data.csv") #H1N1 confirmed cases
flu <- read.csv("ILINet.csv", skip = 1)
head(flu)
us_pop <- 30874000

start_week <- 15
end_week <- 34
start_ind <- which(flu$WEEK == start_week)
end_ind <- which(flu$WEEK == end_week)
flu_sub <- flu[start_ind:end_ind, ]
h1n1_sub <- h1n1[start_ind:end_ind, ]
flu_sub$H1N1_percent <- h1n1_sub$A..2009 / h1n1_sub$TOTAL 
plot(start_week:end_week, flu_sub$H1N1 * 100,
     type = "l", lwd = 2)
## H1N1 percent confirmed * wILI
flu_sub$H1N1_wili <- flu_sub$H1N1_percent * flu_sub$X..WEIGHTED
plot(start_week:end_week, flu_sub$X..WEIGHTED,
     type = "l", lwd = 2)

## Population of US on April 1, 2010:
## https://www.census.gov/quickfacts/fact/table/US/POP010210#viewtop
## 308,740,000 individuals

## So assumingggg

us_pop <- 30874000
incidence <- flu_sub$X..WEIGHTED.ILI / 100 * us_pop
S <- us_pop - cumsum(incidence)

## gamma = 1/3 (inverse of avg recovery = 3days)
## gamma = (7/3) weeks^{-1} (i.e. average duration is 3/7 week)
## SOURCE: VESPIGNANI ET AL 2006 http://journals.plos.org/plosmedicine/article/file?id=10.1371/journal.pmed.0040013&type=printable

## Incidence to prevalence
## http://sphweb.bumc.bu.edu/otlt/MPH-Modules/EP/EP713_DiseaseFrequency/EP713_DiseaseFrequency7.html

gamma_seq <-1/seq(2, 10, length.out = 9)
M <- length(gamma_seq)
df_list <- vector(mode = "list", length = M)
for(ii in 1:M){
    I <- numeric(length(incidence))
    gamma <- gamma_seq[ii]
    I[1] <- incidence[1]
    for(tt in 2:length(incidence)){
        I[tt] <- I[tt-1] * (1 - gamma) + incidence[tt]
    }
    R <- us_pop - S - I
    N <- us_pop

    library(lubridate)
    
    df <- data.frame(S = S, I =I, R=R, gamma = gamma)
    df$date <- ymd("2009-01-01") + weeks(flu_sub$WEEK )
    df_list[[ii]] <- df
}

df <- dplyr::bind_rows(df_list)


write.csv(df, "../../r0_refs/Data/H1N12009/data-h1n1-2.csv", row.names = FALSE)

library(ggplot2)
g1 <- ggplot(data = df, aes(x = date, y = I, col = gamma,
                      group = factor(gamma)))+ 
    geom_line() + geom_point()

g2 <- ggplot(data = df, aes(x = date, y = R, col = gamma,
                      group = factor(gamma)))+ 
    geom_line() + geom_point()

g3 <- ggplot(data = df, aes(x = date, y = S, col = gamma,
                      group = factor(gamma)))+ 
    geom_line() + geom_point()

library(gridExtra)
grid.arrange(g1, g2, g3, ncol = 1)

## Dplyr version
library(tidyr)
library(dplyr)
df_long <- pivot_longer(data = df, -c(gamma, date), names_to = "State",
                        values_to = "Count")
df_long$State <- ifelse(df_long$State == "S", "X",
           ifelse(df_long$State == "I", "Y",
                  "Z"))
ggplot(data = df_long,  aes(x = date, y = Count, col = factor(1/gamma),
                           group = factor(1/gamma))) + geom_line(alpha = .95) +
    geom_point(alpha = .8) +
    facet_wrap(~State, ncol = 1, scales = "free") +
    theme_bw(base_size = 14) +
    labs( x= "Date", y = "Number in State",
         title = "Influenza: Incidence to SIR values",
         subtitle = latex2exp::TeX("For different values of average time to infection $\\gamma^{-1}$")) +
    scale_color_brewer(palette = "Greys", direction = -1,
                          name = latex2exp::TeX("$\\gamma^{-1} (days)$")) +
    geom_vline(xintercept= as.Date("2009-04-30"),
               linetype = "dotted") +
    geom_vline(xintercept= as.Date("2009-08-27"),
                   linetype = "dotted") 
ggsave("h1n1-data.pdf")
table(rowSums(df[, c("S", "I", "R")]))

plot(df$R, type = "b")
lines(df$I, type = "b", col = "red")

## LL test
x0 <- df$S[1]

T <- 15
r0 <- -sum(log(df$S[1:T] / x0)) / sum(df$R[1:T] / N)
r0
plot(df$R / N, -log(df$S / x0))
abline(v = df$R[T] / N)
abline(b = r0, a=0)
