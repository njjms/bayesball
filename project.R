library(Lahman)
library(tidyverse)
library(MASS)
library(readr)

setwd("~/Bayesian/project")
source("sim_conforto.R")

conforto2019 <- read_csv("conforto/2019.csv")
conforto2018 <- read_csv("conforto/2018.csv")
conforto2017 <- read_csv("conforto/2017.csv")

# Look at players from all time
career <- Batting %>% 
    filter(AB > 0) %>% 
    anti_join(Pitching, by = "playerID") %>%
    group_by(playerID) %>% 
    summarise(
        H = sum(H),
        AB = sum(AB)
    ) %>% 
    mutate(
        average = H/AB
    )

m <- MASS::fitdistr(career$average[!(career$average %in% c(0,1))],
                    dbeta,
                    start = list(shape1 = 15, shape2 = 40))

ggplot(career) +
    geom_histogram(mapping = aes(x = average,
                                 y = ..density..),
                   fill = "dodgerblue1",
                   color = "orange") +
    stat_function(fun = dbeta,
                  n = 100,
                  col = "red",
                  args = list(shape1 = m$estimate[1], shape2 = m$estimate[2])) +
    labs(
        title = "Distribution of all Batting Averages",
        subtitle = "Taken from 9607 players in MLB history",
        x = "BA"
    ) +
    theme_minimal()

# Best fitting beta distribution is alpha = 11.14 and beta = 35.62

mean(rbeta(n = 10000, shape1 = 11.14, shape2 = 35.62))

# Lets look at just the Mets
mets.2018.prior <- mets_prior(2018)

# prior is alpha = 10.589, beta = 36.763
mean(rbeta(n = 10000, shape1 = mets.2018.prior$estimate[1], shape2 = mets.2018.prior$estimate[2])) # .223

mets <- Batting %>% 
    filter(AB > 0 & teamID == "NYN" & yearID == 2018) %>% 
    anti_join(Pitching, by = "playerID") %>%
    group_by(playerID) %>% 
    summarise(
        H = sum(H),
        AB = sum(AB)
    ) %>% 
    mutate(
        average = H/AB
    )

ggplot(mets) +
    geom_histogram(mapping = aes(x = average,
                                 y = ..density..),
                   fill = "dodgerblue1",
                   color = "orange") +
    stat_function(fun = dbeta,
                  n = 100,
                  col = "red",
                  args = list(shape1 = mets.2018.prior$estimate[1], shape2 = mets.2018.prior$estimate[2])) +
    stat_function(fun = dbeta,
                  n = 100,
                  col = "purple",
                  linetype = 2,
                  args = list(shape1 = .5, shape2 = .5)) +
    geom_vline(xintercept = .223,
               linetype = 2) +
    labs(
        title = "Distribution of all Batting Averages",
        subtitle = "Taken from 2018 Mets",
        x = "BA"
    ) +
    theme_minimal()

# Cool! Now we have a beta prior. Let's look at Michael Conforto
# Let's simulate a bunch of his seasons with p = .257 (we'll treat this as his "true" 2019 batting average)
# Let's compare the estimation of p between normal Batting Average and Empirical Bayesian estimation using Beta-Binomial model - which converges faster?
# For our prior, we will adjust the batting average distribution of the 2018 Mets to fit closer to the 

p <- .257
set.seed(182)

conforto.2018.prior <- adjust_prior(p = .243,
                                    shape1 = mets.2018.prior$estimate[1],
                                    shape2 = mets.2018.prior$estimate[2])

sim.conforto2019 <- sim.conforto(data = conforto2019,
                                 ini.alpha = conforto.2018.prior[1],
                                 ini.beta = conforto.2018.prior[2])

averages <- c("cumulative" = "orange",
            "ebe" = "dodgerblue1")
ggplot(sim.conforto2019, aes(color = averages)) +
    geom_line(mapping = aes(x = Game,
                            y = cumAverage,
                            color = "cumulative"),
              linetype = 1,
              size =1) +
    geom_line(mapping = aes(x = Game,
                            y = ebAverage,
                            color = "ebe"),
              size = 1,
              linetype = 1) +
    geom_hline(yintercept = p,
               color = "black",
               linetype = 2) +
    scale_color_manual(values = averages) +
    labs(
        title = "Empirical Bayesian Estimation vs. Normal Batting Average Calculation",
        subtitle = "simulated using Michael Conforto's 2019 season",
        y = "Batting Average"
    ) +
    theme_minimal() +
    theme(
        legend.position = "top"
    )

mean((sim.conforto2019$cumAverage - p)^2) # 0.001703591 
mean((sim.conforto2019$ebAverage - p)^2) # 0.0003877084

# That was one simulated season. What if we tried this 1000 times?

set.seed(182)
sims.df <- data.frame(
    Game = integer(),
    Date = character(),
    AB = double(),
    H = integer(),
    cumAB = double(),
    cumH = double(),
    cumAverage = double(),
    ebAverage = double(),
    alpha = double(),
    beta=double(),
    simID = integer()
)

for (i in 1:1000) {
    tmp <- sim.conforto(conforto2019, 
                        ini.alpha = conforto.2018.prior[1],
                        ini.beta = conforto.2018.prior[2])
    tmp$simID <- rep(i, nrow(tmp))
    sims.df <- rbind(sims.df, tmp)
}

sims.df %>% 
    filter(Game == 75) %>% 
    group_by(simID) %>% 
    summarise(
        ebMSE = mean((ebAverage - p)^2),
        cumMSE = mean((cumAverage - p)^2)
    ) %>% 
    ggplot() +
    geom_histogram(mapping = aes(x = ebMSE),
                   fill = "dodgerblue1",
                   alpha = .5,
                   binwidth = .0001) +
    geom_histogram(mapping = aes(x = cumMSE),
                   fill = "orange",
                   alpha = .5,
                   binwidth = .0001) +
    labs(
        title = "Histograms of MSE at midpoint in the season"
    ) +
    theme_minimal()

sims.df %>% 
    filter(Game == 151) %>% 
    group_by(simID) %>% 
    summarise(
        ebMSE = mean((ebAverage - p)^2),
        cumMSE = mean((cumAverage - p)^2)
    ) %>% 
    ggplot() +
    geom_histogram(mapping = aes(x = ebMSE),
                   fill = "dodgerblue1",
                   alpha = .5,
                   binwidth = .0001) +
    geom_histogram(mapping = aes(x = cumMSE),
                   fill = "orange",
                   alpha = .5,
                   binwidth = .0001) +
    labs(
        title = "Histogram of MSE for last game of the season"
    ) +
    theme_minimal()

sims.df %>% 
    group_by(Game) %>% 
    summarise(
        ebSD = sd(ebAverage),
        cumSD = sd(cumAverage)
    ) %>% 
    ggplot(aes(color = averages)) +
    geom_line(mapping = aes(x = Game,
                            y = ebSD,
                            color = "ebe"),
              linetype = 2,
              size = 1) +
    geom_line(mapping = aes(x = Game,
                            y = cumSD,
                            color = "cumulative"),
              linetype = 3,
              size = 1) +
    scale_color_manual(values = averages) +
    labs(
       title = "SD of batting average estimates as season progresses",
       subtitle = "Data simulated from ole Scooter's 2019 season",
       y = "Standard deviation of estimates"
    ) +
    theme_minimal()+
    theme(
        legend.position = "top"
    )

sims.df %>% 
    filter(simID == 1) %>% 
    group_by(Game) %>% 
    summarise(
        ebb = mean(ebAverage),
        cum = mean(cumAverage),
        jeffreys.upper = mean(qbeta(.975, .5 + cumH, .5 + cumAB - cumH)),
        jeffreys.lower = mean(qbeta(.025, .5 + cumH, .5 + cumAB - cumH)),
        cp.upper = mean(qbeta(.975, cumH, cumAB - cumH + 1)),
        cp.lower = mean(qbeta(.025, cumH, cumAB - cumH + 1)),
        bayes.upper = mean(qbeta(.975, alpha + cumH, beta + cumAB - cumH)),
        bayes.lower = mean(qbeta(.025, alpha + cumH, beta + cumAB - cumH))
    ) %>% 
    ggplot(., aes(x = Game,
                    color = averages)) +
    geom_line(mapping = aes(y = ebb,
                            color = "ebe"),
              linetype = 3) +
    geom_line(mapping = aes(y = bayes.upper,
                            color = "ebe")) +
    geom_line(mapping = aes(y = bayes.lower,
                            color = "ebe")) +
    geom_ribbon(mapping = aes(ymin = bayes.lower,
                              ymax = bayes.upper,
                              x = Game),
                inherit.aes = FALSE,
                fill = "dodgerblue1",
                alpha = .2) +
    geom_line(mapping = aes(y = cum,
                            color = "cumulative"),
              linetype = 3) +
    geom_line(mapping = aes(y = jeffreys.lower,
                            color = "cumulative")) +
    geom_line(mapping = aes(y = jeffreys.upper,
                            color = "cumulative")) +
    geom_line(mapping = aes(y = cp.lower,
                            color = "cumulative"),
              linetype = 2) +
    geom_line(mapping = aes(y = cp.upper,
                            color = "cumulative"),
              linetype = 2) +
    geom_ribbon(mapping = aes(ymin = cp.lower,
                              ymax = jeffreys.upper,
                              x = Game),
                inherit.aes = FALSE,
                fill = "orange",
                alpha = .05) +
    scale_color_manual(values = averages,
                       name = "Methods") +
    geom_hline(yintercept = p,
               color = "black") +
    labs(
        title = "Bayesian Credible Intervals vs. Frequentist Confidence Intervals",
        subtitle = "from 1000 Simulations of 'Forto's 2019 season",
        y = "Averages",
        caption = "Dotted orange line is the Clopper-Pearson Confidence Inteval"
    ) +
    theme_minimal() +
    theme(
        legend.position = "top"
    )

sims.df %>% 
    group_by(Game) %>% 
    summarise(
        ebMSE = mean((ebAverage - p)^2),
        cumMSE = mean((cumAverage - p)^2)
    ) %>% 
    ggplot() +
    geom_line(mapping = aes(x = Game,
                            y = ebMSE,
                            color = "ebe"),
              linetype = 2,
              size = 1) +
    geom_line(mapping = aes(x = Game,
                            y = cumMSE,
                            color = "cumulative"),
              linetype = 3,
              size = 1) +
    geom_text(mapping = aes(x = 50, y = .02,
                            label = "Diff: .005"),
              size = 3) +
    geom_segment(mapping = aes(x = 50, y = .0005, xend = 50, yend = .018)) +
    geom_text(mapping = aes(x = 151, y = .02,
                            label = "Diff: .00005"),
              size = 3) +
    geom_segment(mapping = aes(x = 151, y = .0005, xend = 151, yend = .018)) +
    scale_color_manual(values = averages) +
    labs(
       title = "MSE of batting average estimates as season progresses",
       subtitle = "1000 simulated 2019 seasons from Conforto",
       y = "MSE"
    ) +
    theme_minimal()+
    theme(
        legend.position = "top"
    )

sims.df %>% 
    group_by(Game) %>% 
    summarise(
        ebMSE = mean((ebAverage - p)^2),
        cumMSE = mean((cumAverage - p)^2)
    ) %>% 
    filter(Game == 151)

sims.df %>% 
    mutate(
        jeffreys.upper = qbeta(.975, .5 + cumH, .5 + cumAB - cumH),
        jeffreys.lower = qbeta(.025, .5 + cumH, .5 + cumAB - cumH),
        cp.upper = qbeta(.975, cumH, cumAB - cumH + 1),
        cp.lower = qbeta(.025, cumH, cumAB - cumH + 1),
        bayes.upper = qbeta(.975, alpha + cumH, beta + cumAB - cumH),
        bayes.lower = qbeta(.025, alpha + cumH, beta + cumAB - cumH),
        jeff.cov = ifelse(jeffreys.lower <= p & p <= jeffreys.upper, 1, 0),
        cp.cov = ifelse(cp.lower <= p & p <= cp.upper, 1, 0),
        bayes.cov = ifelse(bayes.lower <= p & p <= bayes.upper, 1, 0)
    ) %>% 
    group_by(Game) %>% 
    summarise(
        coverage.jeff = mean(jeff.cov),
        coverage.cp = mean(cp.cov),
        coverage.bayes = mean(bayes.cov),
    ) -> plot2019coverages

ggplot(plot2019coverages, aes(x = Game,
                              color = averages)) +
    geom_line(mapping = aes(y = coverage.bayes,
                            color = "ebe")) +
    geom_line(mapping = aes(y = coverage.jeff,
                            color = "cumulative")) +
    geom_line(mapping = aes(y = coverage.cp,
                            color = "cumulative"),
              linetype = 5) +
    scale_color_manual(values = averages,
                       name = "Method") +
    labs(
        title = "Coverage of Bayesian Credible Intervals vs. Frequentist Confidence Intervals",
        subtitle = "using Mikey C's simulated 2019 seasons",
        caption = "Dashed orange line is Clopper-Pearson Frequentist Interval"
    ) +
    theme_minimal() +
    theme(
        legend.position = "top"
    )

sims.df %>% 
    filter(Game == 151) %>% 
    summarise(
        a = mean(alpha),
        b = mean(beta)
    )

sims.df %>% 
    filter(Game == 151) %>% 
    ggplot() +
    geom_histogram(mapping = aes(x = alpha),
                   fill = "blue",
                   alpha = .2,
                   binwidth = 5) +
    geom_histogram(mapping = aes(x = beta),
                   fill = "red",
                   alpha = .2,
                   binwidth = 5) +
    geom_vline(xintercept = 153,
               color = "blue",
               linetype = 3) +
    geom_vline(xintercept = 445,
               color = "red",
               linetype = 3) +
    labs(
       title = "Distribution of alpha and beta at the end of each simulated season"
    ) +
    theme_minimal()


# Let's do analysis on 2018 season now
# Michael Conforto is coming off of an all-star 2017 season -- how soon is it apparent that he will not follow up on his great season?

mets.2017.prior <- mets_prior(2017)
mets.2017.prior$estimate[1]/(sum(mets.2017.prior$estimate)) # mets had an overall batting average .252 in 2017

conforto.2017.prior <- adjust_prior(.279,
                                    shape1 = mets.2017.prior$estimate[1],
                                    shape2 = mets.2017.prior$estimate[2]) # Conforto had a .279 batting average in 2017
p <- .243
set.seed(182)

sim.conforto2018 <- sim.conforto(data = conforto2018,
                                 ini.alpha = conforto.2017.prior[1],
                                 ini.beta = conforto.2017.prior[2])

ggplot(sim.conforto2018, aes(color = averages)) +
    geom_line(mapping = aes(x = Game,
                            y = cumAverage,
                            color = "cumulative"),
              linetype = 1,
              size =.7) +
    geom_line(mapping = aes(x = Game,
                            y = ebAverage,
                            color = "ebe"),
              size = .7,
              linetype = 1) +
    geom_hline(yintercept = p,
               color = "black") +
    scale_color_manual(values = averages) +
    labs(
        title = "Empirical Bayesian Estimation vs. Normal Batting Average Calculation",
        subtitle = "simulated using Michael Conforto's 2018 season"
    ) +
    theme_minimal() +
    theme(
        legend.position = "top"
    )

sims.2018.df <- data.frame(
    Game = integer(),
    Date = character(),
    AB = double(),
    H = integer(),
    cumAB = double(),
    cumH = double(),
    cumAverage = double(),
    ebAverage = double(),
    alpha = double(),
    beta = double(),
    simID = integer()
)

for (i in 1:1000) {
    tmp <- sim.conforto(conforto2018, ini.alpha = conforto.2017.prior[1], ini.beta = conforto.2017.prior[2])
    tmp$simID <- rep(i, nrow(tmp))
    sims.2018.df <- rbind(sims.2018.df, tmp)
}


sims.2018.df %>% 
    group_by(Game) %>% 
    summarise(
        ebSD = sd(ebAverage),
        cumSD = sd(cumAverage)
    ) %>% 
    ggplot(aes(color = averages)) +
    geom_line(mapping = aes(x = Game,
                            y = ebSD,
                            color = "ebe"),
              linetype = 2,
              size = 1) +
    geom_line(mapping = aes(x = Game,
                            y = cumSD,
                            color = "cumulative"),
              linetype = 3,
              size = 1) +
    scale_color_manual(values = averages) +
    labs(
       title = "Variance of batting average estimates as season progresses",
       subtitle = "Data simulated from Michael Thomas Conforto's 2018 season"
    ) +
    theme_minimal()+
    theme(
        legend.position = "top"
    )

sims.2018.df %>% 
    group_by(Game) %>% 
    summarise(
        ebb = median(ebAverage),
        cum = median(cumAverage),
        jeffreys.upper = median(qbeta(.975, .5 + cumH, .5 + cumAB - cumH)),
        jeffreys.lower = median(qbeta(.025, .5 + cumH, .5 + cumAB - cumH)),
        cp.upper = median(qbeta(.975, cumH, cumAB - cumH + 1)),
        cp.lower = median(qbeta(.025, cumH, cumAB - cumH + 1)),
        bayes.upper = median(qbeta(.975, alpha + cumH, beta + cumAB - cumH)),
        bayes.lower = median(qbeta(.025, alpha + cumH, beta + cumAB - cumH))
    ) -> plot2018

ggplot(plot2018, aes(x = Game,
                    color = averages)) +
    geom_line(mapping = aes(y = ebb,
                            color = "ebe"),
              linetype = 3) +
    geom_line(mapping = aes(y = bayes.upper,
                            color = "ebe")) +
    geom_line(mapping = aes(y = bayes.lower,
                            color = "ebe")) +
    geom_ribbon(mapping = aes(ymin = bayes.lower,
                              ymax = bayes.upper,
                              x = Game),
                inherit.aes = FALSE,
                fill = "dodgerblue1",
                alpha = .2) +
    geom_line(mapping = aes(y = cum,
                            color = "cumulative"),
              linetype = 3) +
    geom_line(mapping = aes(y = jeffreys.lower,
                            color = "cumulative")) +
    geom_line(mapping = aes(y = jeffreys.upper,
                            color = "cumulative")) +
    geom_line(mapping = aes(y = cp.lower,
                            color = "cumulative"),
              linetype = 2) +
    geom_line(mapping = aes(y = cp.upper,
                            color = "cumulative"),
              linetype = 2) +
    geom_ribbon(mapping = aes(ymin = cp.lower,
                              ymax = jeffreys.upper,
                              x = Game),
                inherit.aes = FALSE,
                fill = "orange",
                alpha = .05) +
    scale_color_manual(values = averages,
                       name = "Methods") +
    geom_hline(yintercept = p,
               color = "black") +
    labs(
        title = "Bayesian Credible Intervals vs. Frequentist Confidence Intervals",
        subtitle = "from 1000 Simulations of Michael Thomas Conforto's 2018 season",
        y = "Batting Averages",
        caption = "Dotted orange line is the Clopper-Pearson Confidence Inteval"
    ) +
    theme_minimal() +
    theme(
        legend.position = "top"
    )

diffs <- c("upper" = "red", "lower" = "black")
ggplot(plot2018, aes(color = diffs)) +
    geom_line(mapping = aes(x = Game,
                            y = jeffreys.upper - bayes.upper,
                            color = "upper")) +
    geom_line(mapping = aes(x = Game,
                            y = bayes.lower - jeffreys.lower,
                            color = "lower")) +
    scale_color_manual(values = diffs,
                       name = "Differences") +
    labs(
        title = "Difference between Bayesian and Frequentist Interval Limits",
        y = "Difference"
    ) +
    theme_minimal() +
    theme(
        legend.position = "top"
    )

# Just for completeness, let's also model Michael's 2017 All-star season.
# Conforto has a .220 batting average in 2016 with a 95 OPS+... nothing to suggest that he would be an all-star following this season
# How soon did it become apparent that he was going to be an all-star?

mets.2015.prior <- mets_prior(2015)
mets.2015.prior$estimate[1]/(sum(mets.2015.prior$estimate)) # mets had an overall batting average .232 in 2015

conforto.2016.prior <- adjust_prior(.220) # Conforto had a .220 batting average in 2016
p <- .279
set.seed(182)

sim.conforto2017 <- sim.conforto(data = conforto2017,
                                 ini.alpha = conforto.2016.prior[1],
                                 ini.beta = conforto.2016.prior[2])
ggplot(sim.conforto2017, aes(color = averages)) +
    geom_line(mapping = aes(x = Game,
                            y = cumAverage,
                            color = "cumulative"),
              linetype = 1,
              size =.7) +
    geom_line(mapping = aes(x = Game,
                            y = ebAverage,
                            color = "ebb"),
              size = .7,
              linetype = 1) +
    geom_hline(yintercept = p,
               color = "black") +
    scale_color_manual(values = averages) +
    labs(
        title = "Empirical Bayesian Estimation vs. Normal Batting Average Calculation",
        subtitle = "simulated using MC's 2017 season"
    ) +
    theme_minimal() +
    theme(
        legend.position = "top"
    )

sims.2017.df <- data.frame(
    Game = integer(),
    Date = character(),
    AB = double(),
    H = integer(),
    cumAB = double(),
    cumH = double(),
    cumAverage = double(),
    ebAverage = double(),
    alpha = double(),
    beta = double(),
    simID = integer()
)

for (i in 1:1000) {
    tmp <- sim.conforto(conforto2017, ini.alpha = conforto.2016.prior[1], ini.beta = conforto.2016.prior[2])
    tmp$simID <- rep(i, nrow(tmp))
    sims.2017.df <- rbind(sims.2017.df, tmp)
}

sims.2017.df %>% 
    filter(Game == 109) %>% 
    group_by(simID) %>% 
    summarise(
        ebMSE = mean((ebAverage - p)^2),
        cumMSE = mean((cumAverage - p)^2)
    ) %>% 
    ggplot() +
    geom_histogram(mapping = aes(x = ebMSE),
                   fill = "dodgerblue1",
                   alpha = .5,
                   binwidth = .0001) +
    geom_histogram(mapping = aes(x = cumMSE),
                   fill = "orange",
                   alpha = .5,
                   binwidth = .0001) +
    labs(
        title = "Histogram of MSE for last game of the season"
    ) +
    theme_minimal()

sims.2017.df %>% 
    filter(Game == 109) %>% 
    group_by(simID) %>% 
    summarise(
        ebMSE = mean((ebAverage - p)^2),
        cumMSE = mean((cumAverage - p)^2)
    ) %>% summary()

sims.2017.df %>% 
    group_by(Game) %>% 
    summarise(
        ebSD = var(ebAverage),
        cumSD = var(cumAverage)
    ) %>% 
    ggplot(aes(color = averages)) +
    geom_line(mapping = aes(x = Game,
                            y = ebSD,
                            color = "ebb"),
              linetype = 2,
              size = 1) +
    geom_line(mapping = aes(x = Game,
                            y = cumSD,
                            color = "cumulative"),
              linetype = 3,
              size = 1) +
    scale_color_manual(values = averages) +
    labs(
       title = "Variance of batting average estimates as season progresses",
       subtitle = "Data simulated from Michael Thomas Conforto's 2017 season"
    ) +
    theme_minimal()+
    theme(
        legend.position = "top"
    )

sims.2017.df %>% 
    group_by(Game) %>% 
    summarise(
        ebb = median(ebAverage),
        cum = median(cumAverage),
        jeffreys.upper = median(qbeta(.975, .5 + cumH, .5 + cumAB - cumH)),
        jeffreys.lower = median(qbeta(.025, .5 + cumH, .5 + cumAB - cumH)),
        cp.upper = median(qbeta(.975, cumH, cumAB - cumH + 1)),
        cp.lower = median(qbeta(.025, cumH, cumAB - cumH + 1)),
        bayes.upper = median(qbeta(.975, alpha + cumH, beta + cumAB - cumH)),
        bayes.lower = median(qbeta(.025, alpha + cumH, beta + cumAB - cumH))
    ) -> plot2017

ggplot(plot2017, aes(x = Game,
                    color = averages)) +
    geom_line(mapping = aes(y = ebb,
                            color = "ebb"),
              linetype = 3) +
    geom_line(mapping = aes(y = bayes.upper,
                            color = "ebb")) +
    geom_line(mapping = aes(y = bayes.lower,
                            color = "ebb")) +
    geom_ribbon(mapping = aes(ymin = bayes.lower,
                              ymax = bayes.upper,
                              x = Game),
                inherit.aes = FALSE,
                fill = "dodgerblue1",
                alpha = .2) +
    geom_line(mapping = aes(y = cum,
                            color = "cumulative"),
              linetype = 3) +
    geom_line(mapping = aes(y = jeffreys.lower,
                            color = "cumulative")) +
    geom_line(mapping = aes(y = jeffreys.upper,
                            color = "cumulative")) +
    geom_line(mapping = aes(y = cp.lower,
                            color = "cumulative"),
              linetype = 2) +
    geom_line(mapping = aes(y = cp.upper,
                            color = "cumulative"),
              linetype = 2) +
    geom_ribbon(mapping = aes(ymin = cp.lower,
                              ymax = jeffreys.upper,
                              x = Game),
                inherit.aes = FALSE,
                fill = "orange",
                alpha = .05) +
    scale_color_manual(values = averages,
                       name = "Methods") +
    geom_hline(yintercept = p,
               color = "black") +
    labs(
        title = "Bayesian Credible Intervals vs. Frequentist Confidence Intervals",
        subtitle = "from 1000 Simulations of Michael Thomas Conforto's 2017 season",
        y = "Batting Averages",
        caption = "Dotted orange line is the Clopper-Pearson Confidence Inteval"
    ) +
    theme_minimal() +
    theme(
        legend.position = "top"
    )

diffs <- c("upper" = "red", "lower" = "black")
ggplot(plot2018, aes(color = diffs)) +
    geom_line(mapping = aes(x = Game,
                            y = jeffreys.upper - bayes.upper,
                            color = "upper")) +
    geom_line(mapping = aes(x = Game,
                            y = bayes.lower - jeffreys.lower,
                            color = "lower")) +
    scale_color_manual(values = diffs,
                       name = "Differences") +
    labs(
        title = "Difference between Bayesian and Frequentist Interval Limits",
        y = "Difference"
    ) +
    theme_minimal() +
    theme(
        legend.position = "top"
    )

sims.2017.df %>% 
    mutate(
        jeffreys.upper = qbeta(.975, .5 + cumH, .5 + cumAB - cumH),
        jeffreys.lower = qbeta(.025, .5 + cumH, .5 + cumAB - cumH),
        cp.upper = qbeta(.975, cumH, cumAB - cumH + 1),
        cp.lower = qbeta(.025, cumH, cumAB - cumH + 1),
        bayes.upper = qbeta(.975, alpha + cumH, beta + cumAB - cumH),
        bayes.lower = qbeta(.025, alpha + cumH, beta + cumAB - cumH),
        jeff.cov = ifelse(jeffreys.lower <= p & p <= jeffreys.upper, 1, 0),
        cp.cov = ifelse(cp.lower <= p & p <= cp.upper, 1, 0),
        bayes.cov = ifelse(bayes.lower <= p & p <= bayes.upper, 1, 0)
    ) %>% 
    group_by(Game) %>% 
    summarise(
        coverage.jeff = mean(jeff.cov),
        coverage.cp = mean(cp.cov),
        coverage.bayes = mean(bayes.cov),
    ) -> plot2017coverages

ggplot(plot2017coverages, aes(x = Game,
                              color = averages)) +
    geom_line(mapping = aes(y = coverage.bayes,
                            color = "ebb")) +
    geom_line(mapping = aes(y = coverage.jeff,
                            color = "cumulative")) +
    geom_line(mapping = aes(y = coverage.cp,
                            color = "cumulative"),
              linetype = 2) +
    scale_color_manual(values = averages,
                       name = "Method") +
    labs(
        title = "Coverage of Bayesian Credible Intervals vs. Frequentist Confidence Intervals",
        subtitle = "using Mikey C's simulated 2017 seasons",
        caption = "Dotted orange line is Clopper-Pearson Frequentist Interval"
    ) +
    theme_minimal() +
    theme(
        legend.position = "top"
    )

# So far, we have been playing in a frequentists' game. We have a single parameter p with an unknown but constant value.
# What if instead p was allowed to vary game over game?
# In this case, Michael Conforto's probability p could be drawn randomly from some distribution F
# Suppose that we drew p randomly everyday from Conforto's Beta prior!

set.seed(182)
p <- rbeta(nrow(conforto2017), shape1 = conforto.2017.prior[1], shape2 = conforto.2017.prior[2])
# Reminder: conforto batted .279 in 2017 so we will draw p from a beta(45.17, 117.316) distribution
# He is coming off a season where he batted .220

sim.conforto.p(data = conforto2017,
               ini.alpha = conforto.2016.prior[1],
               ini.beta = conforto.2016.prior[2],
               p = p) -> sim.conforto2017.p

sum(sim.conforto2017.p$H)/sum(sim.conforto2017.p$AB)

sim.conforto2017.p %>% 
    mutate(
        jeffreys.upper = qbeta(.975, .5 + cumH, .5 + cumAB - cumH),
        jeffreys.lower = qbeta(.025, .5 + cumH, .5 + cumAB - cumH),
        cp.upper = qbeta(.975, cumH, cumAB - cumH + 1),
        cp.lower = qbeta(.025, cumH, cumAB - cumH + 1),
        bayes.upper = qbeta(.975, alpha + cumH, beta + cumAB - cumH),
        bayes.lower = qbeta(.025, alpha + cumH, beta + cumAB - cumH),
        jeff.cov = ifelse(jeffreys.lower <= p & p <= jeffreys.upper, 1, 0),
        cp.cov = ifelse(cp.lower <= p & p <= cp.upper, 1, 0),
        bayes.cov = ifelse(bayes.lower <= p & p <= bayes.upper, 1, 0)
    ) -> plot2017.p.coverages

mean(plot2017.p.coverages$jeff.cov)
mean(plot2017.p.coverages$cp.cov)
mean(plot2017.p.coverages$bayes.cov)

mean(((plot2017.p.coverages$alpha)/(plot2017.p.coverages$alpha + plot2017.p.coverages$beta) - plot2017.p.coverages$p)^2)
mean(((plot2017.p.coverages$cumH/plot2017.p.coverages$cumAB) - plot2017.p.coverages$p)^2)
head(plot2017.p.coverages)

ggplot(plot2017.p.coverages, aes(x = Game,
                                 color =averages)) +
    geom_line(mapping = aes(y = (cumH/cumAB),
                             color = "cumulative")) +
    geom_line(mapping = aes(y = ((alpha)/(alpha + beta)),
                             color = "ebe")) +
    geom_point(mapping = aes(x = Game, y = p),
               inherit.aes = FALSE,
               color = "black") +
    geom_line(mapping = aes(y = bayes.upper,
                            color = "ebe")) +
    geom_line(mapping = aes(y = bayes.lower,
                            color = "ebe")) +
    geom_ribbon(mapping = aes(ymin = bayes.lower,
                              ymax = bayes.upper,
                              x = Game),
                inherit.aes = FALSE,
                fill = "dodgerblue1",
                alpha = .2) +
    geom_line(mapping = aes(y = jeffreys.lower,
                            color = "cumulative")) +
    geom_line(mapping = aes(y = jeffreys.upper,
                            color = "cumulative")) +
    geom_line(mapping = aes(y = cp.lower,
                            color = "cumulative"),
              linetype = 2) +
    geom_line(mapping = aes(y = cp.upper,
                            color = "cumulative"),
              linetype = 2) +
    geom_ribbon(mapping = aes(ymin = cp.lower,
                              ymax = jeffreys.upper,
                              x = Game),
                inherit.aes = FALSE,
                fill = "orange",
                alpha = .05) +
    labs(
        title = "A single simulated 2017 season",
        subtitle = "Michael Conforto has a different batting average every game"
    ) +
    scale_color_manual(values = averages, name = "Methods") +
    theme_minimal() +
    theme(
        legend.position = "top"
    )

ggplot(plot2017.p.coverages, aes(x = Game,
                                 color =averages)) +
    geom_point(mapping = aes(y = (cumH/cumAB - p),
                             color = "cumulative")) +
    geom_point(mapping = aes(y = ((alpha)/(alpha + beta) - p),
                             color = "ebe")) +
    labs(
        title = "Residual of point estimates vs true p",
        subtitle = "MTC's 2017 season with different parameter p's every game"
    ) +
    scale_color_manual(values = averages) +
    geom_hline(yintercept = 0,
               linetype = 2) +
    theme_minimal() +
    theme(
        legend.position = "top"
    )
 
set.seed(182)
sims.2017.p.df <- data.frame(
    Game = integer(),
    Date = character(),
    p = double(),
    AB = double(),
    H = integer(),
    cumAB = double(),
    cumH = double(),
    cumAverage = double(),
    ebAverage = double(),
    alpha = double(),
    beta = double(),
    simID = integer()
)

for (i in 1:1000) {
    p <- rbeta(nrow(conforto2017), shape1 = conforto.2017.prior[1], shape2 = conforto.2017.prior[2])
    sim.conforto.p(data = conforto2017,
                   ini.alpha = conforto.2016.prior[1],
                   ini.beta = conforto.2016.prior[2],
                   p = p) -> tmp
    tmp$simID <- rep(i, nrow(tmp))
    sims.2017.p.df <- rbind(sims.2017.p.df, tmp)
}

sims.2017.p.df %>% 
    mutate(
        jeffreys.upper = qbeta(.975, .5 + cumH, .5 + cumAB - cumH),
        jeffreys.lower = qbeta(.025, .5 + cumH, .5 + cumAB - cumH),
        cp.upper = qbeta(.975, cumH, cumAB - cumH + 1),
        cp.lower = qbeta(.025, cumH, cumAB - cumH + 1),
        bayes.upper = qbeta(.975, alpha + cumH, beta + cumAB - cumH),
        bayes.lower = qbeta(.025, alpha + cumH, beta + cumAB - cumH),
        jeff.cov = ifelse(jeffreys.lower <= p & p <= jeffreys.upper, 1, 0),
        cp.cov = ifelse(cp.lower <= p & p <= cp.upper, 1, 0),
        bayes.cov = ifelse(bayes.lower <= p & p <= bayes.upper, 1, 0)
    ) -> sims.2017.p.df

mean(sims.2017.p.df$jeff.cov)
mean(sims.2017.p.df$cp.cov)
mean(sims.2017.p.df$bayes.cov)

mean(sims.2017.p.df$bayes.upper - sims.2017.p.df$bayes.lower)
mean(sims.2017.p.df$cp.upper - sims.2017.p.df$cp.lower)
mean(sims.2017.p.df$jeffreys.upper - sims.2017.p.df$jeffreys.lower)

mean(((sims.2017.p.df$alpha)/(sims.2017.p.df$alpha + sims.2017.p.df$beta) - sims.2017.p.df$p)^2)
mean(((sims.2017.p.df$cumH/sims.2017.p.df$cumAB) - sims.2017.p.df$p)^2)

sims.2017.p.df %>% 
    group_by(Game) %>% 
    summarise(
        mse.bayes = mean((alpha/(alpha + beta) - p)^2),
        mse.freq = mean((cumH/cumAB - p)^2)
    ) %>% 
    ggplot() +
    geom_line(mapping = aes(x = Game,
                            y = mse.bayes,
                            color = "ebe")) +
    geom_line(mapping = aes(x = Game,
                            y = mse.freq,
                            color = "cumulative")) +
    geom_text(mapping = aes(x = 30, y = .05, label = "Diff: .001"), size = 3) +
    geom_text(mapping = aes(x = 100, y = .05, label = "Diff: .0001"), size = 3) +
    geom_segment(mapping = aes(x = 30, xend = 30, y = .001, yend = .045)) +
    geom_segment(mapping = aes(x = 100, xend = 100, y = .001, yend = .045)) +
    scale_color_manual(values = averages,
                       name = "Methods") +
    labs(
        title = "MSE of estimation methods throughout the 2017 season",
        subtitle = "Every simulated game has a different hit probability",
        y = "MSE"
    ) +
    theme_minimal() +
    theme(
        legend.position = "top"
    )

sims.2017.p.df %>% 
    group_by(Game) %>% 
    summarise(
        cov.bayes = mean(bayes.cov),
        cov.jeff = mean(jeff.cov),
        cov.cp = mean(cp.cov),
        mse.bayes = mean((alpha/(alpha + beta) - p)^2),
        mse.freq = mean((cumH/cumAB - p)^2)
    ) %>% 
    ggplot(aes(color = averages)) +
    geom_line(mapping = aes(x = Game,
                            y = cov.bayes,
                            color = "ebe")) +
    geom_line(mapping = aes(x = Game,
                            y = cov.jeff,
                            color = "cumulative")) +
    geom_line(mapping = aes(x = Game,
                            y = cov.jeff,
                            color = "cumulative"),
              linetype = 3) +
    scale_color_manual(values = averages,
                       name = "Methods") +
    labs(
        title = "Coverage of estimation methods throughout the simulated 2017 season",
        y = "Coverage Percentage"
    ) +
    theme_minimal() +
    theme(
        legend.position = "top"
    )

conforto.2017.prior # We drew random samples from Beta(45.1733, 117.316). How close does our Bayesian method converge on this value?

sims.2017.p.df %>% 
    filter(Game == 109) %>% 
    summarise(
        a = mean(alpha),
        b = mean(beta)
    )

sims.2017.p.df %>% 
    filter(Game == 109) %>% 
    ggplot() +
    geom_histogram(mapping = aes(x = alpha),
                   fill = "blue",
                   alpha = .2,
                   binwidth = 5) +
    geom_histogram(mapping = aes(x = beta),
                   fill = "red",
                   alpha = .2,
                   binwidth = 5) +
    geom_vline(xintercept = 98.7,
               color = "blue",
               linetype = 3) +
    geom_vline(xintercept = 322,
               color = "red",
               linetype = 3) +
    labs(
       title = "Distribution of alpha and beta at the end of each simulated season"
    ) +
    theme_minimal()

# There simply wasn't enough observations of each p since there are only 5 AB's maximum per game

98.7/(98.7 + 322) # Average at end of the season is around .234
conforto.2016.prior[1]/sum(conforto.2016.prior) # Beginning of the season was .220
conforto.2017.prior[1]/sum(conforto.2017.prior) # Actual mean of 2017 season was .278

sims.2017.p.df %>% 
    filter(simID == 7) %>% 
    ggplot() +
    geom_line(mapping = aes(x = Game,
                            y = ebAverage),
              color = "dodgerblue1") +
    geom_line(mapping = aes(x = Game,
                            y = cumAverage),
              color = "orange") +
    geom_hline(yintercept = .279,
               linetype = 2)


# Let's try this again but instead of simulating a new p every game, we simlate a new p for every simulated season
# These p's will be drawn from the Beta distribution centered at the mean of the actual 2017 season
# Let's see how the different methods handle this

set.seed(182)

sims.2017.p.df <- data.frame(
    Game = integer(),
    Date = character(),
    AB = double(),
    H = integer(),
    cumAB = double(),
    cumH = double(),
    cumAverage = double(),
    ebAverage = double(),
    alpha = double(),
    beta = double(),
    p = double(),
    simID = integer()
)

for (i in 1:1000) {
    p <- rbeta(1, shape1 = conforto.2017.prior[1], shape2 = conforto.2017.prior[2])
    sim.conforto(data = conforto2017,
                 ini.alpha = conforto.2016.prior[1],
                 ini.beta = conforto.2016.prior[2]) -> tmp
    tmp$simID <- rep(i, nrow(tmp))
    tmp$p <- rep(p, nrow(tmp))
    sims.2017.p.df <- rbind(sims.2017.p.df, tmp)
}

sims.2017.p.df %>% 
    mutate(
        jeffreys.upper = qbeta(.975, .5 + cumH, .5 + cumAB - cumH),
        jeffreys.lower = qbeta(.025, .5 + cumH, .5 + cumAB - cumH),
        cp.upper = qbeta(.975, cumH, cumAB - cumH + 1),
        cp.lower = qbeta(.025, cumH, cumAB - cumH + 1),
        bayes.upper = qbeta(.975, alpha + cumH, beta + cumAB - cumH),
        bayes.lower = qbeta(.025, alpha + cumH, beta + cumAB - cumH),
        jeff.cov = ifelse(jeffreys.lower <= p & p <= jeffreys.upper, 1, 0),
        cp.cov = ifelse(cp.lower <= p & p <= cp.upper, 1, 0),
        bayes.cov = ifelse(bayes.lower <= p & p <= bayes.upper, 1, 0)
    ) -> sims.2017.p.df

sims.2017.p.df %>% 
    filter(Game == 109) %>% 
    summarise(
        a = mean(alpha),
        b = mean(beta)
    )

posts <- c("alpha" = "blue",
           "beta" = "red")
sims.2017.p.df %>% 
    filter(Game == 109) %>% 
    ggplot() +
    geom_histogram(mapping = aes(x = alpha,
                                 fill = "alpha"),
                   alpha = .2,
                   binwidth = 5) +
    geom_histogram(mapping = aes(x = beta,
                                 fill = "beta"),
                   alpha = .2,
                   binwidth = 5) +
    scale_fill_manual(values = posts, name = "Parameters") +
    geom_vline(xintercept = 113,
               color = "blue",
               linetype = 3) +
    geom_vline(xintercept = 307,
               color = "red",
               linetype = 3) +
    geom_text(mapping = aes(x = 113, y = 30, label = "113"), color = "blue") +
    geom_text(mapping = aes(x = 307, y = 30, label = "307"), color = "red") +
    labs(
       title = expression("Distribution of "*alpha*","*" "*beta*" at the end of the simulated seasons"),
       subtitle = "1000 simulated 2017 seasons from ol' Scooter",
       x = "Parameter space"
    ) +
    theme_minimal() +
    theme(
        legend.position = "top",
        plot.title.position = "plot"
    )

sims.2017.p.df %>% 
    group_by(Game) %>% 
    summarise(
        mse.bayes = mean((alpha/(alpha + beta) - p)^2),
        mse.freq = mean((cumH/cumAB - p)^2)
    ) %>% 
    ggplot(aes(color = averages)) +
    geom_line(mapping = aes(x = Game,
                            y = mse.bayes,
                            color = "ebe")) +
    geom_line(mapping = aes(x = Game,
                            y = mse.freq,
                            color = "cumulative")) +
    scale_color_manual(values = averages,
                       name = "Methods") +
    labs(
        title = "MSE of estimation methods throughout the 2017 season",
        y = "MSE"
    ) +
    theme_minimal() +
    theme(
        legend.position = "top"
    )

sims.2017.p.df %>% 
    group_by(Game) %>% 
    summarise(
        cov.bayes = mean(bayes.cov),
        cov.jeff = mean(jeff.cov),
        cov.cp = mean(cp.cov),
        mse.bayes = mean((alpha/(alpha + beta) - p)^2),
        mse.freq = mean((cumH/cumAB - p)^2)
    ) %>% 
    ggplot(aes(color = averages)) +
    geom_line(mapping = aes(x = Game,
                            y = cov.bayes,
                            color = "ebe")) +
    geom_line(mapping = aes(x = Game,
                            y = cov.jeff,
                            color = "cumulative")) +
    geom_line(mapping = aes(x = Game,
                            y = cov.jeff,
                            color = "cumulative"),
              linetype = 3) +
    scale_color_manual(values = averages,
                       name = "Methods") +
    labs(
        title = "Coverage of estimation methods throughout the simulated 2017 season",
        y = "Coverage Percentage"
    ) +
    theme_minimal() +
    theme(
        legend.position = "top"
    )

unique(sims.2017.p.df$p)

pbeta(qbeta(.025, 113, 307), shape1 = conforto.2017.prior[1], shape2 = conforto.2017.prior[2])
pbeta(qbeta(.975, 113, 307), shape1 = conforto.2017.prior[1], shape2 = conforto.2017.prior[2], lower.tail = FALSE)

ggplot(data.frame(average = unique(sims.2017.p.df$p))) +
    geom_histogram(mapping = aes(x = average,
                                 y = ..density..),
                   fill = "dodgerblue1",
                   color = "orange") +
    stat_function(fun = dbeta,
                  n = 1000,
                  col = "red",
                  args = list(shape1 = conforto.2017.prior[1], shape2 = conforto.2017.prior[2])) +
    stat_function(fun = dbeta,
                  n = 1000,
                  col = "purple",
                  linetype = 2,
                  args = list(shape1 = 113, shape2 = 307)) +
    stat_function(fun = dbeta,
                  n = 1000,
                  col = "purple",
                  linetype = 3,
                  args = list(shape1 = conforto.2016.prior[1], shape2 = conforto.2016.prior[2])) +
    geom_text(mapping = aes(x = .21, y = 7.5, label = "Prior")) +
    geom_text(mapping = aes(x = .30, y = 15, label = "Average\nPosterior")) +
    labs(
        title = "Posterior distribution of 2017 batting averages",
        subtitle = "with histogram of realized batting averages",
        x = "BA"
    ) +
    theme_minimal()
