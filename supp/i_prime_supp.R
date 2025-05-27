################################################################################
# This R script constructs the figures for "Introducing the I'-chart:
# an improved individuals chart" by Anhøj, Taylor, and Mohammed.
#
# Data are provided as csv files: bacteremia.csv and diabetes_hba1c.csv.
#
# 2025-05-27
# 
# jacob@anhoej.net
################################################################################

# User defined unction ----
# Function to plot control limits from the I'-chart on top of Shewhart chart
# x:     subgroup
# y:     numerator
# n:     denominator
# chart: Shewhart chart
# ...:   additional graphical parameters, e.g. titles
compplot <- function(x, y, n = NULL, chart, ...) {
  p1 <- qic(x, y, n, chart = chart, ...)  # original Shewhart chart
  p2 <- qic(x, y, n, chart = 'ip', ...)   # I'-chart
  
  p1 +                                         # original chart
    ggplot2::geom_line(ggplot2::aes(y = lcl),  # LCL from I'-chart
                       data   = p2$data, 
                       colour = 'tomato') +    # UCL from I'-chart
    ggplot2::geom_line(ggplot2::aes(y = ucl),
                       data   = p2$data, 
                       colour = 'tomato')
}

# Load qicharts2 package ----
library(qicharts2)

# Import data ----

## bacteremia data
bac   <- read.csv('bacteremia.csv', 
                  comment.char = '#',
                  colClasses   = c('Date',
                                   'integer',
                                   'integer',
                                   'integer',
                                   'integer'))


## diabetes data
hba1c <- read.csv('diabetes_hba1c.csv', 
                  comment.char = '#',
                  colClasses   = c('Date',
                                   'numeric',
                                   'integer'))


# Figures for main article ----

## Figure 1: I-chart of average HbA1c without denominator.
qic(month, avg_hba1c,
    data = hba1c,
    chart = 'i',
    title = "Figure 1: I-chart of average HbA1c without denominator",
    ylab = 'mmol / mol',
    xlab = 'Month')

## Figure 2: I'-chart of average HbA1c with denominator.
qic(month, avg_hba1c * n, n,
    data  = hba1c,
    chart = 'ip',
    title = 'Figure 2: I-chart of average HbA1c with denominator',
    ylab  = 'mmol / mol',
    xlab  = 'Month')

## Figure 3: P-chart of proportion patients who died of bacteremia.
compplot(bac$month, bac$deaths, bac$patients,
         chart = 'p',
         title = 'Figure 3: P-chart of proportion patients who died of bacteremia',
         xlab  = 'Month')

## Figure 4: P'-chart of proportion patients who died of bacteremia.
compplot(bac$month, bac$deaths, bac$patients,
         chart = 'pp',
         title = "Figure 4: P'-chart of proportion patients who died of bacteremia",
         xlab  = 'Month')

## Figure 5: U'-chart of infection rates.
compplot(bac$month, bac$ha_infections, bac$risk_days,
         chart    = 'up',
         multiply = 10000,
         title    = "Figure 5: U'-chart of infection rates",
         ylab     = 'Infections per 10,000 risk days',
         xlab     = 'Month')

# Figures for appendix ----

## Figure 6: I-chart from random data from a normal distribution, no denominator
set.seed(1)                   # fixate random number generator
x <- 1:24                     # subgroup
y <- rnorm(24, 20, 3)         # numerator
n <- runif(24, 80, 120)       # denominator

compplot(x, y, 
         chart = 'i', 
         title = "Figure 6: I-chart from random normal data without denominator")

## Figure 7: I-chart from random, normal data with denominator
compplot(x, y * n, n, 
         chart = 'i',
         title = "Figure 7: I'-chart with denominator")

## Figure 8: Xbar-chart from random normal data
set.seed(1)
x <- rep(1:24, round(runif(24, 10, 20)))
y <- rnorm(length(x), 9)
compplot(x, y, 
         chart = 'xbar',
         title = "Figure 8: Xbar-chart from random normal data")

## Figure 9: Xbar-chart, different random seed
set.seed(6)
x <- rep(1:24, round(runif(24, 10, 20)))
y <- rnorm(length(x), 9)
compplot(x, y, 
         chart = 'xbar', 
         title = "Figure 9: Xbar-chart")

## Figure 10: Xbar-chart, different random seed
set.seed(8)
x <- rep(1:24, round(runif(24, 10, 20)))
y <- rnorm(length(x), 9)
compplot(x, y, 
         chart = 'xbar', 
         title = "Figure 10: Xbar-chart")

## Figure 11: P-chart of random binomial data
set.seed(1)
x <- 1:24
n <- round(runif(24, 100, 120))
y <- rbinom(24, n, 0.1)
compplot(x, y, n, 
         chart = 'p',
         title = "Figure 11: P-chart of random binomial data")

## Figure 12: P'-chart
compplot(x, y, n, 
         chart = 'pp',
         title = "Figure 12: P'-chart of random binomial data")

## Figure 13: U-chart from random Poisson data
set.seed(2)
x <- 1:24
n <- (runif(24, 90, 110))
y <- rpois(24, 25)
compplot(x, y, n, 
         chart = 'u', 
         title = "Figure 13: U-chart from random Poisson data")

## Figure 14: U'-chart
compplot(x, y, n, 
         chart = 'up', 
         title = "Figure 14: U-chart from random Poisson data")

