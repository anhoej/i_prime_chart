#' ONE CHART TO RULE THEM ALL
#' 
#' Observations:
#'  
#'    In chart is a very close match for prime charts and the original I chart.
#'  
#'    In chart is often a reasonable match for C, U, P, and Xbar charts.
#'  
#'    The In chart plots the ratio. If we want to plot the original y values 
#'    (numerator) for measurement data, we must multiply the numerator and the
#'    denominator before plotting.
#'    
#'    With multiple measurements or counts per subgroup, the mean is plotted,
#'    which may not be what we want (e.g. C and S charts). Use the agg.fun 
#'    argument to specify.
#'    
#'  Questions:
#'  
#'    Can the In chart replace prime charts and I charts?
#'  
#'    Is the In chart (and prime charts) a better (or worse) representation of
#'    counts, rates, and proportions than the original C, U, and P charts?
#'    
#'    CAN THE In CHART REPLACE ALL OTHER CHARTS?
#'
#' To run the examples below, install dev version of qicharts2 from GitHub: 
#'    devtools::install_github('anhoej/qicharts2')

library(qicharts2)
library(tidyverse)
library(patchwork)

# Function to plot control limits from the In chart on top of Shewhart chart ---
compplot <- function(x, y, n = NULL, chart, ...) {
  p1 <- qic(x, y, n, chart = chart, ...)
  p2 <- qic(x, y, n, chart = 'ip', ...)
  
  p1 +
    geom_line(aes(y = lcl), data = p2$data, colour = 'tomato') +
    geom_line(aes(y = ucl), data = p2$data, colour = 'tomato')
}

# Random normal data -----------------------------------------------------------
x <- 1:24
y <- rnorm(24, 20, 3)
n <- runif(24, 9, 11)
compplot(x, y, chart = 'i')

# with denominator, multiply numerator to keep y axis scale
compplot(x, y, n, chart = 'i') /
  compplot(x, y * n, n, chart = 'i')

# multiple measurements per subgroup
x <- rep(1:24, round(runif(24, 10, 20)))
y <- rnorm(length(x), 9)
compplot(x, y, chart = 'xbar') /
  compplot(x, y, chart = 'i')

# Random binomial data ---------------------------------------------------------
x <- 1:24
n <- round(runif(24, 100, 120))
y <- rbinom(24, n, 0.1)
compplot(x, y, n, 'p') /
  compplot(x, y, n, 'pp')

# small numbers
n <- round(runif(24, 100, 120))
y <- rbinom(24, n, 0.01)
compplot(x, y, n, 'p') /
  compplot(x, y, n, 'pp')

# big numbers
n <- round(runif(24, 100, 120))
y <- rbinom(24, n, 0.5)
compplot(x, y, n, 'p') /
  compplot(x, y, n, 'pp')

p1 <- compplot(x, y, n, 'p')
layer_scales(p1)$y$range$range

# Random poisson data ----------------------------------------------------------
x <- 1:24
n <- round(runif(24, 1000, 1200))
y <- rpois(24, 25)
compplot(x, y, chart = 'c') / 
  compplot(x, y, chart = 'i')
compplot(x, y, n, chart = 'u') / 
  compplot(x, y, n, 'up')

# small numbers
x <- 1:24
n <- round(runif(24, 1000, 1200))
y <- rpois(24, 4)
compplot(x, y, n, 'u') / 
  compplot(x, y, n, 'up')

# Taylor's complaint data ------------------------------------------------------ 
# source: https://variation.com/normalized-individuals-control-chart/
compplot(complaints$month,
         complaints$complaints, 
         complaints$sales, 
         'u',
         multiply = 10000) /
  compplot(complaints$month,
           complaints$complaints, 
           complaints$sales, 
           'up',
           multiply = 10000)

# Taylor's lot data ------------------------------------------------------------
compplot(lots$lot, lots$value, lots$n, 'i')

# NHS accidents data -----------------------------------------------------------
compplot(nhs_accidents$i, nhs_accidents$r, nhs_accidents$n, 'p') /
  compplot(nhs_accidents$i, nhs_accidents$r, nhs_accidents$n, 'pp')

# CDI data ---------------------------------------------------------------------
compplot(cdi$month, cdi$n, chart = 'c')
compplot(cdi$month, cdi$n, chart = 'c', 
         part = cdi$period)
compplot(cdi$month, cdi$n, cdi$days, chart = 'u', multiply = 10000)
compplot(cdi$month, cdi$n, cdi$days, chart = 'u',
         multiply = 10000, part = cdi$period) /
  compplot(cdi$month, cdi$n, cdi$days, chart = 'up',
           multiply = 10000, part = cdi$period)

# Mohammed's example data ------------------------------------------------------
mam <- data.frame(
  x = 1:12,
  y = c(77.58, 77.19, 73.69, 69.17, 67.16, 67.58, 
        77.96, 74.99, 78.09, 81.74, 70.82, 68.61),
  n = c(1702, 1919, 1836, 1846, 1910, 1779, 
        2023, 1987, 1815, 1998, 1796, 1987)
)
compplot(mam$x, mam$y * mam$n, mam$n, chart = 'i', decimals = 4)

# testing with no denominator
compplot(mam$x, mam$y, chart = 'i')

# testing with missing values
mam$y[3] <- NA
mam$n[7] <- NA
compplot(mam$x, mam$y, mam$n, multiply = 1000, chart = 'i')

# C section data ---------------------------------------------------------------
d <- read_csv('data/csection_delay.csv', comment = '#')
compplot(d$month, d$delay, chart = 'xbar') /
  compplot(d$month, d$delay, agg.fun = 'sd', chart = 's')

# Bacteremia data --------------------------------------------------------------
d <- read_csv('data/bacteremia.csv', comment = '#')
compplot(d$month, d$ha_infections, chart = 'c')
compplot(d$month, d$ha_infections, d$risk_days, chart = 'u') /
  compplot(d$month, d$ha_infections, d$risk_days, chart = 'up')
compplot(d$month, d$deaths, d$patients, chart = 'p') /
  compplot(d$month, d$deaths, d$patients, chart = 'pp')

# Cdiff data -------------------------------------------------------------------
d <- read_csv('data/cdiff.csv', comment = '#')
compplot(d$month, d$infections, chart = 'c') /
  compplot(d$month, d$infections, chart = 'i')
compplot(d$month, d$infections, d$risk_days, chart = 'u', multiply = 10000) /
  compplot(d$month, d$infections, d$risk_days, chart = 'up', multiply = 10000)

# Renography data --------------------------------------------------------------
d <- read_csv('data/renography_doses.csv', comment = '#')
compplot(d$week, d$dose, chart = 'xbar') /
  compplot(d$week, d$dose, chart = 'i')
compplot(d$week, d$dose, agg.fun = 'sd', chart = 's') /
  compplot(d$week, d$dose, agg.fun = 'sd', chart = 'i')

# Birth data ------------------------------------------
d <- read_csv('data/robson1_births.csv', comment = '#')
compplot(d$biweek, d$length, chart = 'xbar') /
  compplot(d$biweek, d$length, chart = 'i')
compplot(d$biweek, d$length, agg.fun = 'sd', chart = 's') /
  compplot(d$biweek, d$length, agg.fun = 'sd', chart = 'i')

# Diabetes data ----------------------------------------------------------------
# NOTE: data have already been aggregated (y = average hba1c)
d <- read_rds('data/lkt_diabetes.rds') |> 
  filter(region == 'Hovedstaden', var == 'o1a') |> 
  select(month, y, n)
compplot(d$month, d$y, d$n, chart = 'i')        # WRONG!
compplot(d$month, d$y * d$n, d$n, chart = 'i')  # CORRECT!

# Hospital infection data ------------------------------------------------------
d <- filter(hospital_infections, infection == 'BAC')

# NOTE: multiple counts (hospitals) per subgroup
compplot(d$month, d$n, chart = 'c')                   # OOPS!
compplot(d$month, d$n, chart = 'c', agg.fun = 'sum')  # BETTER
compplot(d$month, d$n, d$days, multiply = 10000, chart = 'u') /
  compplot(d$month, d$n, d$days, multiply = 10000, chart = 'up')

# funnel plot
qic(hospital, n, days, data = d,
    chart = 'up')
qic(hospital, n, days, data = d,
    chart = 'in')
compplot(d$hospital, d$n, d$days, chart = 'up')

bac2 <- d |> 
  arrange(n) |> 
  mutate(hospital = fct_inorder(hospital))
compplot(bac2$hospital, bac2$n, bac2$days, multiply = 10000, chart = 'up')


# Patient harm data ------------------------------------------------------------

# Bypass data ------------------------------------------------------------------
