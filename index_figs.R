library(qicharts2)
library(tidyverse)

bac   <- read_csv('data/bacteremia.csv', comment = '#')
hba1c <- read_csv('data/diabetes_hba1c.csv', comment = '#')

# Function to plot control limits from the I'-chart on top of Shewhart chart
compplot <- function(x, y, n = NULL, chart, ...) {
  p1 <- qic(x, y, n, chart = chart, ...)
  p2 <- qic(x, y, n, chart = 'ip', ...)
  
  p1 +
    geom_line(aes(y = lcl),
              data     = p2$data,
              linetype = 'dashed',
              colour   = 'tomato') +
    geom_line(aes(y = ucl),
              data     = p2$data,
              linetype = 'dashed',
              colour   = 'tomato')
}

# plot dimensions
w = 9
h = 5

pdf('figs/fig01.pdf', width = w, height = h)
qic(month, avg_hba1c,
    data = hba1c,
    chart = 'i',
    title = NULL,
    ylab = 'mmol / mol',
    xlab = 'Month')
dev.off()

pdf('figs/fig02.pdf', width = w, height = h)
qic(month, avg_hba1c * n, n,
    data = hba1c,
    chart = 'ip',
    title = NULL,
    ylab = 'mmol / mol',
    xlab = 'Month')
dev.off()

pdf('figs/fig03.pdf', width = w, height = h)
compplot(bac$month, bac$deaths, bac$patients,
         chart = 'p',
         title = NULL,
         xlab = 'Month')
dev.off()


pdf('figs/fig04.pdf', width = w, height = h)
compplot(bac$month, bac$deaths, bac$patients,
         chart = 'pp',
         title = NULL,
         xlab = 'Month')
dev.off()


pdf('figs/fig05.pdf', width = w, height = h)
compplot(bac$month, bac$ha_infections, bac$risk_days,
         chart = 'up',
         multiply = 10000,
         title = NULL,
         ylab = 'Infections per 10,000 risk days',
         xlab = 'Month')
dev.off()


pdf('figs/fig06.pdf', width = w, height = h)
set.seed(1)                   # fix random number generator
x <- 1:24                     # subgroups
y <- rnorm(24, 20, 3)         # numerators
n <- runif(24, 80, 120)       # denominators

p1 <- compplot(x, y, chart = 'i', title = NULL)
p2 <- compplot(x, y * n, n, chart = 'i', title = NULL)

ylim <- range(layer_scales(p1)$y$range$range,
              layer_scales(p2)$y$range$range)
ylim <- scale_y_continuous(limits = ylim)
p1 + ylim
dev.off()


pdf('figs/fig07.pdf', width = w, height = h)
p2 + ylim
dev.off()


pdf('figs/fig08.pdf', width = w, height = h)
set.seed(1)
x <- rep(1:24, round(runif(24, 10, 20)))
y <- rnorm(length(x), 9)
p1 <- compplot(x, y, chart = 'xbar', title = NULL)

set.seed(6)
x <- rep(1:24, round(runif(24, 10, 20)))
y <- rnorm(length(x), 9)
p2 <- compplot(x, y, chart = 'xbar', title = NULL)

set.seed(8)
x <- rep(1:24, round(runif(24, 10, 20)))
y <- rnorm(length(x), 9)
p3 <- compplot(x, y, chart = 'xbar', title = NULL)

ylim <- range(layer_scales(p1)$y$range$range,
              layer_scales(p2)$y$range$range,
              layer_scales(p3)$y$range$range)
ylim <- scale_y_continuous(limits = ylim)

p1 + ylim
dev.off()


pdf('figs/fig09.pdf', width = w, height = h)
p2 + ylim
dev.off()


pdf('figs/fig10.pdf', width = w, height = h)
p3 + ylim
dev.off()


pdf('figs/fig11.pdf', width = w, height = h)
set.seed(1)
x <- 1:24
n <- round(runif(24, 100, 120))
y <- rbinom(24, n, 0.1)

p1 <- compplot(x, y, n, 'p', title = NULL)
p2 <- compplot(x, y, n, 'pp', title = NULL)


ylim <- range(layer_scales(p1)$y$range$range,
              layer_scales(p2)$y$range$range)
ylim <- scale_y_continuous(limits = ylim)

p1 + ylim
dev.off()


pdf('figs/fig12.pdf', width = w, height = h)
p2 + ylim
dev.off()


pdf('figs/fig13.pdf', width = w, height = h)
set.seed(2)
x <- 1:24
n <- (runif(24, 90, 110))
y <- rpois(24, 25)

p1 <- compplot(x, y, n, chart = 'u', title = NULL)
p2 <- compplot(x, y, n, chart = 'up', title = NULL)

ylim <- range(layer_scales(p1)$y$range$range,
              layer_scales(p2)$y$range$range)
ylim <- scale_y_continuous(limits = ylim)

p1 + ylim
dev.off()


pdf('figs/fig14.pdf', width = w, height = h)
p2 + ylim
dev.off()


