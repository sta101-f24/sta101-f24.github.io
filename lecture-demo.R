# ==============================================================================
# packages
# ==============================================================================

library(tidyverse)
library(tidymodels)
library(openintro)

# ==============================================================================
# raw distribution vs mean
# ==============================================================================

mu = 3
sigma = 2
n = 30

normTail(m = mu, s = sigma, ylim = c(0, 1.5))
normTail(m = mu, s = sigma / sqrt(n), add = TRUE)

# ==============================================================================
# raw distribution vs mean
# ==============================================================================

mu = 3
sigma = 2
n = 30

normTail(m = 0, s = sigma, xlim = c(-6*sigma, 6*sigma + mu))


# ==============================================================================
# raw distribution vs mean
# ==============================================================================

x_grid = seq(-6, 6, length.out = 500)
d_val = dt(x_grid, df = 4)

par(mar = c(3, 0.5, 0.5, 0.5))
plot(x_grid, d_val, type = "l", bty = "n", yaxt = "n", ylab = "",
     xlab = "test statistic")

abline(v = -3, lty = 2, col = "red", lwd = 3)

polygon()






