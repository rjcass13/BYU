#################
# Q1
#################
x <- seq(-.999, .999, .001)

name <- c('fx', 'a', 'b', 'c')
mu <- c(0, 0, 3, 3)
sigma <- c(0, 1, 1, 2)
colors <- c('black', 'red', 'green', 'blue')

values <- cbind(name, mu, sigma, colors)

fx <- function(x) {
  63/4 * (x^6 - x^8)
}

for (i in 1:4) {
  name_i <- values[i, ][1]
  mu_i <- as.numeric(values[i, ][2])
  sigma_i <- as.numeric(values[i, ][3])
  color_i <- values[i, ][4]

  if (sigma_i == 0) {
    x_i <- x - mu_i
    y <- sapply(x, fx)
  } else {
    x_i <- (x - mu_i)/sigma_i
    y <- 1/sigma_i * sapply(x, fx)
  }

  print(max(y))

  if (i == 1) {
    plot(x_i, y, main = 'HW 5.1', col = color_i, ylim = c(0, 2), xlim = c(-4, 1), type = 'l', lwd = 3, xlab = 'X', ylab = 'f(x)')
  } else {
    lines(x_i, y, col = color_i)
  }

  legend("topleft", legend = name, col = colors, lty = 1)
}


#################
# Q3
#################
M = c(1000, 10000, 100000)

for (m in M) {
  theta <- seq(0, pi/2, length.out = m)
  gx <- numeric(m)
  gx_1 <- numeric(m)
  gx_2 <- numeric(m)
  cutoff = atan(2)

  for (i in 1:m) {
    ang <- theta[i]
    if (ang <= cutoff) {
      gx[i] <- 1 / (2 * cos(ang))
      gx_1[i] <- 1 / (2 * cos(ang))
      gx_2[i] <- NA
    } 
    else {
      gx[i] <- 1 / cos((pi/2) - ang)
      gx_2[i] <- 1 / cos((pi/2) - ang)
      gx_1[i] <- NA
    }
  }
  cat("M:", m, "Mean:", mean(gx), "SE:", round(sd(gx)/sqrt(m), 4), "\n")
  cat("GX1", "Mean:", mean(gx_1, na.rm = TRUE), "SE:", round(sd(gx_1)/sqrt(m), 4), "\n")
  cat("GX2", "Mean:", mean(gx_2, na.rm = TRUE), "SE:", round(sd(gx_2)/sqrt(m), 4), "\n")
}

#################
# Q4
#################
M <- 10000
x_a <- seq(0, 1, length.out = M)
fx_a <- 5 / (atan(5) * (1 + 25*x_a^2))
x_b <- abs(rcauchy(M, location = 0, scale = 1/5))
x_b <- x_cauchy/max(x_cauchy)
fx_b <- 5 / (atan(5) * (1 + 25*x_b^2))
w_b <- 1/(2*dcauchy(x_b, 0, 1/5))
x_c <- rbeta(M, 2, 1)
fx_c <- 5 / (atan(5) * (1 + 25*x_c^2))
w_c <- 1/dbeta(x_c, 2, 1)
x_d <- rbeta(M, 1, 2)
fx_d <- 5 / (atan(5) * (1 + 25*x_d^2))
w_d <- 1/dbeta(x_d, 1, 2)
cat("Unif - E(X):", mean(fx_a), "VAR:", var(fx_a), "\n")
cat("Cauchy - E(X):", mean(fx_b * w_b), "VAR:", var(fx_b), "\n")
cat("Beta(2,1) - E(X):", mean(fx_c * w_c), "VAR:", var(fx_c), "\n")
cat("Beta(1,2) - E(X):", mean(fx_d * w_d), "VAR:", var(fx_d), "\n")