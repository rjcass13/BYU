n <- 100
theta <- seq(-9, 9, by = 1)/10
alpha <- .05
# Assuming equal sided
lower <- qchisq(alpha/2, 1)
upper <- qchisq(1-alpha/2,1)
n_sim <- 500

power <- numeric(length(theta))
for (i in 1:length(theta)) {
  p1 <- (1-theta[[i]])^2/4
  p2 <- (1-theta[[i]]^2)/2
  p3 <- (1+theta[[i]])^2/4

  val <- numeric(length(n_sim))
  for (j in 1:n_sim) {
    dt <- rmultinom(n, size = 3, prob = c(p1,p2,p3))
    Y1 <- sum(dt[1,])
    Y2 <- sum(dt[2,])
    Y3 <- sum(dt[3,])
    theta_hat <- (Y3 - Y1)/n
    val[[j]] <- (1-theta_hat)^(2*Y1)*(1-theta_hat^2)^(Y2)*(1+theta_hat)^(2*Y3)
  }

  power[[i]] <- mean(val > upper | val < lower)
}

plot(theta, power)
