## slice stepping out and shrinkage algorithm of Neal 2003
slice_stepping_out <- function (x, log_target, w, max = Inf) {
  nEvaluations <- 0
  f <- function(x) {
    nEvaluations <<- nEvaluations + 1
    log_target(x)
  }
  fx <- f(x)
  y <- log(runif(1)) + fx
  L <- x - runif(1) * w
  R <- L + w
  if (!is.finite(max)) {
    while (y < f(L)) L <- L - w
    while (y < f(R)) R <- R + w
  }
  else if (max > 0) {
    J <- floor(runif(1) * max)
    K <- max - 1 - J
    while (J > 0 && y < f(L)) {
      L <- L - w
      J <- J - 1
    }
    while (K > 0 && y < f(R)) {
      R <- R + w
      K <- K - 1
    }
  }
  repeat {
    x1 <- L + runif(1) * (R - L)
    if (y < f(x1)) {
      return(list(x = x1, nEvaluations = nEvaluations))
    }
    if (x1 < x) 
      L <- x1
    else R <- x1
  }
}


## conjugate update functions

upd_nnmkv <- function(x, v, m0, v0) {
  ## x ~ N(m, v) # v known
  ## m ~ N(m0, v0)
  ## update for m
  
  n <- length(x)
  sumx <- sum(x)
  v1 <- 1.0 / ((1.0 / v0) + (n / v))
  m1 <- v1 * ((m0 / v0) + (sumx / v))
  rnorm(1, mean = m1, sd = sqrt(v1))
}

upd_nigvkm <- function(x, m, a0, b0) {
  ## x ~ N(m, v) # m known
  ## v ~ IG(a0/2, b0/2)
  ## update for v
  
  n <- length(x)
  ssx <- sum((x - m)^2)
  a1 <- a0 + n
  b1 <- b0 + ssx
  1.0 / rgamma(1, shape = a1, rate = b1)
}


## specific update functions

upd_sig2 <- function(sig2_old, x, m, max_sig, wslice) {
  
  if (length(x) == 0) {
 
    sig_out <- runif(1, min = 0.0, max = max_sig) # draw from prior if there are no units assigned to the current cluster

  } else {
    
    logtarget <- function(sig) {
      if (sig < 0.0 || sig > max_sig) {
        out <- -Inf
      } else {
        out <- sum(dnorm(x, mean = m, sd = sig, log = TRUE)) # likelihood only here since prior is uniform (density is const)
      }
      out
    }
    
    ## using a slice sampler here, but any hybrid step could work
    tmp <- slice_stepping_out(x = sqrt(sig2_old), log_target = logtarget, 
                              w = wslice)
    
    sig_out <- tmp$x
    
  }
  
  sig_out^2
}

upd_Zi <- function(yi, K, w, mus, sig2s) {

  logdens_i <- dnorm(yi, mean = mus, sd = sqrt(sig2s), log = TRUE) # vectorized across all K components
  lw <- log(w)
  
  lp <- lw + logdens_i # this is a vector of length K
  lp <- lp - max(lp) # computational trick to avoid overflow (adding a const on log scale is equivalent to multiplying by const on original scale)
  p <- exp(lp) # go from log to probability scale
  p <- p / sum(p) # normalize (not necessary for R's sample function)
  
  sample.int(K, size = 1, prob = p)
}

upd_w <- function(Z, K, alpha) {
  tab <- tabulate(Z, nbins = K) # vector of counts (length K)
  alpha1 <- alpha + tab
  
  w0 <- rgamma(K, shape = alpha1, rate = 1.0)
  w0 / sum(w0) # draw from a Dirichlet
}
