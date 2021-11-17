load_all("~/research_code/glmeiv/")
# checking the variance estimate
m_beta <- 1
g_beta <- 0.8
c <- 0.7
pi <- 0.4
n <- 10000
B <- 10000

# variance formula
compute_variance <- function(c, g_beta, pi, m_beta) {
  z <- 1 - stats::pnorm(c)
  w <- 1 - stats::pnorm(c - g_beta)
  mean_phat <- z * (1 - pi) + w * pi
  l <- compute_mean(c, g_beta, pi, m_beta)
  var <- (m_beta^2 * w * pi + mean_phat - 2 * l * m_beta * w * pi + l^2 * mean_phat)/(mean_phat^2)
  return(var)
}

# mean formula
compute_mean <- function(c, g_beta, pi, m_beta) {
  z <- 1 - stats::pnorm(c)
  w <- 1 - stats::pnorm(c - g_beta)
  mean_phat <- z * (1 - pi) + w * pi
  (m_beta * w * pi)/(z * (1 - pi) + w * pi)
}

compute_var_phat_p <- function(c, g_beta, pi, n) {
  w <- 1 - stats::pnorm(c - g_beta)
  n * w * pi * (1 - w * pi)
}

# generate data
generate_data <- function(B, n, m_beta, g_beta, pi) {
  ep_mat <- matrix(data = rnorm(n = n * B), nrow = n, ncol = B)
  tau_mat <- matrix(data = rnorm(n = n * B), nrow = n, ncol = B)
  p <- matrix(data = rbinom(n = n * B, size = 1, prob = pi),
              nrow = n, ncol = B)
  m <- sapply(X = seq(1, B), FUN = function(i) m_beta * p[,i] + ep_mat[,i])
  g <- sapply(X = seq(1, B), FUN = function(i) g_beta * p[,i] + tau_mat[,i])
  return(list(m = m, g = g, p = p))
}

# estimator
compute_beta_hat <- function(dat, c) {
  g_mat <- dat$g
  p_hat_mat <- g_mat >= c
  p_mat <- dat$p
  m_mat <- dat$m
  
  beta_hat <- sapply(seq(1, B), function(i) {
    p_hat <- p_hat_mat[,i]
    m <- m_mat[,i]
    sum(p_hat * m)/sum(p_hat)
  })
  
  return(list(beta_hat = beta_hat, p_hat_mat = p_hat_mat))
}

# test
dat <- generate_data(B = B, n = n, m_beta = m_beta, g_beta = g_beta, pi = pi)
beta_hats <- compute_beta_hat(dat = dat, c = c)

# empirical
mean(beta_hats$beta_hat) # mean
var(sqrt(n) * beta_hats$beta_hat) # variance

# theoretical
compute_mean(c = c, g_beta = g_beta, pi = pi, m_beta = m_beta)
compute_variance(c = c, g_beta = g_beta, pi = pi, m_beta = m_beta)

#################
# With intercepts
#################
# checking the variance estimate
m_beta <- 1
g_beta <- 4
m_beta_0 <- 0.5
g_beta_0 <- 0.0
c <- 2
pi <- 0.4
n <- 20000
B <- 10000

generate_data <- function(B, n, m_beta, g_beta, pi) {
  ep_mat <- matrix(data = rnorm(n = n * B), nrow = n, ncol = B)
  tau_mat <- matrix(data = rnorm(n = n * B), nrow = n, ncol = B)
  p <- matrix(data = rbinom(n = n * B, size = 1, prob = pi),
              nrow = n, ncol = B)
  m <- sapply(X = seq(1, B), FUN = function(i) m_beta_0 + m_beta * p[,i] + ep_mat[,i])
  g <- sapply(X = seq(1, B), FUN = function(i) g_beta_0 + g_beta * p[,i] + tau_mat[,i])
  return(list(m = m, g = g, p = p))
}

# estimator
compute_beta_hat <- function(dat, c) {
  g_mat <- dat$g
  p_hat_mat <- g_mat >= c
  p_mat <- dat$p
  m_mat <- dat$m
  beta_hat <- sapply(seq(1, B), function(i) {
    p_hat <- as.integer(p_hat_mat[,i])
    m <- m_mat[,i]
    fit <- lm(formula = m ~ p_hat)
    coef(fit)[["p_hat"]]
  })
  return(list(beta_hat = beta_hat, p_hat_mat = p_hat_mat))
}

dat <- generate_data(B = B, n = n, m_beta = m_beta, g_beta = g_beta, pi = pi)
res <- compute_beta_hat(dat, c)
mean(res$beta_hat)
l <- glmeiv::get_tresholding_estimator_bias(m_perturbation = m_beta, g_perturbation = g_beta, pi = pi, return_bias = FALSE)
normed_beta_hat <- 1/sqrt(n) * (res$beta_hat - l)

# compute Tis
# Ts <- sapply(X = seq(1, B), FUN = function(i) {
#  phat <- as.integer(res$p_hat_mat[,i])
#  m <- dat$m[,i]
#  (1/sqrt(n)) * sum((phat - mean(phat)) * (m - l * phat))
# })