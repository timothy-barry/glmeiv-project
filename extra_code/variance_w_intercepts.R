load_all("~/research_code/glmeiv/")
# checking the variance estimate
m_beta <- 0.5
m_beta_0 <- 1
pi <- 0.35
n <- 10000
B <- 10000

# generate data
generate_data <- function(B, n, m_beta_0, m_beta, pi) {
  ep_mat <- matrix(data = rnorm(n = n * B), nrow = n, ncol = B)
  p <- matrix(data = rbinom(n = n * B, size = 1, prob = pi), nrow = n, ncol = B)
  y <- sapply(X = seq(1, B), FUN = function(i) m_beta_0 + m_beta * p[,i] + ep_mat[,i])
  return(list(y = y, p = p))
}
dat <- generate_data(B = B, n = n, m_beta_0 = m_beta_0, m_beta = m_beta, pi = pi)

compute_estimator <- function(dat) {
  y <- dat$y
  x <- dat$p
  B <- ncol(y)
  n <- nrow(y)
  beta_hats <- sapply(X = seq(1, B), FUN = function(i) {
    curr_y <- y[,i]
    curr_x <- x[,i]
    diff <- curr_x - mean(curr_x)
    beta_hat <- (sum(diff * curr_y))/(sum(diff^2))
  })
  return(beta_hats)
}

beta_hats <- compute_estimator(dat)
var(sqrt(n) * beta_hats)
1/(pi * (1 - pi))
