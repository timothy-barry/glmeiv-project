load_all("~/research_code/glmeiv/")
# checking the variance estimate
m_beta_0 <- 0
m_beta_1 <- 1
g_beta_0 <- 0
g_beta_1 <- 0.00
c <- 0.1 # 0.0 * g_beta_1
pi <- 0.5
n <- 10000

###########
# EIV model
###########
run_sim_eiv <- function() {
  p <- rbinom(n = n, size = 1, pi)
  ep_1 <- rnorm(n = n)
  ep_2 <- rnorm(n = n)
  # generate m, g
  m <- m_beta_0 + m_beta_1 * p + ep_1
  g <- g_beta_0 + g_beta_1 * p + ep_2
  # get p_hat
  p_hat <- as.integer(g >= c)
  # fit linear model
  fit <- lm(m ~ p_hat)
  coef(fit)[[2]]
}

beta_1_hats <- replicate(n = 5000, expr = run_sim_eiv(), simplify = TRUE)
# empirical var
var(sqrt(n) * beta_1_hats)

########################
# simple (non-EIV) model
########################
if (FALSE) {
run_sim <- function() {
  x <- rbinom(n = n, size = 1, pi)
  ep <- rnorm(n = n)
  y <- m_beta_0 + m_beta_1 * x + ep
  fit <- lm(y ~ x)
  coef(fit)[["x"]]
}

beta_1_hats <- replicate(n = 25000, expr = run_sim(), simplify = TRUE)
# empirical variance
var(sqrt(n) * beta_1_hats)
# theoretical variance
1/(pi* (1 - pi))
}

# theoretical
z <- 1 - stats::pnorm(c)
w <- 1 - stats::pnorm(c - g_beta_1)
mean_phat <- z * (1 - pi) + w * pi
var <- (1 + m_beta_1^2 * pi * (1 - pi))/(mean_phat * (1 - mean_phat))
